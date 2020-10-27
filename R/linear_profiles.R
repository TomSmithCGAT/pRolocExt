#' Return a map of sample to replicate based on MSnSet column names and
#' replicate boundaries
#'
#' @param obj `MSnSet`
#' @param replicate_boundaries `numeric vector` defining the boundaries between
#' replicates. E.g if 3 replicates with 10 samples each, boundaries are `c(10,20)`
get_sample_2_rep <- function(obj, replicate_boundaries){
  replicate <- rep(1:(length(replicate_boundaries)+1),
                   diff(c(0, replicate_boundaries, ncol(obj))))

  sample2rep <- data.frame('sample'=colnames(obj), 'replicate'=replicate)

  sample2rep
}

#' Plots linear profiles for each feature
#'
#' @description Generates line plots where each feature is plotted separately.
#' If `replicate_boundaries` are supplied, the plot is facetted by replicate.
#' Assumes order of samples in input MSnSet is the correct order for plotting
#'
#' @param obj `MSnSet` containing features to plot
#' @param replicate_boundaries `numeric vector` defining the boundaries between
#' replicates. E.g if 3 replicates with 10 samples each, boundaries are `c(10,20)`
#'
#' @import ggplot2
#' @export
#' @examples
#' library(pRolocdata)
#' data(lopitdcU2OS2018)
#' Plot_feature_profiles(head(lopitdcU2OS2018), c(10,20))
Plot_feature_profiles <- function(obj, replicate_boundaries=NULL){

  exprs_df <- data.frame(exprs(obj)) %>%
    tibble::rownames_to_column('feature') %>%
    tidyr::pivot_longer(-feature, names_to='sample', values_to='abundance') %>%
    dplyr::mutate(sample=factor(sample, levels=colnames(obj)))

  if(!missing(replicate_boundaries)){
    # add the replicate information using the boundaries
    sample2rep <- get_sample_2_rep(obj, replicate_boundaries)
    exprs_df <- exprs_df %>% merge(sample2rep, by='sample')
  }

  p <- ggplot(exprs_df, aes(sample, abundance, colour=feature, group=feature)) +
    camprotR::theme_camprot(base_size=15) +
    geom_line() +
    theme(aspect.ratio=1,
          axis.text.x=element_text(angle=45, vjust=1, hjust=1, size=10))  +
    xlab("Sample") +
    ylab("Abundance")

  if(!missing(replicate_boundaries)){
    p <- p +
      facet_wrap(.~replicate, scales='free_x') +
      theme(panel.spacing.x=unit(0.1, "lines"),
            strip.background = element_rect(colour="white", fill="white"),
            panel.border = element_rect(colour = "black"))
  }

  if(nrow(obj)<=12){
    colours <- camprotR::get_cat_palette(nrow(obj))
    p <- p + scale_colour_manual(values=colours, name="", na.value="grey")
  } else{
    p <- p + scale_colour_discrete(name="", na.value="grey")
  }

  return(p)
}

#' Plots linear profiles for each marker set
#'
#' @description Generates line plots for each marker. Each marker set is plotted
#' in a separate facet. Assumes order of samples in input MSnSet is the correct
#' order for plotting
#'
#' @param obj `MSnSet` containing features to plot
#' @param fcol `character` defining the markers column
#' @param plot_all `logical` should all individual features be plotted
#' @param alpha `numeric` alpha value(s) for marker sets
#' @param facet_by `character` Split plot into facets for each marker sets
#' (`markers`), each feature (`feature`), or no facetting (`none`)
#' @param group_by Grouping factor for lines. Variable must be specified in pData for obj.
#' This parameter can be used to e.g ensure replicates are plotted as separate lines
#'
#' @import ggplot2
#' @export
#' @examples
#' library(pRolocdata)
#' data(lopitdcU2OS2018)
#' obj <- lopitdcU2OS2018[,1:10]
#' obj <- obj[fData(obj)$markers != 'unknown',]
#' alpha <- 10*classWeights(obj)
#' plot_marker_profiles(obj, facet_by='markers', alpha=alpha)
plot_marker_profiles <- function(obj,
                                 fcol='markers',
                                 plot_all=T,
                                 alpha=1,
                                 facet_by='none',
                                 group_by=NULL){

  allowed_facet_by_values <- c('markers', 'features', 'none')

  if(!facet_by %in% allowed_facet_by_values){
    stop(sprintf('facet_by must be one of: %s',
                 paste(allowed_facet_by_values, collapse=', ')))
  }

  if(!plot_all & facet_by!='none'){
    stop(sprintf('incompatible options, plot_all=FALSE and facet_by=%s', facet_by))
  }

  exprs_df <- exprs(obj)
  f_df <- fData(obj) %>% dplyr::select(!!sym(fcol))

  exprs_df <- exprs_df %>%
    data.frame()

  new_colnames2old_colnames <- data.frame('new'=colnames(exprs_df),
                                          'old'=colnames(exprs(obj)))

  exprs_df <- exprs_df %>%
    tibble::rownames_to_column('id') %>%
    tidyr::pivot_longer(-id, names_to='sample', values_to='abundance') %>%
    merge(new_colnames2old_colnames, by.x='sample', by.y='new') %>%
    mutate(sample=factor(camprotR::remove_x(old), levels=colnames(obj)))

  exprs_df <- merge(exprs_df, f_df, by.x="id", by.y="row.names") %>%
    merge(pData(obj), by.x='sample', by.y='row.names')

  p <- ggplot(exprs_df, aes(sample, abundance, colour=!!sym(fcol))) +
    theme_bw(base_size=10) +
    theme(aspect.ratio=1,
          axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))  +
    xlab("") +
    ylab("Abundance")

  if(length(alpha>1)){
    p <- p +
      aes(alpha=!!sym(fcol))
  }

  if(plot_all){
    if(length(alpha)==1){
      p <- p +
        aes(group=id) +
        geom_line(alpha=alpha)
    } else{
      p <- p +
        aes(group=id) +
        geom_line() +
        aes(alpha=!!sym(fcol)) +
        scale_colour_discrete(name='')
    }
    if(!missing(group_by)){
      p <- p + aes(group=interaction(!!sym(group_by), id))
    }
  } else{
    if(length(alpha)==1){
      p <- p +
        stat_summary(aes(group=!!sym(fcol)),
                     geom="line", fun.y=mean, size=1, alpha=alpha)
    } else{
      p <- p +
        stat_summary(aes(group=!!sym(fcol)),
                     geom="line", fun.y=mean, size=1) +
        aes(alpha=!!sym(fcol))
    }
    if(!missing(group_by)){
      p <- p + aes(group=interaction(!!sym(group_by), !!sym(fcol)))
    }
  }
  if(length(alpha)>1){
    p <- p + scale_alpha_manual(values=alpha, guide=FALSE)
  }

  if(facet_by=='features'){
    p <- p +
      facet_wrap(~id)
  } else if(facet_by=='markers'){
    p <- p +
      facet_wrap(vars(!!sym(fcol)))
  }

  p <- p +
    guides(colour=guide_legend(override.aes = list(alpha = 1, size=1)))

  return(p)
}
