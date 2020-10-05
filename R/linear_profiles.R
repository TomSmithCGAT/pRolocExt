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
#' PlotFeatureProfiles(head(lopitdcU2OS2018), c(10,20))
PlotFeatureProfiles <- function(obj, replicate_boundaries=NULL){

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



