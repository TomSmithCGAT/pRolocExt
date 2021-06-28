#' Make 2D projections
#'
#' @description Generates 2D projections using either PCA or t-SNE
#' 
#' @param obj `MSnSet` containing features to plot
#' @param method `character` describing `pRoloc::plot2D` method to use
#' @param fcol `character` Feature meta-data label (fData column name) defining
#' the groups to be differentiated using different colours. Default is markers
#' @param dims 	A `numeric` of length 2 defining the dimensions to be plotted.
#' Default is c(1, 2)
#' @param ... additional arguments to be passed to `pRoloc::plot2D`
#' @import pRoloc
#' @export
#' @examples
#' library(pRolocdata)
#' data(lopitdcU2OS2018)
#' make_proj(lopitdcU2OS2018)
make_proj <- function(obj, method="PCA", fcol="markers", dims=c(1,2), ...){

  if(!method %in% c('t-SNE', 'PCA')){
    warning('make_proj function has only been testing with t-SNE and PCA')
  }
  
  PCA_matrix <- plot2D(obj, method=method, fcol=fcol, plot=FALSE, dims=dims, ...)
  
  PCA_df <- PCA_matrix %>% merge(data.frame(fData(obj)), by="row.names") %>%
    tibble::column_to_rownames('Row.names')
  
  PCA_df$markers <- PCA_df[[fcol]]
  PCA_df$unknown <- PCA_df$markers=="unknown"
  
  PCA_df <- PCA_df[order(-PCA_df$unknown),]
  
  return(PCA_df)
}

?pRoloc::plot2D
