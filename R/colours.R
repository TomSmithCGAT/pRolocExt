#getClassColours <- function(){
#  cols = getStockcol()[c(1:5,12,7:11,13:20)]
#  return(cols)
#  }

#' Map marker class names to colours
#'
#' @description To ensure standardised marker colours, we define a
#' set of colours for the 'normal' marker sets.
#'
#' @return `named list` with colours for each possible expected marker class
#' @export
get_class_2_colours <- function(){

  class_2_colour <- list("CYTOSOL"="#E41A1C",
                         "Cytosol"="#E41A1C",
                         "PROTEASOME"="#984EA3",
                         "PROTEASOME 19S"="#704da2",
                         "PROTEASOME 20S"="#a24d70",
                         "RIBOSOME"="#9999FF",
                         "mRNP"="#9999FF",
                         "RIBOSOME 40S"="#9999FF",
                         "RIBOSOME 60S"="#000099",
                         "ER"="#FF7F00",
                         "er"="#FF7F00",
                         "GOLGI"="#377EB8",
                         "GOLGI/LYSOSOME"="#377EB8",
                         "Golgi/Lysosome"="#377EB8",
                         "GA"="#377EB8",
                         "LYSOSOME"="#F781BF",
                         "PM"="#00CED1",
                         "PEROXISOME"="#A65628",
                         "MITOCHONDRION"="#FFD700",
                         "MITOCHONDRIA"="#FFD700",
                         "Mitochondria"="#FFD700",
                         "Mitochondrion"="#FFD700",
                         "NUCLEUS/CHROMATIN"="#9ACD32",
                         "NUCLEUS-CHROMATIN"="#238B45",
                         "NUCLEUS"="#9ACD32",
                         "Nuclear"="#9ACD32",
                         "CHROMATIN"="#238B45",
                         "unknown"="grey50",
                         "missing"="grey50")

  class_2_colour
}

#' Get colours for a set of marker classes
#'
#' @description To ensure standardised marker colours, we define a
#' set of colours for the 'normal' marker sets.
#'
#' @param marker_classes `vector` marker class names
#' @param class_2_colour `named list` mapping marker class names to hex colour
#'
#' @return `vector` of hex colour codes
#' @export
get_colours <- function(marker_classes, class_2_colour=get_class_2_colours()){

  missing_classes <- setdiff(marker_classes, names(class_2_colour))

  if(length(missing_classes)>0){
    stop(sprintf(paste0('not all marker classes have assigned colours. ',
                        'These classes are missing: %s'),
                        paste0(missing_classes, collapse=', ')))
  }

  unlist(class_2_colour[names(class_2_colour) %in% marker_classes][marker_classes], use.names = FALSE)
}



