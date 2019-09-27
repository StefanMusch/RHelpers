#' @title Survey Tools - Extracting The Object Label
#' @description Often, SPSS files will come with a label hidden in the
#'  `attr(which = "label")` parameter. This function extracts only the relevant
#'   part of the label that is often needed (e.g. without the Q1 part)
#' 
#' @return Character string with the label output
#' 
#' @param x The labelled object to extract the label from.
#' @param split Logical. Whether the label needs to be split based on a seperator. 
#' @param sep The seperator character(s) to split the label on if split = TRUE.
#'  Should include spaces if applicable. 
#' @param position Position of the label that you need. E.g. 
#' if the label is: "Q1 - What is the label?", you will need the 2nd part
#'  after the split
#' @examples 
#' 
#' x <- "Labelled vector"
#' attr(x, which = "label") <- "Q1 - What is the label?"
#' 
#' srv_extract_label(x, split = TRUE, sep = " - ", position = 2)
#' 
#' @export
srv_extract_label <- function(x, split = TRUE, sep = " - ", position = 2) {
    
    if(split){
        lbl <- attr(x, which = "label") %>%
            strsplit(., split = sep, fixed = TRUE)
        
        
        if(length(lbl) != 0) lbl[[1]][position]
        else ""
    }
    else{
        lbl <- attr(x, which = "label")
        
        if(length(lbl) != 0) lbl
        else ""
    }
    
}

globalVariables(c('.'))

