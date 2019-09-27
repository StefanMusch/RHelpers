#' @title Clean Text
#' 
#' @param x A character string
#' @param lowercase Makes text lower case
#' @param numbers Removes numbers
#' @param punctuation Removes punctuation
#' @param spaces Removes spaces
#' @param ascii Removes non-ascii strings
#' @param remove_stopwords Removes stopwords
#' 
#' @examples
#' some_text = "Hi!, This is just 1 stupid EXAMPLE to show clean.text :) [123-456-789]"
#' #default application
#' tm_clean_text(some_text)
#' 
#' @export
tm_clean_text <- function(x,
                          lowercase  = TRUE,
                          numbers    = TRUE,
                          punctuation= TRUE,
                          spaces     = TRUE,
                          ascii      = TRUE,
                          remove_stopwords = FALSE){
    # Input Checks
    stopifnot(is.character(x))
    stopifnot(is.logical(lowercase))
    stopifnot(is.logical(numbers))
    stopifnot(is.logical(punctuation))
    stopifnot(is.logical(spaces))
    stopifnot(is.logical(ascii))
    stopifnot(is.logical(remove_stopwords))
    
    # lower case
    if (lowercase)
        x <- tolower(x)
    # remove numbers
    if (numbers)
        x <- gsub("[[:digit:]]", "", x)
    # remove punctuation symbols
    if (punctuation)
        x <- gsub("[[:punct:]]", " ", x)
    # remove extra white spaces
    if (spaces) {
        x <- gsub("[ \t]{2,}", " ", x)
        x <- gsub("^\\s+|\\s+$", "", x)
    }
    
    if(ascii){
        x <- iconv(x, "UTF-8", "ASCII", sub="")
    }
    
    if(remove_stopwords){
        ds_stopwords <- get('ds_stopwords', envir = asNamespace('g6tr'))
        x <- data.frame(word = x) %>% anti_join(ds_stopwords) %>% pull(x)
    }
    
    # return
    x
}
