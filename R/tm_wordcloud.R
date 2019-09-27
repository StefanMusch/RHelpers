#'@importFrom tidytext unnest_tokens
#'@importFrom ggwordcloud ggwordcloud
#'
#'@title Create a g6tr themed wordcloud with the option to remove stopwords
#'
#'@return A ggplot object
#'
#'@param df a dataframe object
#'@param var Variable name (character vector) to create a wordcloud of
#'@param remove_stopwords Removes stopwords from the wordcloud if TRUE. 
#'based on the stop_words package.
#'@param clean_text Clean words if TRUE
#'@param ... further arguments passed to \link{ggwordcloud}
#'
#'@examples
#'
#'library(janeaustenr)
#'library(ggplot2)
#'library(dplyr)
#' df <- 
#'   austen_books() %>%
#'   filter(book == "Sense & Sensibility")
#'   
#'tm_wordcloud(
#'    df = df,
#'    var = "text", 
#'    remove_stopword = TRUE,
#'    clean_text = TRUE
#' )
#'
#'@export
tm_wordcloud <- function(df,
                         var,
                         remove_stopwords = TRUE,
                         clean_text = TRUE,
                         colors = c("#36c8ef", "#50bd90", "#ef5d36", "#ffcf60"),
                         ...){
    
    # Input Checks
    stopifnot(class(df) %in% c("data.frame", "tbl_df", "tbl", "data.table"))
    stopifnot(is.logical(remove_stopwords))
    stopifnot(is.logical(clean_text))
    stopifnot(is.character(var))
    
    ds_stopwords <- get('ds_stopwords', envir = asNamespace('g6tr'))
    
    words <- df %>%
        select(!!var) %>%
        tidytext::unnest_tokens(input = !!var,
                                output = "word", to_lower = T) %>%
        {if(clean_text) 
            mutate_at(., vars("word"), g6tr::tm_clean_text, ...)
            else .} %>%
        {if(remove_stopwords) anti_join(., ds_stopwords) else .} %>%
        group_by(word) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
    
    ggwordcloud::ggwordcloud(words = words$word,
                             shape = "diamond",
                             freq = words$count,
                             min.freq = 10,
                             max.words = 100,
                             colors = colors,
                             random.color = F,
                             ...
    ) +
        ggplot2::theme(plot.title = element_text(size = 16),
                       plot.margin=grid::unit(c(0,0,0,0), "mm"))
}
globalVariables('word')
