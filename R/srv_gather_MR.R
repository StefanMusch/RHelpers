#' @title Survey Tools - Gathering Multiple-Response Variables
#' @description Gathering Multiple-Response variables into its own tidy data
#' frame to easily work with the structure
#' 
#' @return A tidy gathered data frame that you can assign
#' 
#' @param df Dataframe input
#' @param id_var Variable name or number of the ID
#' @param var_prefix The prefix that describes all of the MR variables you
#' wish to gather. Passed into \link[tidyselect]{select_helpers}.
#' @param other Often, a MR variable also has a column related to the "other" 
#' response. You can supply a string here to exclude variables that end 
#' with that particular string. (e.g. "s_majorr_eo). Passed into 
#' \link[tidyselect]{select_helpers}.
#' @param replace_colnames Whether to replace the column names with the labels. 
#' This will automatically change the categories to a more expressive name if
#' TRUE.
#' @param ... Further arguments passed to \link{srv_replace_colnames} and
#' \link{srv_extract_label}. 
#' 
#' \itemize{
#' \item  \code{exc - }  Column names or numbers to exclude.
#' \item  \code{split - }  Whether the label needs to be split based on a seperator. 
#' \item  \code{sep - }The seperator character(s) to split the
#'  label on if split = TRUERUE. Should include spaces if applicable. 
#' \item \code{position - } Position of the label that you need. 
#'  E.g. if the label is: "Q1 - What is the label?", you will need the 2nd part
#'  after the split
#' }

#' @examples 
#' 
#' library(dplyr)
#' library(purrr)
#' data(ds_survey_example, package = 'g6tr')
#' 
#' # Single Variable
#' 
#' MR_Variable_replaced <- 
#'  srv_gather_MR(df = ds_survey_example, id_var = "uuid",
#'     var_prefix = "s_major", replace_colnames = TRUE,
#'     sep = " - ", position = 1, exc = "uuid")
#'          
#' MR_Variable <- srv_gather_MR(ds_survey_example, id_var = "uuid",
#'                              var_prefix = "s_major", replace_colnames = FALSE)
#' 
#' # Multiple Variables
#' mr_vars <- c("s_major", "career_learn", "career_attract")
#' 
#' # In one data frame
#' mr_vars_gathered <- 
#'   mr_vars %>%
#'   map_df(function(x) 
#'      srv_gather_MR(var_prefix = x, df = ds_survey_example,
#'         id_var = "uuid",replace_colnames = TRUE,
#'         sep = " - ", position = 1,
#'         exc = "uuid")
#'        )
#' 
#' # In separate data frames
#' mr_vars_gathered_list <- 
#'   mr_vars %>%
#'   map(.x = ., function(x) 
#'      srv_gather_MR(var_prefix = x, df = ds_survey_example,
#'          id_var = "uuid",replace_colnames = TRUE,
#'          sep = " - ", position = 1,
#'          exc = "uuid")
#'         )
#'                                     
#'  names(mr_vars_gathered_list) <- mr_vars 
#'  list2env(mr_vars_gathered_list, envir = .GlobalEnv)
#'  
#' @export
srv_gather_MR <- function(df,
                          id_var = "uuid",
                          var_prefix = "",
                          other = "",
                          replace_colnames = TRUE, ...) {
    
    MR_variable <- df %>%
        select(id_var, starts_with(var_prefix)) %>%
        {if(nchar(other)>0) select(-ends_with(other)) else .} %>%
        {if(replace_colnames) g6tr::srv_replace_colnames(df = ., ...) else .} %>%
        gather(key = category, value = response, -!!id_var)
    
    labels <- df %>%
        select(id_var, starts_with(var_prefix)) %>% 
        {if(nchar(other)>0) select(-ends_with(other)) else .} %>%
        {if(replace_colnames) g6tr::srv_replace_colnames(df = ., ...) else .} %>%
        mutate_if(is.labelled, as_factor, labels = "values") %>% 
        gather(key = category, value = label, -!!id_var, factor_key = TRUE) %>%
        pull(label)
    
    MR_variable <- MR_variable  %>%
        cbind(labels)
    
    return(MR_variable)
}

