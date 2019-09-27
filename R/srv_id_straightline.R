#' @importFrom rlang sym
#' 
#' @title Survey Tools - Identify Straightliners
#' @description Respondents often "straightline" through a survey when it's 
#' reward based. Meaning that they either just randomly (hard to spot) answer or
#' that they provide the same answer option to all question (e.g. always "yes"). 
#' This function tries to identify the latter, providing intelligence about
#' the quality of data for you to act on. 
#' 
#' @return A character vector of unique ID's who may be straightliners.
#' 
#' @param df A \href{https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html}{tidy}
#' data frame consisting of a key and value column. Often created from
#'\link{srv_gather_MR}.
#' @param id_var Variable name of 
#' @param category_var Column name of the tidy data frame  that holds the variables to test
#' @param response_var Column name of the tidy data frame that holds the answering options
#' @param cutoff Numeric [0-1] proportion allowed. Defaults to 0. 
#' If 0, respondents are identified as straightliners when the response to all
#' variables are the same. If e.g. cutoff = 0.5, respondents are identified as 
#' straightliners when half the variables are answered with the same answering option.
#' 
#' @examples 
#' df <- data(ds_survey_tidy)
#' 
#' srv_id_straightline(
#'  ds_survey_tidy,
#'  id_var = "sys_RespNum",
#'  category_var = "level",
#'  response_var = "labels")
#'  
#' @export
srv_id_straightline <- function(df, 
                                id_var,
                                category_var,
                                response_var,
                                cutoff = 0) {
    
    stopifnot(is.numeric(cutoff))
    stopifnot(is.character(id_var) & is.character(category_var) &
                  is.character(response_var))
    
    
    id_var <- rlang::sym(id_var)
    category_var <- rlang::sym(category_var)
    response_var <- rlang::sym(response_var)
    
    n_id <- 
        df[[id_var]] %>%
        unique %>%
        length
    
    n_response_options <-
        df[[response_var]] %>%
        unique %>% 
        length
    
    n_variables <-
        df[[category_var]] %>%
        unique %>% 
        length
    
    individual_counts <-
        df %>%
        group_by(!!id_var, !!response_var) %>%
        summarize(count = n())
    
    n_cutoff <- n_variables - round((n_variables * cutoff),0)
    
    potential_straightliners <-
        individual_counts %>%
        filter(count >= n_cutoff) %>%
        arrange(-count) %>%
        ungroup()
    
    straightliner_table <-
        potential_straightliners %>%
        group_by(!!response_var, count) %>%
        summarize(n = n()) %>%
        arrange(-count)
    
    message(paste("Your data consists of", n_variables, "variables.",
                  "\nSee table below to find out how often the same response was made for all variables (based on your specified cutoff)"))
    
    message("\n", paste0(
        capture.output(straightliner_table), 
        collapse = "\n") , "\n"
    )
    
    message(paste("Identified",
                  length(unique(potential_straightliners[[id_var]])),
                  "potential straightliners out of",
                  n_id, "respondents:"))
    return(sort(unique(potential_straightliners[[id_var]])))
    
}
