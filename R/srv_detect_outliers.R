#' @importFrom grDevices boxplot.stats
#' @importFrom graphics boxplot 
#' @importFrom graphics hist
#' @importFrom graphics par
#' 
#' @title Survey Tools - Detect Outliers (and remove)
#'
#' @description Detecting outliers in numerical columns based on Tukey’s method 
#' to identify outliers ranged above and below the 1.5*IQR. 
#' 
#' @return An outlier comparison and a vector with the same values -
#' either removed or substitated
#' depending on your selection.
#' 
#' @param x A numerical vector to check for outliers.
#' @param replace Replace identified outliers with \code{type}?
#' @param type What to replace the identified outliers with. Can be the 
#' "mean", "median", or "NA". 
#' @param value If a value is supplied, the outliers will be replaced with 
#' said value. Will always overwrite the "type" parameter is also supplied.
#' @examples 
#' x <- iris$Sepal.Width
#' library(dplyr)
#' # Outlier detection & removal if desirable
#' y <- srv_detect_outliers(x, replace = TRUE, type = "mean")
#' y <- srv_detect_outliers(x, replace = TRUE, type = "NA")
#' a <- srv_detect_outliers(x, replace = TRUE, type = "NA")
#' sum(is.na(a))
#' 
#' z <- srv_detect_outliers(x, replace = FALSE, type = "NA")
#' sum(is.na(z))
#' 
#' # And in a pipe:
#' 
#' abc <- 
#'    iris %>%
#'    mutate(Sepal.Width.Replaced = srv_detect_outliers(Sepal.Width,
#'            replace = TRUE, type = "NA"))
#' 
#' bcd <- 
#'    iris %>%
#'    mutate(Sepal.Width.Replaced = srv_detect_outliers(Sepal.Width,
#'           replace = FALSE))
#'    
#' xyz <-
#'    iris %>%
#'    mutate(Sepal.Width.Replaced = srv_detect_outliers(Sepal.Width,
#'           replace = TRUE, value = 25))
#' 
#' mean(abc$Sepal.Width.Replaced, na.rm = TRUE)
#' mean(bcd$Sepal.Width.Replaced)
#' mean(xyz$Sepal.Width.Replaced)
#' 
#' 
#'@export
srv_detect_outliers <- function(x,
                                replace = FALSE,
                                type = c("mean", "median", "NA"),
                                value = NULL) {
    
    stopifnot(is.logical(replace))
    stopifnot(is.null(value) | is.numeric(value))
    if(replace){
        stopifnot(type %in% c("mean", "median", "NA"))
    }
    
    
    
    # Original data ----
    # Vector data summary
    tot <- sum(!is.na(x))
    na1 <- sum(is.na(x))
    m1 <- mean(x, na.rm = TRUE)
    
    # Plotting variable w/ outliers
    par(mfrow=c(2, 2), oma=c(0,0,3,0))
    boxplot(x, main="With outliers")
    hist(x, main="With outliers", xlab=NA, ylab=NA)
    
    # Outlier data ----
    # Check which values are outliers
    outlier_values <- boxplot.stats(x)$out
    
    # Mean of the outliers
    mo <- mean(outlier_values)
    
    # How many outliers?
    na2 <- length(outlier_values)
    # Replacing outliers with either the "mean", "median" or "na"
    
    if(is.null(value)){
        
        if(x %in% outlier_values & type == "mean") 
            var_outlier_replaced <- mean(x, na.rm = TRUE)
        if(x %in% outlier_values & type == "median")
            var_outlier_replaced <- median(x, na.rm = TRUE)
        if(x %in% outlier_values & type == "NA")
            var_outlier_replaced <- NA_real_ # necessary NA specification
        
    } else {
        var_outlier_replaced <- ifelse(x %in% outlier_values, value, x)
    }
    
    m2 <- mean(var_outlier_replaced, na.rm = TRUE)
    # Plotting vector with outliers replaced according to the wish
    boxplot(var_outlier_replaced, main="After replacing outliers")
    hist(var_outlier_replaced, main="After replacing outliers", xlab=NA, ylab=NA)
    
    message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
    message("Proportion (%) of outliers: ", round((na2 - na1) / tot*100,2))
    message("Mean of the outliers: ", round(mo,2))
    message("Mean without removing outliers: ", round(m1,3))
    message("Mean if we remove outliers: ", round(m2,3))
    
    if(replace){
        # Replacing outliers with a type (NA/ mean / median)
        message("\nOutliers successfully replaced")
        return(var_outlier_replaced)
    }
    else{
        message("\nNothing changed")
        return(x)
    }
}
