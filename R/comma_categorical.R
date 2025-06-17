#' @title Function to format categorical variables with commas.
#' @param x random variable.
#' @import gt table1
#' @return Implement comma format for categorical variables.
#' @name comma_categorical
#' @export
#'
##NOTE: This function is not being use as a function, instead is embeded in 'report', 'generate_reports' and 'year_var_reports' functions
library(table1)
comma_categorical <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
    sprintf("%s (%s%%)",
            prettyNum(FREQ, big.mark = ","),
            format(round(PCT, 1), nsmall = 1)))))
}
