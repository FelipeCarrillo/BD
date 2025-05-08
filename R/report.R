#' @title Generate automated pdf reports of each salmon run and year.
#'
#' @param years Can be entered as: years = 2011 or years = 2011:2012.
#' @param var Enter the variable to generate report for. eg. var = c('Fall', 'Winter', 'spring')
#' @param input_file Rmd file to use to generate reports.
#' @param output_format Report with html, word or pdf formats. pdf format is the default.
#' @param output_name It will default to the name of the input_file unless the user selects a different name.
#'
#' @import shiny rmarkdown kableExtra gt table1 parseLatex
#' @return Generate automated pdf reports for individual years and fish runs. Can also sum multiple runs for a single year.
#' @seealso \code{\link{generate_reports}} to sum multiple years and runs into a single report.
#' @seealso \code{\link{year_var_reports}} for other custom pdf report options.
#'
#'
#' @name report
#' @examples
#' library(BD)
#' library(tidyverse)
#' library(readxl)
#'
#' my_wd <- system.file("examples", package = "BD")
#' setwd(my_wd)
#'
#' # Dataset used to generate report
#' salmon <- read_excel('RBsalmon.xlsx')
#' head(salmon)
#' report(2010:2012, c("Fall", "Spring"), "individualReport_mult.rmd") #THIS WORKS WELL
#'
#' @export
library(rmarkdown)

report <- function(years, var, input_file, output_format = "pdf_document", output_name = NULL) {
  for (race2 in var) {
    rmarkdown::render(
      input = input_file,
      output_file = paste0(gsub(" ", "_", paste(years, collapse = "-")), "_report.pdf"),
      params = list(year = years, runs = var), envir = new.env(parent = globalenv()))
  }
}

# library(BD)
# library(tidyverse)
# library(readxl)
#
# my_wd <- system.file("examples", package = "BD")
# setwd(my_wd)
# report(2010:2012, c("Fall", "Spring"), "individualReport_mult.rmd") #THIS WORKS WELL
