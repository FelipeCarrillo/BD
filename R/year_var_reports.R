#' @title Generate automated pdf reports of each salmon run and year.
#'
#' @param data A dataframe with data. Preferably fish data and must have year and salmon race (race2) columns.
#' @param years Can be entered as: years = 2011 or years = 2011:2012.
#' @param fish_run Enter the variable to generate report for. eg. var = c('Fall', 'Winter', 'spring')
#' @param rmd_file Rmd file to use to generate reports.
#'
#' @import shiny rmarkdown kableExtra gt table1 parseLatex
#' @return Generate automated pdf reports for individual years and fish runs.
#' @seealso \code{\link{report}} to sum multiple years and runs into a single report.
#' @seealso \code{\link{generate_reports}} for other custom pdf report options.
#'
#' @name year_var_reports
#' @examples
#' library(BD)
#' library(tidyverse)
#' library(readxl)

#' my_wd <- system.file("examples", package = "BD")
#' setwd(my_wd)

#' salmon <- read_excel('RBsalmon.xlsx')
#' salmon <- na.omit(salmon)
#' salmon$SampleDate <- as.Date(salmon$SampleDate,"%m/%d/%Y")
#' salmon <- dplyr::filter(salmon,!race == "n/p")
#' salmon <- dplyr::filter(salmon,ForkLength > 0)
#' salmon$year <- format(salmon$SampleDate,"%Y") #year
#' salmon$week <- format(salmon$SampleDate,"%V") #week starts with 01 on jan 1..also "%W" and "%U" can be used
#' salmon$month <- format(salmon$SampleDate,"%B") #month name..abreviated "%b" or number "%m"

#' salmon <- salmon[,c(1,2,5,6,9,11,13,14:16)]
#' #salmon <- salmon %>% dplyr::filter(year >2009 & year < 2012); unique(salmon$year)
#' head(salmon)

#' #Line below runs all the years in salmon dataset and all the fish runs (be careful if your dataset contains many years)
#' year_var_reports(data = salmon, years = unique(data$year), fish_run = unique(data$race2), rmd_file = 'YearAndVariableLoop.rmd')

#' #Line below prints reports for only they years and runs selected.
#' year_var_reports(data = salmon, years = c(2009, 2011), fish_run = c('Fall', 'Winter', 'Spring'), rmd_file = 'YearAndVariableLoop.rmd')

#' #Reports can also be generated without typing the the function arguments but they MUST be in the same order as below.
#' year_var_reports(salmon, 2010:2011, c('Spring'), 'YearAndVariableLoop.rmd')
#'
#' @export
library(rmarkdown)
year_var_reports <- function(data, years, fish_run, rmd_file) {
  for (year in years) {
    for (runs in fish_run) {
      # Filter data for the current year and variable
      filtered_data <- data[data$year == year & data$race2 == runs, ]
      # Skip if no data for the combination
      if (nrow(filtered_data) == 0) next
      #render a pdf
      render(input = rmd_file,
             output_file = paste0("Report_", year, "_", runs, ".pdf"),
             params = list(year = year,runs = runs, data = data),
             envir = new.env(parent = globalenv()))
    }
  }
}

 # library(BD)
 # library(tidyverse)
 # library(readxl)
 #
 # my_wd <- system.file("examples", package = "BD")
 # setwd(my_wd)
 #
 # salmon <- read_excel('RBsalmon.xlsx')
 # salmon <- na.omit(salmon)
 # salmon$SampleDate <- as.Date(salmon$SampleDate,"%m/%d/%Y")
 # salmon <- dplyr::filter(salmon,!race == "n/p")
 # salmon <- dplyr::filter(salmon,ForkLength > 0)
 # salmon$year <- format(salmon$SampleDate,"%Y") #year
 # salmon$week <- format(salmon$SampleDate,"%V") #week starts with 01 on jan 1..also "%W" and "%U" can be used
 # salmon$month <- format(salmon$SampleDate,"%B") #month name..abreviated "%b" or number "%m"
 #
 # salmon <- salmon[,c(1,2,5,6,9,11,13,14:16)]
 # #salmon <- salmon %>% dplyr::filter(year >2009 & year < 2012); unique(salmon$year)
 # head(salmon)
 #
 # #Line below runs all the years in salmon dataset and all the fish runs (be careful if your dataset contains many years)
 # year_var_reports(data = salmon, years = unique(data$year), fish_run = unique(data$race2), rmd_file = 'YearAndVariableLoop.rmd')
 #
 # #Line below prints reports for only they years and runs selected.
 # year_var_reports(data = salmon, years = c(2009, 2011), fish_run = c('Fall', 'Winter', 'Spring'), rmd_file = 'YearAndVariableLoop.rmd')
 #
 # #Reports can also be generated without typing the the function arguments but they MUST be in the same order as below.
 # year_var_reports(salmon, 2010:2012, c('Spring'), 'YearAndVariableLoop.rmd')
