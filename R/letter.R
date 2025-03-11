
#' @title shiny app to print automated section 7 concurrence letters
#'
#' @import shiny rmarkdown shinyjs
#' @return shiny app for documents. Type 'letter()' to launch the shiny interface.
#' @name letter
#'
#' @examples
#' letter()
#' @export
#'
letter <- function() {
  library(shiny)
  library(rmarkdown)
  rmarkdown::render(input = file.path(getwd(),"inst", "letter.rmd"),
                    output_file = "myletter.pdf")

  #rmarkdown::render(input = system.file("rmd/letter.Rmd", package="BD"),
  #                  output_file = "myletter.pdf")
  setwd("./inst")
browseURL("myletter.pdf")
setwd("..")
}


