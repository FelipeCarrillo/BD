
#' @title shiny app to print automated section 7 concurrence letters
#'
#' @import shiny rmarkdown shinyjs gridExtra
#' @return shiny app for documents. Type 'letter("yourfilename.rmd")' to launch the shiny interface.
#' @name letter
#' @examples
#' # Example usage. If output_file (output_name) is left blank in letter(), a pdf file is generated with the input_file name
#' library(rmarkdown)
#' library(gridExtra)
#'
#' my_wd <- system.file("examples", package = "BD")
#' setwd(my_wd)
#' letter("letter.rmd")
#' browseURL("letter.pdf")
#'
#' #Customize output name of rmd file
#' letter("letter.rmd", output_name = "BeautifulLetter.pdf")
#' browseURL("BeautifulLetter.pdf")
#'
#' @export
library(rmarkdown)
library(gridExtra)
letter <- function(input_file, output_format = "pdf_document", output_name = NULL) {
  rmarkdown::render(input = input_file,
                    output_format = output_format,
                    output_file = output_name)
}

#'
#'
# letter <- function() {
#   library(shiny)
#   library(rmarkdown)
#   rmarkdown::render(input = file.path(getwd(),"inst", "letter.rmd"),
#                     output_file = "myletter.pdf")
#
#   #rmarkdown::render(input = system.file("rmd/letter.Rmd", package="BD"),
#   #                  output_file = "myletter.pdf")
#   setwd("./inst")
# browseURL("myletter.pdf")
# setwd("..")
# }


