#' @title shiny app to visualize any biological dataset
#'
#' @param dat Data with any number of row and columns
#' @import readxl shiny GWalkR ggplot2
#' @return shiny app to do exploratory data analysis (EDA). Type 'explore(RBsalmon)' or any other dataset in your working directory to launch shiny app
#'
#' @name explore
#'
#' @export
explore <- function(dat) {
  library(GWalkR)
  library(shiny)
  library(readxl)

     #rb_fish <- read_excel('RBsalmon.xlsx')
    #head(rb_fish)
    shinyApp(
      ui = fluidPage(
        titlePanel("Explore dataset: "),
        gwalkrOutput("mygraph")
      ),
      server = function(input, output, session) {
        session$onSessionEnded(function() {
          stopApp()
        })

        output$mygraph = renderGwalkr(
          gwalkr(dat)
        )
      } #close server
    ) #close shiny app

} #close function
