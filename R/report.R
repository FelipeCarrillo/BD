#' @title shiny app to visualize juvenile salmon runs
#'
#' @param dat Data with 6 columns
#' @import tidyverse DBI shiny dbplyr shinythemes DT shinycssloaders readxl
#' @return shiny app of salmon runs. Type 'report(Rbsalmon)' to launch shiny app
#'
#' @name report
#'
#'
#' @export
report <- function(dat) {
library(shiny)
library(tidyverse)
library(DBI)
library(dbplyr)
library(shinythemes)
library(DT)
library(shinycssloaders)
library(readxl)

runs <- c("Fall","Winter","Spring","LateFall","AllRuns")
dat$SampleDate <- as.Date(dat$SampleDate)
dat$year <- format(dat$SampleDate, "%Y")

shinyApp(
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Juvenile Salmon Monitoring Program",
             windowTitle = "Juvenile Salmon Graphs",
      sidebarLayout(
      sidebarPanel(
        h3("Select Salmon Run to Plot"),
        #tags$head(tags$style(".progress-bar{background-color:#3c763d;}")), # green banner
        tags$head(
          tags$style(
            ".progress-bar {
          background-image: linear-gradient(to right, red , yellow) !important;
          background-size: auto !important;
      }")
        ),

selectInput(inputId = "run",
                  label = "Chinook Runs",
                  choices = runs,
                  selected = "Winter"),

      #Slider to select custom years
      sliderInput(inputId = "Yearslider",
                  label="Years to plot",
                  sep="",
                  min=2000,
                  max=2014,
                  value=c(2010,2012))),
    mainPanel(
      withSpinner(plotOutput("plot"),type=5,color="#00ff00",color.background = "orange"),
      withSpinner(DT::dataTableOutput("data1")))
    ))),

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  plot_data <- reactive({
  dplyr::filter(dat,race2 == input$run & year <= max(input$Yearslider) & year >= min(input$Yearslider))
 })

 plot_all <- reactive({
 dat[dat$year >= input$Yearslider[1] & dat$year <= input$Yearslider[2], ]
    })

  dataInput <- reactive({
    if (input$run == "AllRuns") {
      plot_all() }
      else{
        plot_data()
    }

    })

  output$data1 = DT::renderDataTable(server=FALSE,{
    dataInput() %>%  #datInput() comes from the reactive function above
      DT::datatable(extensions = "Buttons",
                    options = list(initComplete = JS("function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'goldenrod', 'color': 'blue'});",
                    "}"), scrollY = "300px", pageLength = 10, scrollX = TRUE, dom = "Bftsp",
                    buttons = c("excel")   #buttons = c("copy","excel")
                    ))
                   })

  # Plot data
  create_plots <- reactive({

#Make the plots
      theme_set(theme_classic())
      #xlabels <- 2000:2014
      switch(input$run,

        "Fall" = ggplot(plot_data(),aes(SampleDate,Count)) +
                         geom_point(color='blue',alpha=0.6) + theme_bw() +
          labs(x="",y="Number in thousands",title="Fall Run Salmon"),
        "Winter" = ggplot(plot_data(),aes(SampleDate,Count)) +
                      geom_point(color='orange',alpha=0.6) + theme_bw() +
          labs(x="",y="Number in thousands",title="Winter Run Salmon"),
        "Spring" = ggplot(plot_data(),aes(SampleDate,Count)) +
          geom_point(color='green',alpha=0.6) + theme_bw() +
          labs(x="",y="Number in thousands",title="Spring Run Salmon"),
        "LateFall" = ggplot(plot_data(),aes(SampleDate,Count)) +
          geom_point(color='purple',alpha=0.6) + theme_bw() +
          labs(x="",y="Number in thousands",title="Late Fall Run Salmon"),
        "AllRuns" = ggplot(plot_all(),aes(SampleDate,Count,color = race2)) +
          geom_point() + theme_bw() +
          labs(x="",y="Number in thousands",title="All Salmon Runs combined")
       )

    })#create_plots

  output$plot <- renderPlot({
   # showNotification("DONE!", duration = 5, closeButton =  TRUE, type = "warning")
    create_plots()

    }) # height = 200, width = 300 Plot window

}

# Run the application
#shinyApp(ui = ui, server = server)

) #parenthesis for clossing shinyApp
} #bracket for clossing function
