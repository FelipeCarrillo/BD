#' @title shiny app to visualize Delta Smelt
#'
#' @param dat Data with multiple columns
#' @import shiny shinydashboard mapview tidyverse leaflet sf shinyWidgets shinythemes scales shinyjs shinycssloaders
#' @return shiny app of Delta Smelt. Type 'delta_smelt(smelt)' to launch shiny app
#'
#' @name delta_smelt
#'
#'
#' @export
delta_smelt <- function(dat) {
  if (!require("pacman")) install.packages("pacman")
  p_load(shiny, shinydashboard, mapview, tidyverse, leaflet, sf, shinyWidgets, shinythemes, scales, shinyjs, shinycssloaders, dygraphs)

  source('setSliderColor.R') #Import this function from Katherines's folder to color the slider
  #https://github.com/dreamRs/shinyWidgets/blob/26838f9e9ccdc90a47178b45318d110f5812d6e1/R/setSliderColor.R ##Check this out

  mapviewOptions(basemaps = c("Esri.WorldStreetMap","Esri.WorldTopoMap","Esri.NatGeoWorldMap","USGS.USImageryTopo"),
                 raster.palette = grey.colors,
                 vector.palette = colorRampPalette(c("snow", "cornflowerblue", "grey10")),
                 na.color = "magenta",
                 layers.control.pos = "topright")

  #ds <- read_excel('delta_smelt.xlsx', sheet = "delta_smelt")
  #head(ds)

  #ds <- save(ds, file = 'smelt.rda')
  dat <- dat[,c(1:2,11:13,19,22)]
  dat$numb_fish <- rep(1,nrow(dat)) #add number of fish...each row is one fish
  dat$LifeStage <- with(dat,ifelse(is.na(LifeStage), 'unkLifeStage', LifeStage))

  dat <- dat |> rename(lat = LatitudeStart, lon = LongitudeStart)
  # Exclude records where lat and lon columns have NA
  dat <- dat %>%
    dplyr::filter(!is.na(lat) & !is.na(lon))

  dat$SampleDate <- as.Date(dat$SampleDate,"%m/%d/%Y")
  dat$year <- format(dat$SampleDate,"%Y")#year
  dat$year <- as.numeric(dat$year)
  dat <- dat %>% mutate(month = floor_date(SampleDate, "month")) #Delete this if a different approach is taken while displaying plot
  #write_xlsx(ds, 'test.xlsx')

  # make ds sf object:
  ds_sf <- st_as_sf(dat,coords = c(5, 4), remove = F, crs = 4326)
  colors <- colorRampPalette(c('yellow', 'red', 'blue', 'green'))

  shinyApp(
  ui <- fluidPage(
    useShinyjs(),
    theme = shinytheme("cerulean"),
    navbarPage("Delta Smelt Tracking",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h3("Select Options"),
                              selectInput(inputId = "type",
                                          label = "Type",
                                          choices = c("LifeStage", "Survey", "ReleaseMethod"),
                                          selected = "LifeStage"),

                              #conditionalPanel( # show panel conditionally
                              # condition = "input.type == 'LifeStage'", #Meaning = Only show this control when 'type' above is = LifeStage, otherwise hide it
                              pickerInput(inputId = "lstages",
                                          label = "StageBox",
                                          choices = c("Adult", "Larva", "Juvenile", "unkLifeStage"),
                                          selected = c("Adult", "Larva", "Juvenile", "unkLifeStage"),
                                          options = list(`actions-box` = TRUE),
                                          multiple = TRUE),
                              #),
                              checkboxInput("Fixed","Grid Graphic", value = FALSE),
                              br(),#Add a blank row
                              hr(),#Add a horizontal line
                              hr(),
                              chooseSliderSkin("Square"),
                              setSliderColor(c("LightSeaGreen ", "#FF4500", "", "Teal"), c(1, 2, 4)),
                              sliderInput(inputId =  "Yearslider",
                                          label = "Years to plot",
                                          sep = "",
                                          min = min(dat$year),
                                          max = max(dat$year),
                                          step = 1,
                                          #value = 2024)),
                                          value = c(min = min(dat$year)+1,max = max(dat$year)-1))),
                 # mainPanel(leafletOutput("map",width = "100%", height="87vh"))
                 #)
                 #))
                 mainPanel(
                   tabsetPanel(id = "myTabs",
                               #tabPanel("Map", leafletOutput("map",width = "100%", height="87vh")),
                               tabPanel("Map", value = "mymap", leafletOutput("map", width = "113%", height="87vh")), #width="100%",height="1000px"
                               tabPanel("Plot", value = "myplot", plotOutput("plot", height = "85vh", width = "113%")),
                               #tabPanel("Plot", dygraphOutput("dyTest")),
                               tabPanel("Data", value = "mydata", DT::dataTableOutput("mytable")),
                               tabPanel("", value = "empty", p("This is just a placeholder.")))#Invisible Placeholder just in case I need another tab(notice "")
                 ))
    )
  ),


  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })

    #output$plot <- renderPlot({
    #  plot(cars, main = "Plot 1: Cars")
    #})

    #############################################################################################
    #IMPORTANT: 'observe' hides the pickerInput 'lstages' when 'LifeStage' is not the active selection and also
    #conditionalPanel on pickerInput works (see above) but 'toggle' is more intuitive to me..so I will use it below
    observe({
      toggle(id = "lstages", condition = input$type == "LifeStage", anim = TRUE, animType = "slide")
      #Read toggle like this:
      #ONLY SHOW THE 'lstages' DROPDOWN WHEN "type" is equal to 'LifeStage', otherwise hide 'lstages'
    })
    ###############################################################################################

    output$map <- renderLeaflet({
      req(input$lstages)
      md <- ds_sf #Make a copy of the ds_sf dataset
      head(md)

      if(input$type == "LifeStage") # only filter by LifeStage if LifeStage is selected..
        ###md <- ds_sf %>% filter(LifeStage %in% input$lstages & year == input$Yearslider)
        md <- ds_sf %>% filter(LifeStage %in% input$lstages & year >= min(input$Yearslider) & year <= max(input$Yearslider))

      else
        ###md <- ds_sf %>% filter(year == input$Yearslider)
        md <- ds_sf %>% filter(year >= min(input$Yearslider) & year <= max(input$Yearslider))

      #final <- mapview(md, zcol = "LifeStage", col.regions = colors, layer.name = "LifeStage", alpha = 0.2, cex = 4)
      final <- mapview(md, zcol = input$type, col.regions = colors, layer.name = input$type, alpha = 0.2, cex = 4)
      final@map
    })


    #Make the reactive event to dynamically create and update the table
    tabdata <- reactive({
      #Drop geometry from dataframe otherwise it will show up as an empty column in table
      ds_sf <- st_drop_geometry(ds_sf)

      if(input$type == "LifeStage") # only filter by LifeStage if LifeStage is selected..
        md <- ds_sf %>% filter(LifeStage %in% input$lstages & year >= min(input$Yearslider) & year <= max(input$Yearslider))

      else
        md <- ds_sf %>% filter(year >= min(input$Yearslider) & year <= max(input$Yearslider))

    })

    ##Use tabdata() result below to feed the table and to print the dynamic ggplot title years being visualized
    output$mytable = DT::renderDataTable(server = TRUE,({ #server = TRUE to display only the data selected with the selectInput controls
      ##Don't display the month and numb_fish columns in the table
      select(tabdata(),-c(month, numb_fish))

    })
    )

    #Disable the checkbox when the Plot window is not active
    observeEvent(input$myTabs,{
      if(input$myTabs == "myplot"){
        enable("Fixed")
      } else {
        disable("Fixed")
      }
    })



    output$plot <- renderPlot({

      if (!input$Fixed) {
        ggplot(tabdata(), aes(x = month, y = numb_fish)) +
          geom_bar(stat = "identity", aes_string(fill = input$type)) + #Notice the aes_string because LifeStage is a character
          scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
          theme_classic(base_size = 12) + scale_fill_brewer(palette="Paired") +
          theme(legend.position = 'bottom') + labs(title = paste0("Monthly ",input$type , " Data for Year(s) ",
                                                                  min(tabdata()$year), " to " ,max(tabdata()$year)))
      } else {

        ggplot(tabdata(), aes(x = month, y = numb_fish)) +  ##Used width = 20 to avoid the super skinny bars when plotting 'unkLifestage'
          geom_bar(width = 20, stat = "identity", aes_string(fill = input$type)) + #Notice the aes_string because LifeStage is a character
          scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
          theme_bw(base_size = 12) + scale_fill_brewer(palette="Paired") +
          facet_grid(LifeStage ~ input$type, scales = "free", space= "free") +
          theme(legend.position = 'bottom', panel.grid.minor = element_blank()) +
          labs(title = paste0("Monthly ",input$type , " Data for Year(s) ",
                              min(tabdata()$year), " to " ,max(tabdata()$year)))
      }

    })


  }

  )#clossing for ui fluidpage
  } # bracket for closing delta_smelt function
 # shinyApp(ui, server)



