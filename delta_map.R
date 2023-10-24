library(shiny)
library(contentid)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shinythemes)
library(sf)
library(leaflet)
library(snakecase)

# read in the data from EDI
sha1 <- 'hash://sha1/317d7f840e598f5f3be732ab0e04f00a8051c6d0'
delta.file <- contentid::resolve(sha1, registries=c("dataone"), store = TRUE)

# fix the sample date format, and filter for species of interest
delta_data <- read.csv(delta.file) %>% 
  mutate(SampleDate = mdy(SampleDate))  %>% 
  filter(grepl("Salmon|Striped Bass|Smelt|Sturgeon", CommonName)) %>% 
  rename(DissolvedOxygen = DO,
         Ph = pH,
         SpecificConductivity = SpCnd)

cols <- names(delta_data)

sites <- delta_data %>% 
  distinct(StationCode, Latitude, Longitude) %>% 
  drop_na() %>% 
  st_as_sf(coords = c('Longitude','Latitude'), crs = 4269,  remove = FALSE)


ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"),
             collapsible = TRUE,
             HTML('<a style = "text-decoration:non;cursor:default;color:#FFFFFF;" class = "active" href = "#">Sacramento River Floodplain Data</a>'),
             id = "nav",
             windowTitle = "Sacramento Floodplain data",
             tabPanel("Data Sources",
                      verticalLayout(
                        titlePanel("Sacramento River floodplain fish and water quality"),
                        tags$hr(),
                        p("Map of area"),
                        mainPanel(leafletOutput("map"))
                      )
                      ),
             tabPanel("Explore",
                      verticalLayout(
                        p("analysis")
                      )) 
             )
 
)

server <-  function(input,output){
  output$map <- renderLeaflet({
    leaflet(sites) %>% 
      addTiles() %>% 
      addCircleMarkers(data = sites,
                       lat = ~Latitude,
                       lng = ~Longitude,
                       radius = 10,
                       fillColor = "grey",
                       fillOpacity = 1,
                       weight = 0.25,
                       color = "black",
                       label = ~StationCode
                       )
    
  })
  
}
  
shinyApp(ui = ui, server = server)
