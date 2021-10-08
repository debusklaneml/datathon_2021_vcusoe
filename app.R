#see https://shiny.rstudio.com/gallery/superzip-example.html


# Setup -------------------------------------------------------------------

library(tidyverse)
library(shiny)
library(leaflet)
library(sf)
library(tidygeocoder)
library(nngeo)
library(jsonlite)

load("data/meals_comp.RData")

source("R/functions.R")


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("After School Meals"),
  sidebarLayout(
    sidebarPanel(
      textInput("address", "Address", value = "1015 W Main St, Richmond, VA 23220")
    ),
    mainPanel(
      leafletOutput("map", height = 800),
      textOutput("closest")
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  adr_df <- reactive({
    lat_long_fun(input$address)
  })
  
  closest_ind <- reactive({
    get_closest_ind(adr_df(), y = meals_comp)
  })
  
  closest_txt <- reactive({
    make_closest_text(closest_ind(), df = meals_comp)
  })
  
  map_df <- reactive({
    create_map_df(adr_df(), meals_comp, name = input$address, ind = closest_ind())
  })
  
  output$map <- renderLeaflet({
    leaflet(map_df()) %>%
      addTiles() %>%
      setView(lng = adr_df()$long[[1]], lat = adr_df()$lat[[1]], zoom = 13) %>%
      addCircleMarkers(color = ~pal(type),
                       stroke = FALSE,
                       label = ~sitename,
                       fillOpacity = .6)
  })
  
  output$closest <- renderText(closest_txt())
  
}

# RunApp ------------------------------------------------------------------

shinyApp(ui, server)

