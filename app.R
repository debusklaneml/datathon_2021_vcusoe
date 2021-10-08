#see https://shiny.rstudio.com/gallery/superzip-example.html


# Setup -------------------------------------------------------------------

library(tidyverse)
library(shiny)
library(leaflet)
library(sf)
library(tidygeocoder)
library(nngeo)
library(jsonlite)
library(bslib)

load("data/meals_comp.RData")

source("R/functions.R")


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "minty"),
  
  titlePanel("Afterschool Meals Provider Locator"),
  sidebarLayout(
    sidebarPanel(
      
      h2("Instructions"),
      
      HTML(paste("This app provides a tool for users to look up nearby sites participating in the CACFP afterschool meals 
      program. Simply type your address into the box below, and the map will display your address (as a ", 
                 tags$span(style="color:red", "red marker"),
                 "), the closest site (as a ",
                 tags$span(style="color:#378805", "green marker"),
                 "), and additional nearby sites (as ",
                 tags$span(style="color:navy", "blue markers"),
                 ").", sep = "")
      ),
      
      br(),
      br(),
      
      textInput("address", tags$b("Type Your Address"), value = "1015 W Main St, Richmond, VA 23220"),
      
      br(),
      
      h2("Your Closest Site"),
      
      textOutput("closest"),
      
      h2("Learn More"),
      
      HTML(paste("To learn more about the CACFP afterschool meals program, you can visit the ", 
                 tags$a(href="https://state.nokidhungry.org/virginia/afterschool-meals/", "No Kid Hungry"),
                 " website or the ",
                 tags$a(href="https://www.vdh.virginia.gov/child-and-adult-care-food-program/", "Virginia Department of Health"),
                 "website",
                 sep = ""
      )
      )
    ),
    
    mainPanel(
      leafletOutput("map", height = 800)
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

