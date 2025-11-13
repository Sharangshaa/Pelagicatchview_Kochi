library(shiny)
library(leaflet)
library(tidyverse)
##read data----------------------------------------
df <- read.csv("effort_points.csv")

##to set view focused on the location------------------------------------
mean_lat <- mean(df$latitude, na.rm = TRUE)
mean_lng <- mean(df$longitude, na.rm = TRUE)

## set color for the gear points on the map ------------------------------
pal <- colorFactor("viridis", levels = unique(df$gear))

# Define UI for application--------------------------------------------
ui <- fluidPage(

    # Application title--------------------------------------
  titlePanel("Mock fisheries effort map of pelagic fishers"),

    # Sidebar for filtering gear used with a slider input for years-------------------------
  sidebarLayout(
    sidebarPanel(
      selectInput("gear", "Select Gear", 
                  choices = c("All", unique(df$gear))),
      sliderInput("year", "Select Year", 
                  min = min(df$year), max = max(df$year), 
                  value = c(min(df$year), max(df$year)), step = 1, sep = "")
    ),

        # Show a plot of Indian ocean-----------------------
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  filteredData <- reactive({
    data <- df %>%
      filter(year >= input$year[1], year <= input$year[2])
    if (input$gear != "All") {
      data <- data %>% filter(gear == input$gear)
    }
    data
  })
  ## define default zoom using mean of lat and longs-----------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% setView(lng = mean_lng, lat = mean_lat,zoom=5)
  })
  
  observe({
    data <- filteredData()
    leafletProxy("map", data = data) %>% 
      clearMarkers() %>% 
      clearShapes() %>% 
      clearControls() %>%
      addCircles(lng = ~longitude, lat = ~latitude,
                 
                 ## defining radius of fishing ground based the area covered (buffer is the fishing area explored around the point)----------
                 radius = data$buffer,
                 stroke = FALSE, fillOpacity = 0.5,color = ~pal(gear),
                 
                 ## Show catch details of the point when click through popup-----------------------
                 popup = ~paste("<strong> Billfish catch: </strong>", target_volume,"kg", "<br>",
                                "<strong> Shark catch:</strong>", elasmo_volume,"kg", "<br>",
                                "<strong> Trip Length:</strong>", trip_day, "days"))  %>%
      
                   ##legend for the gear on map------------------------------------                    
                  addLegend(
                  position = "bottomright",
                  pal = pal,
                  values = ~gear,
                  title = "Gear",
                  opacity = 1
      )
    
  }) 
}

shinyApp(ui = ui, server = server)

