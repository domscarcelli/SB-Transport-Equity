library(shiny)
library(leaflet)
library(rgdal)
library(data.table)

#bus route colors
colors <- list("#cae868", "#4aefe4", "#9ffcb1", "#11c46b", "#76f79f", "#ef8687", "#82e060",
           "#1dc44c", "#abcdfc", "#f43feb", "#e26376", "#00b6f2", "#ea9d9e", "#fca6c8",
           "#7e55d1", "#e36e07", "#dd5654")

#bus route shapefile
shapedata <- readOGR(dsn = path.expand('~/*path*/shp/'), #input own path to shape files
                     layer = "Transpo", stringsAsFactors = F)

#interactive map to visualize peak times and monthly differences
ui <- fluidPage(
  headerPanel("Transpo Ridership Data"),
  mainPanel(width = 9,
    leafletOutput("mymap", height = 750)
  ),
  sidebarPanel(width = 3,
    selectInput("month", "Month:", 
                choices = c("September" = "_Sept",
                            "October" = "_Oct",
                            "November" = "_Nov",
                            "Total" = "_Tot"),
                selected = "_Tot"),
    selectInput("time","Ridership Segments", 
                choices = c("Total" = "Tot",
                            "AM Peak" = "AM",
                            "Midday" = "Mid",
                            "PM Peak" = "PM",
                            "Late Night" = "LN"),
                selected = "Total"),
    tableOutput("table")
  )
)

#building the map
server <- function(input, output) {
  output$table <- renderTable({
    DT <- data.table(
      Routes = c(shapedata@data[,"FENAME"]),
      Riders = c(shapedata@data[,paste0(input$time, input$month)])
    )
    DT
  })
  output$mymap <- renderLeaflet({
    rides <- ((as.numeric(shapedata@data[,paste0(input$time, input$month)])^0.5)) + 3
    content <- paste(sep = "<br/>",
                     paste("<b>", shapedata@data[,"FENAME"], "</b>", sep = " "),
                     paste("Rides:", as.numeric(shapedata@data[,paste0(input$time, input$month)]), sep = " ")
    )
    leaflet(shapedata) %>% addProviderTiles(provider = "Stamen.Toner") %>%
      addPolylines(color = colors, opacity = 1, weight = rides, popup = content)
      #addLegend("bottomright")
  })
}

shinyApp(ui = ui, server = server)
