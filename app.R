#rosemary map in R

library(shiny)
library(leaflet)
library(htmltools)
library(jsonlite)
library(shinyjs)
getwd()

setwd("C:\\Users\\hello\\source\\repos\\rosemaryMap_R")

#this rosemaryMap_R can be the same as the rosemaryMap forked from github for html 
#except the images must be in a www folder to be compatible with shiny and how it reads things



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #map { height: 100vh; }
      #rosemary-info {
        position: absolute;
        top: 60px;
        left: 50%;
        transform: translateX(-50%);
        background: white;
        padding: 15px;
        width: 300px;
        height: 400px;
        overflow-y: scroll;
        font-size: 14px;
        z-index: 1000;
        border-radius: 6px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
        line-height: 1.4;
        display: none;
      }
      #title {
        position: absolute;
        top: 10px;
        left: 50%;
        transform: translateX(-50%);
        background: white;
        padding: 6px 12px;
        font-size: 18px;
        z-index: 1000;
        border-radius: 4px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.2);
        cursor: pointer;
      }
    "))
  ),
  tags$div(
    id = "title",
    style = "display: flex; align-items: center; gap: 10px;",
    tags$img(src = "IMG_2098.PNG", height = "30px"),
    "🌿 Rosemary Map"
  ),
  div(id = "rosemary-info", 
      HTML("<h1>Rosemary (Salvia rosmarinus)</h1>
            <p><em>Commonly known as rosemary...</em></p>
            <h2>Description</h2>
            <p>Rosemary has a fibrous root system...</p>
            <h2>Uses</h2>
            <p>Used for culinary and fragrance purposes...</p>
            <p><a href='https://en.wikipedia.org/wiki/Rosemary' target='_blank'>Wikipedia</a></p>")
  ),
  leafletOutput("map")
)


server <- function(input, output, session) {
  # Load and parse GeoJSON as nested list
  rosemaryData <- fromJSON("www/rosemary.geojson", simplifyVector = FALSE)
  features <- rosemaryData$features
  
  # Extract attributes from each feature
  lngs <- sapply(features, function(f) f$geometry$coordinates[[1]])
  lats <- sapply(features, function(f) f$geometry$coordinates[[2]])
  plant_type <- sapply(features, function(f) f$properties$plant_type)
  notes <- sapply(features, function(f) f$properties$notes)
  images <- sapply(features, function(f) f$properties$image)
  images<-sub("images/","",images)
 # images<-sub("jpg","JPG",images)
  images<-sub("png","PNG",images)
  print(images)
  
  # Create popup content
  popups <- mapply(function(pt, note, lat, lng, img) {
    paste(
      
      "<b>Plant Type: </b>", pt, "<br>",
      "<b>Notes: </b>", note, "<br>",
      "<b>Coordinates: </b>", lat,", ", lng, "<br>",
      "<b>Google Maps Link: </b><a href='https://www.google.com/maps/place/", lat, ",", lng, "' target='_blank'>Open in Maps</a><br>",
      if (!is.null(img) && img != "") {
        paste0("<br><img src='", img, "' style='width:100%;max-width:200px;'>")
     } else ""
    )
  }, plant_type, notes, lats, lngs, images, SIMPLIFY = FALSE)
  


  print(unname(popups))
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Gray") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Streets") %>%
      addProviderTiles("OpenTopoMap", group = "Topographic") %>%
      setView(lng = -122.335232, lat = 47.643952, zoom = 12) %>%
      addMarkers(lng = lngs, lat = lats, popup = lapply(unname(popups),HTML)) %>%  #, popup = lapply(unname(popups),HTML)
      addLayersControl(
        baseGroups = c("Gray", "Streets", "Topographic"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
}

shinyApp(ui, server)
runApp()

