###Interactive Maps
#Theodosius Struijk
#21/03/2025

#Sources used for data:
#D. R. Shackleton-Bailey, Letters to Atticus, vol. I, Loeb Classical Library (Cambridge, Massachusetts; London, England: Harvard University Press, 1999)


#Loading library
library(htmlwidgets)
library(tidyverse)
library(googlesheets4)
library(leaflet)
library(glue)

#Read in data
cicero <- read_sheet("https://docs.google.com/spreadsheets/d/1IpMJlijTsf4w_bq6KC62CO_I_7XMqY1NqvaRmBxkzYk/edit?gid=148633452#gid=148633452",
                     col_types = "cncnnnncnnnc",
                     range="Week12DAM")

#Filtering na
cicero %>% 
  filter(!is.na(Longitude)) %>% 
  filter(!is.na(Latitude))


#Creating map
 
#Creating label text
  labeltext <- glue(
  "<b>Location: </b> {cicero$location_name}<br/>",
  "<b>Date: </b> {cicero$date}<br/>",
  "<b>Letter: </b> {cicero$letter}<br/>",
  "<b>Recipient: </b> {cicero$Recipient}<br/>") %>%
  lapply(htmltools::HTML)->labeled

#creating base map  
  cicmapbase2 <- 
    leaflet(cicero) %>% addTiles() %>%
    addMarkers(lng = cicero$Longitude, 
               lat = cicero$Latitude,
               popup = labeled)
  
  #Getting layers
  esri <- grep("^Esri",providers, value = TRUE)
  for (provider in esri) {
    cicmapbase2 <- cicmapbase2 %>% addProviderTiles(provider,group = provider)
  }
  
  #Creating map
 cicmap2 <- cicmapbase2 %>% 
   addLayersControl(baseGroups = names(esri), #Adding layer control
                    options = layersControlOptions(collapsed = T)) %>% 
   addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE, #Adding minimap
              position = "bottomright") %>%
   addMeasure( #Adding measuring tool
     position = "bottomleft",
     primaryAreaUnit = "sqmeters",
     primaryLengthUnit = "meters",
     activeColor = "#2D535D",
     completedColor = "#9A457D"
   ) %>% 
   htmlwidgets::onRender("
                        function(el, x) {
                        var myMap = this;
                        myMap.on('baselayerchange',
                        function (e) {
                        myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                        })
                        }") %>% 
   addControl("", position = "topright")

  cicmap2 #Viewing map
  
  saveWidget(cicmap2, "cicmap2.html", selfcontained = TRUE) #Saving map
  