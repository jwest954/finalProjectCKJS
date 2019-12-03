
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(tidyverse)
library(devtools) 
library(openintro)
library(maps)
library(ggthemes) 
library(ggmap)
library(gplots) #new
library(RColorBrewer) #new
library(sf) #new
library(leaflet) #new
library(carData) 



pal2 <- colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = Compiled_Data$international_type)


#Cathegorize data
table(Compiled_Data$international)
Compiled_Data$international_type<-ifelse(Compiled_Data$international <=10, "low", 
                                  ifelse(Compiled_Data$international >10 & Compiled_Data$international <= 20, "intermediate",
                                  ifelse(Compiled_Data$international > 20, "high", "other")))
table(Compiled_Data$SOC)
Compiled_Data$SOC_type<-ifelse(Compiled_Data$SOC <=10, "low", 
                        ifelse(Compiled_Data$SOC >10 & Compiled_Data$SOC <= 25, "intermediate",
                        ifelse(Compiled_Data$SOC > 25, "high", "other")))











ui <- fluidPage(
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"), 
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
    absolutePanel(top = 70, left = 20, 
                  checkboxInput("IS", "International Students", FALSE),
                  checkboxInput("soc", "Students of Color", FALSE)
    )))
    
    
server <- function(input, output, session) {
      
      pal1 <- colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = Compiled_Data$international_type)# build pallette
      
      pal2<-colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = Compiled_Data$SOC_type)
      
      output$mymap <-renderLeaflet({
        leaflet(data) %>% 
          setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
          addTiles() %>% 
          addCircles(data = Compiled_Data %>% filter(year==2017),
                     lat = ~ lat, lng = ~ lon, label = ~as.character(paste0("College: ", sep = " ", College)), 
                     weight = 1, radius = ~sqrt(fulltime)*1500, popup = ~ College , 
                     color = ~gray,
                     fillOpacity = 0.5)
      })
      
      observe({
        proxy1 <- leafletProxy("mymap", data= Compiled_Data %>% filter(year==2017))
        proxy1 %>% clearMarkers()
        if (input$IS) {
          proxy1 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal1(international_type), fillOpacity = 0.2,  
                                      label = ~as.character(paste0(College, sep = " ", international))) %>%
                     addLegend("bottomright", pal = pal1, values = Compiled_Data$international_type,
                      title = "International Student Percent cathegory",
                      opacity = 3)}
        else {
          proxy1 %>% clearMarkers() %>% clearControls()
        }
      })

      observe({
        proxy2 <- leafletProxy("mymap", data = Compiled_Data %>% filter(year==2017))
        proxy2 %>% clearMarkers()
        if (input$soc) {
          proxy2 %>%  addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal2(SOC_type), fillOpacity = 0.2,  label = ~as.character(paste0(College, sep = " ", SOC))) %>%
            addLegend("bottomright", pal = pal2, values = Compiled_Data$SOC_type,
                      title = "Student of Color Percent cathegory",
                      opacity = 3)}
        else{
          proxy2 %>% clearMarkers() %>% clearControls()
          }
      })
      
}


shinyApp(ui = ui, server = server)

