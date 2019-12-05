
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
#table(Compiled_Data$international)
#Compiled_Data$international_type<-ifelse(Compiled_Data$international <=10, "low", 
                                  #ifelse(Compiled_Data$international >10 & Compiled_Data$international <= 20, "intermediate",
                                  #ifelse(Compiled_Data$international > 20, "high", "other")))
#table(Compiled_Data$SOC)
#Compiled_Data$SOC_type<-ifelse(Compiled_Data$SOC <=10, "low", 
                       # ifelse(Compiled_Data$SOC >10 & Compiled_Data$SOC <= 25, "intermediate",
                       # ifelse(Compiled_Data$SOC > 25, "high", "other")))


CompData<-Compiled_Data %>% 
  mutate(international_type=ifelse(international <=10, "low", 
                            ifelse(international >10 & international <= 20, "intermediate",
                            ifelse(international > 20, "high", "other"))), 
         SOC_type=ifelse(SOC <=10, "low", 
                  ifelse(SOC >10 & SOC <= 25, "intermediate",
                  ifelse(SOC > 25, "high", "other"))),
        Female_type=ifelse(female<=47,"low",
                     ifelse(female >47 & female <=53, "intermediate",
                     ifelse(female >53, "high", "other"))),
        retention_type= ifelse(retention<=92, "low",
                        ifelse(retention>92 & retention<=96,"intermediate",
                        ifelse(retention >96, "high", "other"))),  
                               
        graduation_type=ifelse(graduation<=85, "low",
                        ifelse(graduation>85 & graduation<=92,"intermediate",
                        ifelse(graduation >92, "high", "other"))), 
       
         tuition_type=ifelse(tuition<=51119, "low",
                      ifelse(tuition>51119 & tuition<=61750,"intermediate",
                      ifelse(tuition >61750, "high", "other"))), 
        rank_type=ifelse(rank<=10, "low",
                  ifelse(rank>10 & rank<=36,"intermediate",
                  ifelse(rank >36, "high", "other")))) 



ui <- fluidPage(
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"), 
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
    absolutePanel(top = 70, left = 20, 
                  checkboxInput("IS", "International Students", FALSE),
                  checkboxInput("soc", "Students of Color", FALSE),
                  checkboxInput("female", "Female Ratio", FALSE),
                  checkboxInput("retention", "Retention Rate", FALSE),
                  checkboxInput("graduation", "Graduation Rate", FALSE),
                  checkboxInput("tuition", "Tuition Cost", FALSE),
                  checkboxInput("rank", "Rank", FALSE)
    )))
    
    
server <- function(input, output, session) {
      
      pal1 <- colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = CompData$international_type)# build pallette
      
      pal2<-colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = CompData$SOC_type)
      
      pal3<-colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = CompData$female_type)
      
      pal4<-colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = CompData$retention_type)
      
      pal5<-colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = CompData$graduation_type)
      
      pal6<-colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = CompData$tuition_type)
      
      pal7<-colorFactor(
        palette = c('blue', 'yellow', 'red'),
        domain = CompData$rank_type)
      
      output$mymap <-renderLeaflet({
        leaflet(data) %>% 
          setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
          addTiles() %>% 
          addCircles(data = CompData %>% filter(year==2017),
                     lat = ~ lat, lng = ~ lon, label = ~as.character(paste0("College: ", sep = " ", College)), 
                     weight = 1, radius = ~sqrt(fulltime)*1500, popup = ~ College , 
                     color = ~gray,
                     fillOpacity = 0.5)
      })
      
      observe({
        proxy1 <- leafletProxy("mymap", data= CompData %>% filter(year==2017))
        proxy1 %>% clearMarkers()
        if (input$IS) {
        proxy1 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal1(international_type), fillOpacity = 0.2,  
                                      label = ~as.character(paste0(College, sep = " ", international))) %>%
                    addLegend("bottomright", pal = pal1, values = CompData$international_type,
                    title = "International Student Rate",
                    opacity = 3)}
        else {
          proxy1 %>% clearMarkers() %>% clearControls()
        }
      })

      observe({
        proxy2 <- leafletProxy("mymap", data = CompData %>% filter(year==2017))
        proxy2 %>% clearMarkers()
        if (input$soc) {
          proxy2 %>%  addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal2(SOC_type), fillOpacity = 0.2,  label = ~as.character(paste0(College, sep = " ", SOC))) %>%
            addLegend("bottomright", pal = pal2, values = CompData$SOC_type,
                      title = "Student of Color Rate",
                      opacity = 3)}
        else{
          proxy2 %>% clearMarkers() %>% clearControls()
          }
      })
      
      observe({
        proxy3 <- leafletProxy("mymap", data= CompData %>% filter(year==2017))
        proxy3 %>% clearMarkers()
        if (input$female) {
          proxy3 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal3(female_type), fillOpacity = 0.2,  
                                      label = ~as.character(paste0(College, sep = " ", female))) %>%
            addLegend("bottomright", pal = pal1, values = CompData$female_type,
                      title = "Female Ratio",
                      opacity = 3)}
        else {
          proxy3 %>% clearMarkers() %>% clearControls()
        }
      })
      
      observe({
        proxy4 <- leafletProxy("mymap", data= CompData %>% filter(year==2017))
        proxy4 %>% clearMarkers()
        if (input$retention) {
          proxy1 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal4(retention_type), fillOpacity = 0.2,  
                                      label = ~as.character(paste0(College, sep = " ", retention))) %>%
            addLegend("bottomright", pal = pal4, values = CompData$retention_type,
                      title = "Retention Rate",
                      opacity = 3)}
        else {
          proxy4 %>% clearMarkers() %>% clearControls()
        }
      })
      
      observe({
        proxy5 <- leafletProxy("mymap", data= CompData %>% filter(year==2017))
        proxy5 %>% clearMarkers()
        if (input$graduation) {
          proxy5 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal5(graduation_type), fillOpacity = 0.2,  
                                      label = ~as.character(paste0(College, sep = " ", graduation))) %>%
            addLegend("bottomright", pal = pal5, values = CompData$graduation_type,
                      title = "Graduation Rate",
                      opacity = 3)}
        else {
          proxy5 %>% clearMarkers() %>% clearControls()
        }
      })
      
      observe({
        proxy6 <- leafletProxy("mymap", data= CompData %>% filter(year==2017))
        proxy6 %>% clearMarkers()
        if (input$tuition) {
        proxy6 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal6(tuition_type), fillOpacity = 0.2,  
                                      label = ~as.character(paste0(College, sep = " ", tuition))) %>%
            addLegend("bottomright", pal = pal1, values = CompData$tuition_type,
                      title = "Tuition Cost",
                      opacity = 3)}
        else {
          proxy6 %>% clearMarkers() %>% clearControls()
        }
      })
      
      observe({
        proxy7 <- leafletProxy("mymap", data= CompData %>% filter(year==2017))
        proxy7 %>% clearMarkers()
        if (input$rank) {
        proxy7 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal7(rank_type), fillOpacity = 0.2,  
                                      label = ~as.character(paste0(College, sep = " ", rank))) %>%
            addLegend("bottomright", pal = pal7, values = CompData$rank_type,
                      title = "Rank cathegory",
                      opacity = 3)}
        else {
        proxy7 %>% clearMarkers() %>% clearControls()
        }
      })
}


shinyApp(ui = ui, server = server)

