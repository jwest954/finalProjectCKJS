#Final project for data science

library(shiny)
library(tidyverse)
library(plotly)
library(readr)
library(readxl)
library(leaflet)
library(maps)
library(maptools)
library(readxl)

Endowments <- read_csv("Endowments.csv")

Compiled_Data <- read_csv("Compiled_Data.csv")

#tidy the data by pivoting it
tidy_Endowments <- Endowments %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year", 
               values_to = "endowment", names_ptypes = list(year=integer())) 

#read in the second dataset. Make sure it's still in the right location!
#The ../ indicates "go out one folder" 
USNews_Rankings <- read_excel("../PCDB_USNews_Rankings.xlsx", 
                              sheet = "US News Ranks", range = "A2:K43") %>% 
  rename(College = ...1)

#This tidys the second dataset by pivoting it
tidy_USNews_Rankings <- USNews_Rankings %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year", 
               values_to = "ranking", names_ptypes = list(year=integer()))
#Uploading data for the map 
map_data <- read_excel("../map_data/map-data.xlsx")

#tidying map data
map_data_longer <- map_data %>% 
  select(-1*starts_with("20")) %>% 
  pivot_longer(cols = starts_with("R20"),
               names_to = "year", 
               values_to = "ranking") %>% 
  mutate(year= str_sub(year, start=2, end=5))

map_data_lon <-map_data %>% 
  select(-1*starts_with("R20")) %>% 
  pivot_longer(cols=starts_with("20"),
               names_to = "year",
               values_to = "endowment")
#Full map data (like the actual one)
full_map_data<- map_data_longer %>% 
  full_join(map_data_lon, by=c("College", "year", "Region","lon", "lat", "State"))



ui <- fluidPage(
  titlePanel("College Information"),
  tabsetPanel(type="tabs",
              tabPanel("Search", helpText("Input your parameters to find colleges that match your search!
                                          NOTE: Data is for 2017"),
                       splitLayout(
                         textInput("tuition", "What should max tuition be?" ),
                         selectInput("Region", "What region should the school be in?",
                                     choices=unique(Compiled_Data$Region)),
                         textInput("rank", "How should the school be ranked?")),
                       textInput("testScore", "ACT or SAT score"),
                       radioButtons("percentile", "Where would you like to fall?", 
                                    choices = c("Top 25%", "Middle 50%", "Bottom 25%")),
                       tableOutput(outputId = "searchlist")),
              tabPanel("Comparison", sliderInput(inputId = "year_range", label = "Year Range", 
                                                 min = 2008, max=2017, value=c(2008,2017), sep = ""),
                       splitLayout(
                         selectInput("College1", "College 1:", 
                                   choices=unique(tidy_Endowments$College)),
                       selectInput("College2", "College 2:", 
                                   choices=unique(tidy_Endowments$College))
                       ),
                       helpText("These plots show the changes in the selected colleges' rankings for the selected range."),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Endowments", plotlyOutput(outputId = "plot1")),
                                   tabPanel("Ranking", plotlyOutput(outputId = "plot2"))
                       )),
              tabPanel("Map", leafletOutput(outputId="mymap"))
              
             
  ))
              
  
  
  
  

server <- function(input, output) {
  output$plot1 <- renderPlotly({print(ggplotly(tidy_Endowments %>% 
                                         filter(College ==input$College1 | College ==input$College2) %>% 
                                         ggplot(aes(x=year, y=endowment, color=College))+
                                         geom_line()+
                                         scale_x_continuous(limits = input$year_range)+
                                         coord_cartesian(ylim = c(0, 3000))))})

  output$plot2 <- renderPlotly({print(ggplotly(tidy_USNews_Rankings %>% 
                                                 filter(College ==input$College1 | College ==input$College2) %>% 
                                                 ggplot(aes(x=year, y=ranking, color=College))+
                                                 geom_line()+
                                                 scale_x_continuous(limits = input$year_range)+
                                                 coord_cartesian(ylim = c(0, 50), xlim = c(2010,2017))))})

 output$mymap <-renderLeaflet({
   leaflet(data) %>% 
     setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
     addTiles() %>% 
     addMarkers(data = full_map_data, lat = ~ lat, lng = ~ lon, label = ~College)
 })
 
 output$searchlist <- renderTable(Compiled_Data %>% filter(year==2017) %>% 
                                    select(-X1, -lat, -lon, -year) %>% 
                                    filter(tuition<input$tuition) %>% 
                                    filter(Region==input$Region) %>% 
                                    filter(rank<=input$rank) #%>% 
                                    #ifelse(input$percentile>=37, 
                                    
                                  )
     
}
#Extra comment

shinyApp(ui = ui, server = server)