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
library(shinythemes)
library(DT)

Endowments <- read_csv("Endowments.csv")

Final_Data_2017 <- read_csv("Final_Data_2017.csv")
Final_Data_2017 <- Final_Data_2017 %>% filter(Region !="NA")


#tidy the data by pivoting it
tidy_Endowments <- Endowments %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year", 
               values_to = "endowment", names_ptypes = list(year=integer())) 

#The .././ indicates "go out one folder, then in one folder" 
USNews_Rankings <- read_excel(".././Compiled_Data/PCDB_USNews_Rankings.xlsx", 
                                 range = "A2:K43") %>% 
  rename(College = ...1)

#tidy the data by pivoting it
tidy_USNews_Rankings <- USNews_Rankings %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year", 
               values_to = "ranking", names_ptypes = list(year=integer()))

Compiled_Data <- read_csv("Compiled_Data.csv")

#Uploading data for the map 
map_data <- read_excel("../map_data/map-data.xlsx")

#tidy the map data
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

#Cathegorizing data for map 
CompData<-Compiled_Data %>% 
  mutate(international_type=ifelse(international <=10, "low", 
                                   ifelse(international >10 & international <= 20, "intermediate",
                                          ifelse(international > 20, "high", "other"))), 
         SOC_type=ifelse(SOC <=10, "low", 
                         ifelse(SOC >10 & SOC <= 25, "intermediate",
                                ifelse(SOC > 25, "high", "other"))),
         female_type=ifelse(female<=47,"low",
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
                                 ifelse(rank >36, "high", "other"))),
         size_type=ifelse(fulltime<=1677, "small",
                          ifelse(fulltime>1677 & fulltime<=2344,"intermediate",
                                 ifelse(fulltime >2344, "big", "other"))))
         


#Building pallettes for map
pal1 <- colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$international_type)

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

pal8<-colorFactor(
  palette = c('blue', 'yellow', 'red'),
  domain = CompData$size_type)


ui <- fluidPage(
  theme = shinytheme("united"),
  h1("Shortlist"),
  h4("A comparison tool for liberal arts colleges"),
  br(),
  tabsetPanel(type="tabs",
        tabPanel("User Guide",
                 p(strong("Shortlist User Guide")),
                 p("Shortlist is a search engine for prospective higher education students who are looking for a liberal arts experience."),
                 p(strong("Search Tab")),
                 p("In the “Search” tab, users may input college characteristics, and the engine live updates to display all colleges in the dataset that match the user’s criteria. These search characteristics are tuition, region, rank, athletic division, calendar system, campus type, and user’s SAT/ACT score (user must select which percentile they would like to fall under). College search output includes region, state, number of full time students, number of part time students, percent of international students, percent of students of color (SOC), percent of female students, retention rate, graduation rate, tuition, school rank, and college endowment. The user can view these summarizing values and evaluate the schools."),
                 p(strong("Comparison Tab")),
                 p("In the “Comparison” tab, users may directly compare two colleges from the Search tab. The engine uses data from 2008-2017. It displays summary graphs of endowment and school rank over time. The user can use gganimate tools to zoom in on graphs to view fine detail changes over time."),
                 p(strong("Map Tab")),
                 p("In the “Map” tab, the user can select one or multiple college variables, and a map will visualize characteristics of the colleges in the data set, based on 2017 data. These characteristics are percent of international students, percent of SOC, percent of female students, retention rate, graduation rate, tuition, and rank."),
                 p(strong("Considerations")),
                 p("It is important to note that the data set used in this app is limited to 40 liberal arts colleges in the US. It is primarily useful for students who wish to refine their college search once they have decided to attend a small liberal arts school. This app provides a framework that could be used with a larger data set of schools to expand the scope of the search. Additionally, the data used in this app is from two different sources. One includes data from 2008 to 2017, and the other from 2018.")),
        tabPanel("Search", helpText(strong("Input your parameters to find colleges that match your search! NOTE: Data is for 2017")),
                flowLayout(
                         numericInput("tuition1", "Maximum tuition", 70000, 50000, 70000, 5000),
                         selectInput("Region", "Region",
                                     choices= c(Final_Data_2017$Region, "Any"), selected = "Any"),
                         selectInput("calendarsystem", "Calendar system",
                                      choices=c(Final_Data_2017$calendar_system, "Any"), selected = "Any"),
                         numericInput("acceptancerate", "Minimum acceptance rate", .7, .05, .7, .05),
                         selectInput("campus", "Campus type",
                                      choices = c(Final_Data_2017$campus, "Any"), selected = "Any"),
                         selectInput("division", "Athletic division",
                                      choices = c(Final_Data_2017$division, "Any"), selected = "Any"),
                         textInput("rank", "Minimum school rank"),
                          textInput("testScore", "ACT or SAT score"),
                          radioButtons("percentile", "Percentile", 
                                     choices = c("Top 25%", "Middle 50%", "Bottom 25%"))
                         ),
                        dataTableOutput(outputId = "searchlist")),
        tabPanel("Comparison",
                flowLayout(
                         selectInput("College1", "College 1:", 
                                   choices=unique(tidy_Endowments$College)),
                         selectInput("College2", "College 2:", 
                                   choices=unique(tidy_Endowments$College))),
                         helpText("These plots show the changes in the selected colleges' rankings for the selected range."),
                         tabsetPanel(type = "tabs",
                                   tabPanel("Endowments", plotlyOutput(outputId = "plot1")),
                                   tabPanel("Ranking", plotlyOutput(outputId = "plot2")))),
        tabPanel("Map", leafletOutput(outputId="mymap"),
                 helpText("The map shows the location of the colleges classified by color in the categories shown in the panel with data from 2017"),
                       absolutePanel(top = 250, left = 20, 
                                     checkboxInput("IS", "International Students", FALSE),
                                     checkboxInput("soc", "Students of Color", FALSE),
                                     checkboxInput("female", "Female Ratio", FALSE),
                                     checkboxInput("retention", "Retention Rate", FALSE),
                                     checkboxInput("graduation", "Graduation Rate", FALSE),
                                     checkboxInput("tuition", "Tuition Cost", FALSE),
                                     checkboxInput("rank", "Rank", FALSE),
                                     checkboxInput("fulltime", "Student body size", FALSE)))
                   

  
  
  
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
                                                 scale_y_reverse()+
                                                 coord_cartesian(ylim = c(0, 50), xlim = c(2010,2017))))})

  output$mymap <-renderLeaflet({
    leaflet(data) %>% 
      setView(lng = -99, lat = 45, zoom = 2) %>% 
      addTiles() %>% 
      addCircles(data = CompData %>% filter(year==2017),
                 lat = ~ lat, lng = ~ lon, label = ~as.character(paste0("College: ", sep = " ", College)), 
                 weight = 1, popup = ~ College , 
                 radius=20000, color = ~gray,
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
        addLegend("bottomright", pal = pal3, values = CompData$female_type,
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
      proxy4 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE,
                                  color = ~pal4(retention_type), fillOpacity = 0.2,  
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
        addLegend("bottomright", pal = pal6, values = CompData$tuition_type,
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
  
  observe({
    proxy8 <- leafletProxy("mymap", data= CompData %>% filter(year==2017))
    proxy8 %>% clearMarkers()
    if (input$fulltime) {
    proxy8 %>% addCircleMarkers(lat = ~ lat, lng = ~ lon, stroke = FALSE, color = ~pal8(size_type),
                          fillOpacity = 0.2,  
                          label = ~as.character(paste0(College, sep = " ", fulltime))) %>%
        addLegend("bottomright",pal = pal8, 
                  values = CompData$size_type,
                  title = "Student body size",
                  opacity = 3)
        }
    else {
      proxy8 %>% clearMarkers() %>% clearControls()
    }
  })
  

  output$searchlist <- DT::renderDataTable(DT::datatable({
    data <- Final_Data_2017 %>% 
    select(-X1, -lat, -lon, -year, -X1_1, -type) %>%
    filter(tuition<input$tuition1) %>% 
    filter(acceptance_rate <= input$acceptancerate)
    if(input$Region != "Any"){
      data <- data[data$Region == input$Region,]
    }
    if(input$division != "Any"){
      data <- data[data$division == input$division,]
    }
    if(input$calendarsystem != "Any"){
      data <- data[data$calendar_system == input$calendarsystem,]
    }
    if(input$campus != "Any"){
      data <- data[data$campus == input$campus,]
    }
    #if(input$testScore >= 37){
    #   if (input$percentile == "Top 25%"){
    #     if (data$sat_composite_75 > input$testScore) {
    #       data <- data[data$sat_composite_75 == input$sat_composite_75,]}}
    #   if (input$percentile == "Middle 50%)
    #     if (data$sat_composite_75 > input$testScore or data$sat_composite_25 < input$testScore) {
      #     data <- data[data$sat_composite_75 == input$sat_composite_75,]},
    #   if (input$percentile == "Top 75%) {
    #     if (data$sat_composite_25 < input$testScore {
    #       data <- data[data$sat_composite_25 == input$sat_composite_25,]})}
    # else{}
    # }
    data2 <- data %>% 
      rename(`calendar system`=calendar_system,
             `acceptance rate`=acceptance_rate,
             `SAT composite 25th percentile`=sat_composite_25,
             `SAT composite 75th percentile`=sat_composite_75,
             `ACT composite 25th percentile`=act_composite_25,
             `ACT composite 75th percentile`=act_composite_75,
             `net price`=net_price,
             region=Region,
             `full time students`=fulltime,
             `part time students`=parttime,
             `percent international students`=international,
             `percent students of color`=SOC,
             `percent female`= female,
             `retention rate`= retention,
             `graduation rate`=graduation,
             `endowment (in millions)`=endowment
             )
    data2
  }))
  
}


                              


shinyApp(ui = ui, server = server)