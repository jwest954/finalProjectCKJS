

library(shiny)
library(tidyverse)
library(plotly)
library(readr)
library(readxl)

Endowments <- read_csv("Endowments.csv")

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

ui <- fluidPage(
  titlePanel("College Comparison"),
  sliderInput(inputId = "year_range", label = "Year Range", 
              min = 2008, max=2017, value=c(2008,2017), sep = ""),
  selectInput("College1", "College 1:", 
              choices=unique(tidy_Endowments$College)),
  selectInput("College2", "College 2:", 
              choices=unique(tidy_Endowments$College)),
  helpText("These plots show the changes in the selected colleges' rankings for the selected range."),
  #submitButton(text = "Create my plot"),
  tabsetPanel(type = "tabs",
              tabPanel("Endowments", plotlyOutput(outputId = "plot1")),
              tabPanel("Ranking", plotlyOutput(outputId = "plot2"))))
  

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
  
}


shinyApp(ui = ui, server = server)