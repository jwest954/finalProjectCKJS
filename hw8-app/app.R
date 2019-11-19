

library(shiny)
library(tidyverse)
library(plotly)

Endowments <- read_csv("Endowments.csv")

tidy_Endowments <- Endowments %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year", 
               values_to = "endowment", names_ptypes = list(year=integer())) 

ui <- fluidPage(
  titlePanel("College Endowments"),
  sliderInput(inputId = "year_range", label = "Year Range", 
              min = 2008, max=2017, value=c(2008,2017), sep = ""),
  selectInput("College1", "College 1:", 
              choices=unique(tidy_Endowments$College)),
  selectInput("College2", "College 2:", 
              choices=unique(tidy_Endowments$College)),
  helpText("This plot will show the changes in the selected colleges' endowment for the selected range."),
  #submitButton(text = "Create my plot"),
  tabsetPanel(type = "tabs",
              tabPanel("Plot", plotOutput(outputId = "plot")),
              tabPanel("Plot2", plotlyOutput(outputId = "plot2"))
              ))
  

server <- function(input, output) {
  output$plot<- renderPlot({
    tidy_Endowments %>% 
      filter(College ==input$College1 | College ==input$College2) %>% 
      ggplot(aes(x=year, y=endowment, color=College))+
      geom_line()+
      scale_x_continuous(limits = input$year_range)+
      coord_cartesian(ylim = c(0, 3000))
  })
  output$plot2 <- renderPlotly({print(ggplotly(tidy_Endowments %>% 
                                         filter(College ==input$College1 | College ==input$College2) %>% 
                                         ggplot(aes(x=year, y=endowment, color=College))+
                                         geom_line()+
                                         scale_x_continuous(limits = input$year_range)+
                                         coord_cartesian(ylim = c(0, 3000))))})
}
shinyApp(ui = ui, server = server)