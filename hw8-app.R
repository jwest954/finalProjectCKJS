

library(shiny)
library(tidyverse)

Endowments <- read_csv("Endowments.csv")

tidy_Endowments <- Endowments %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year", 
               values_to = "endowment", names_ptypes = list(year=integer())) 

ui <- fluidPage(
  sliderInput(inputId = "year_range", label = "Year Range", 
              min = 2008, max=2017, value=c(2008,2017), sep = ""),
  selectInput("College", "College:", 
              choices=unique(tidy_Endowments$College)),
  helpText("Data of College endowment 2008-2017."),
  submitButton(text = "Create my plot"),
  plotOutput(outputId = "plot"))

server <- function(input, output) {
  output$plot<- renderPlot({
    tidy_Endowments %>% 
      filter(College ==input$College) %>% 
      ggplot(aes(x=year, y=endowment))+
      geom_line()+
      scale_x_continuous(limits = input$year_range)
  })
}
shinyApp(ui = ui, server = server)