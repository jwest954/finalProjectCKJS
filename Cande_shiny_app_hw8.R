library(shiny)
library(tidyverse)
library(readr)
library(rsconnect)

Endowments <- read_csv("Endowments.csv")


Endowments <- read_csv("Endowments.csv")

tidy_Endowments <- Endowments %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year", 
               values_to = "endowment", names_ptypes = list(year=integer())) 

ui <- fluidPage(
  titlePanel("College Endowment over the years"),
  sliderInput(inputId = "year_range", label = "Year Range", 
              min = 2008, max=2017, value=c(2008,2017), sep = ""),
  
  sidebarLayout(     
sidebarPanel(
    selectInput("College", "College:", 
                  choices=unique(tidy_Endowments$College)),
    helpText("Data of College endowment 2008-2017."),
    submitButton(text = "Create my plot")
  ),

mainPanel(plotOutput(outputId = "plot"))
  )
  )

server <- function(input, output) {
  output$plot<- renderPlot({
    tidy_Endowments %>% 
      filter(College ==input$College) %>% 
      ggplot(aes(x=year, y=endowment))+
      geom_line(color="red")+
      scale_x_continuous(limits = input$year_range)
  })
}
shinyApp(ui = ui, server = server)