library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title","Title","Country VS Carbon dioxide commision"),
      selectInput("countries","Country",choices = levels(elements$country),selected = "Japan"),
      checkboxInput("fit", "Add line of best fit", FALSE),
      radioButtons("colour", "Point colour",
                   choices = c("blue", "red", "green", "black")),
      numericInput("size", "Point size", 1, 1),
      sliderInput("years","Years",min(elements$year),max(elements$year),value = c(1995,2001))
      ),
    mainPanel(
      plotlyOutput("plot")
    )
    
  )
)

server <- function(input, output, session) {
  
  output$plot <-renderPlotly({
    elements<-read.csv(file.path("/Users/gaojie/R/Assignment","NFA 2018.csv"))
    element1=select(elements,country,year,population,total,carbon,Percapita.GDP..2010.USD.)
    element2<-(element1 %>%
                 group_by(country,year,population)%>%
                 summarise(total_mean=(mean(total)),collapse=',',carbon_mean=mean(carbon),per_gdp_mean=mean(Percapita.GDP..2010.USD.)))
    data<-subset(element2,country %in% input$countries & year>=input$years[1] & year<=input$years[2])
    data<-as.data.frame(data)
    p<-ggplot(data,aes(year,carbon_mean))+
      geom_point(size = input$size, col = input$colour) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "loess")
    }
    
    p
  })

}
shinyApp(ui, server)