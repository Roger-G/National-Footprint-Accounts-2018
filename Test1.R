library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)
library(janitor)
library(gridExtra)
elements<-read.csv(file.path("/Users/gaojie/R/Assignment","NFA 2018.csv"))
element1=select(elements,country,UN_region,year,population,total,carbon,Percapita.GDP..2010.USD.)

#print(element1)
## Data preparation
## Data preparation
element2<-(element1 %>%
             group_by(country,year,population)%>%
             summarise(total_mean=(mean(total)),carbon=mean(carbon)))

element3 = aggregate(cbind(carbon,population) ~ UN_region+year, element1, FUN=sum)

#print(element3)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "region",
        label = "Choose the two objects to compare",
        choices = list("Country and CO2 commision","Continent and CO2 commision"),
        selected = "Country and CO2 commision"
      ),
      conditionalPanel(
        condition = "input.region=='Country and CO2 commision'",
        selectInput("countries1","Country1",choices = levels(elements$country),selected = "China"),
        selectInput("countries2", "Country2",choices = levels((elements$country)),selected = "Africa"),
        #checkboxInput("fit", "Add line of best fit", FALSE),
        fluidRow(
          column(5,radioButtons("colour", "Country1",
                                choices = c("blue", "red", "green", "yellow"),selected = 'blue')),
          column(5,radioButtons("colour1", "Country2",
                                choices = c("blue", "red", "green", "pink"),selected = 'red'))
        )
      ),
      conditionalPanel(
        condition = "input.region=='Continent and CO2 commision'",
        selectInput("continent1","Continent1",choices = levels(elements$UN_region),selected = "Asia"),
        selectInput("continent2", "Continent2",choices = levels((elements$UN_region))),
        #checkboxInput("fit", "Add line of best fit", FALSE),
        fluidRow(
          column(5,radioButtons("colour", "Continent1",
                                choices = c("blue", "red", "green", "yellow"),selected = 'yellow')),
          column(5,radioButtons("colour1", "Continent2",
                                choices = c("blue", "red", "green", "pink"),selected = 'green'))
        )
      ),
      
      # numericInput("size", "Point size", 1, 1),
      sliderInput("years","Years",min(elements$year),max(elements$year),value = c(1970,2001))
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",plotlyOutput("plot")),
        tabPanel("Plot2",plotlyOutput("test")),
        tabPanel('Plot3',plotlyOutput("plot2")),
        tabPanel('World CO2 commsion map',plotlyOutput("map"))
                 )
      )
      # plotlyOutput("plot"),
      # plotlyOutput("test"),
      # plotlyOutput("plot2")
    )
  )


server <- function(input, output, session) {
  
  Data <- reactive({
    box1<-subset(element3,year>=input$years[1] & year<=input$years[2])
    
    if (input$region=="Country and CO2 commision"){
      data1<-subset(element2,country %in% input$countries1 & year>=input$years[1] & year<=input$years[2])
      data2<-subset(element2,country %in% input$countries2 & year>=input$years[1] & year<=input$years[2])
    }
    else if (input$region=="Continent and CO2 commision"){
      data1<-subset(element3,UN_region %in% input$continent1 & year>=input$years[1] & year<=input$years[2])
      # 
      # print(data1)
      # print(data2)
    }
  })
  
  
  output$plot<-renderPlotly({
    if (input$region=="Country and CO2 commision"){
      data1<-subset(element2,country %in% input$countries1 & year>=input$years[1] & year<=input$years[2])
      data2<-subset(element2,country %in% input$countries2 & year>=input$years[1] & year<=input$years[2])
    }
    else if (input$region=="Continent and CO2 commision"){
      data1<-subset(element3,UN_region %in% input$continent1 & year>=input$years[1] & year<=input$years[2])
      data2<-subset(element3, UN_region%in% input$continent2 & year>=input$years[1] & year<=input$years[2])
    }
    # data=curData()
    # plot_ly(curData(),x=~year,y=~carbon,name = 'nothing',type = 'box')
    data1<-as.data.frame(data1)
    data2<-as.data.frame(data2)
    p<-ggplot(data1,aes(x=year,y=carbon/1000000))+
      geom_line(data=data1,aes(x=year,y=carbon/1000000),size = 1,col = input$colour) +
      geom_line(data=data2,aes(x=year,y=carbon/1000000),size = 1,col = input$colour1) +
      ylab('Total Carbon Emission / M') +
      ggtitle(input$title)
    p
  })
  
  Data1<-reactive({
    subset(element3,year>=input$years[1] & year<=input$years[2])
  })
  output$test<-renderPlotly({
    
    box1<-Data1()
    p<-plot_ly(box1,x=~UN_region,y=~carbon,name = 'nothing',type = 'box')%>%
      layout(title=sprintf("From %g to %g",input$years[1],input$years[2]))
    p
    # add_trace(data2x=~year,y=~carbon,mode='point')
  })
  output$plot2<-renderPlotly({
    
    # box1<-subset(element3[element3$year>=input$years[1]|element3$year<=input$years[2],])
    box1=Data1()
    
    (ggplot(box1 %>% filter(UN_region != 'World'), aes(year, carbon)) + geom_area(aes(fill=UN_region), alpha=0.5) + ylab('Total Carbon Emissions') + ggtitle(sprintf("From %g to %g",input$years[1],input$years[2]))) %>%
      ggplotly()
  })
  output$plot2<-renderPlotly({
    
    # box1<-subset(element3[element3$year>=input$years[1]|element3$year<=input$years[2],])
    box1=Data1()
    
    (ggplot(box1 %>% filter(UN_region != 'World'), aes(year, carbon)) + geom_area(aes(fill=UN_region), alpha=0.5) + ylab('Total Carbon Emissions') + ggtitle(sprintf("From %g to %g",input$years[1],input$years[2]))) %>%
      ggplotly()
  })
  output$map<-renderPlotly({
    
    # specify some map projection/options
    g <- list(
      scope = 'world',
      projection = list(type = 'natural earth'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    # create our plot
    plot_geo(element2, locationmode = 'country names') %>%
      add_trace(
        z = ~carbon, 
        locations = ~country,
        color = ~carbon,
        colors = "Blues"   
      ) %>%
      layout(
        title = 'Mean Carbon Emissions on the world',
        geo = g
      )
  }
  )
}
shinyApp(ui, server)