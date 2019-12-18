library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(leaflet)
library(lubridate)
library(RColorBrewer)
library(clifro)
library(hexbin)

city <- read.csv(file="./city_attributes.csv")
city <- city[(city$Country=="United States"),]
avg_period_choices <- c("5-year","Annual","Monthly")

years <- c(2013:2017)
months <- c("January","February","March","April","May","June",
            "July","August","September","October","November","December")

shinyUI(dashboardPage(
  dashboardHeader(title = "Weather & AQ App"),
  dashboardSidebar(
    selectizeInput(inputId="avg_period",
                   label="Averaging Period",
                   choices=avg_period_choices),
    conditionalPanel(
      condition = "input.avg_period=='Annual'",
      selectizeInput(inputId="year_period",
                     label="Time Period Selection",
                     choices=years)
    ),
    conditionalPanel(
      condition = "input.avg_period=='Monthly'",
      selectizeInput(inputId="month_period",
                     label="Time Period",
                     choices=months)
    ),
    sliderInput("hrofday",label=h5(strong("Hour of Day")),min=0,max=23,value=c(0,23)),
    radioButtons("pol","Map and Histogram Display:",
                 c("Temperature (deg F)" = "temp",
                   "Ozone (ppb)" = "O3",
                   "NO2 (ppb)" = "NO2"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.info-box {min-height: 76px;} .info-box-icon {height: 76px; line-height: 76px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    fluidRow(
      box(title="Statistics",status="info",solidHeader=TRUE,
          fluidRow(uiOutput("cityBox")),
          fluidRow(uiOutput("climateBox")),
          fluidRow(uiOutput("countBox")),
          fluidRow(uiOutput("completeBox"))
      ,width=4,height=420),
      box(leafletOutput("map"),width=8)
    ),
    fluidRow(
      box(title="Histogram of Weather Observations",status="primary",solidHeader=TRUE,plotOutput("dist"),width=4),
      box(title="O3 vs. NO2 Hexbin Plot",status="primary",solidHeader=TRUE,plotOutput("hexbin"),width=4),
      box(title="Wind Rose",status="primary",solidHeader=TRUE,plotOutput("wrose"),width=4)
    )
  )
)
)
