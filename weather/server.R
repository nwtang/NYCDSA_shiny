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

# Select just cities in USA and Canada for plotting
city <- read.csv(file="./city_attributes.csv")
city <- city[(city$Country=="United States"),]
usa_can_cities <- unlist(lapply(city['City'],as.character))

# Read in preprocessed data (temperature, descriptions, wind direction and speed)
temp_sub <- read.csv(file="./temp_sub.csv")
pal_temp <- colorNumeric(palette = c("#6600ff","yellow","#cc0000"), domain = c(10,105))
pal_O3 <- colorNumeric(palette = c("#6600ff","yellow","#cc0000"), domain = c(0,70))
pal_NO2 <- colorNumeric(palette = c("#6600ff","yellow","#cc0000"), domain = c(0,35))

# Server function begins here
shinyServer(function(input, output, session) {
  avg_per <- reactive({input$avg_period})
  time_per <- reactive({if (avg_per()=="Annual") {
    input$year_period 
    } else if (avg_per()=="Monthly") {
      input$month_period
    } else {
      "All"
    }
    })
  start_hour <- reactive({input$hrofday[1]})
  end_hour <- reactive({input$hrofday[2]})
  select_city <- reactive({input$map_marker_click$id})
  output$cityBox <- renderUI({
    if (is.null(select_city())) {
      infoBox("Selected City","None - select from map",icon=icon("city"),fill=TRUE,width=12)
    } else {
      infoBox("Selected City",select_city(),icon=icon("city"),fill=TRUE,width=12)
    }
  })
  output$climateBox <- renderUI({
    if (is.null(select_city())) {
      infoBox("Climate Description","None - select from map",icon=icon("sun"),fill=TRUE,width=12)
    } else {
      infoBox("Climate Description",select(filter(city,City==select_city()),Climate),icon=icon("sun"),fill=TRUE,width=12)
    }
  })
  temp_avg <- reactive({
    if (avg_per()=="Annual") {
      temp_sub %>% filter(Year==time_per() & HrofDay >= start_hour() & HrofDay <= end_hour()) %>% group_by(City) %>%
        summarize(Temp_avg=mean(Temp,na.rm=TRUE),O3_avg=mean(O3,na.rm=TRUE),NO2_avg=mean(NO2,na.rm=TRUE)) %>% 
        merge(city,by="City")
    } else if (avg_per()=="Monthly") {
      temp_sub %>% filter(grepl(as.character(time_per()),Month) & HrofDay >= start_hour() & HrofDay <= end_hour()) %>%
        group_by(City) %>% summarize(Temp_avg=mean(Temp,na.rm=TRUE),O3_avg=mean(O3,na.rm=TRUE),
                                     NO2_avg=mean(NO2,na.rm=TRUE)) %>% merge(city,by="City")
    } else {
      temp_sub %>% filter(HrofDay >= start_hour() & HrofDay <= end_hour()) %>% group_by(City) %>% 
        summarize(Temp_avg=mean(Temp,na.rm=TRUE),O3_avg=mean(O3,na.rm=TRUE),NO2_avg=mean(NO2,na.rm=TRUE)) %>% 
        merge(city,by="City")
    }
  })
  temp_city <- reactive({
    if (avg_per()=="Annual") {
      temp_sub %>% 
        filter(Year==time_per() & HrofDay >= start_hour() & HrofDay <= end_hour() & 
                 grepl(as.character(select_city()),City))
    } else if (avg_per()=="Monthly") {
      temp_sub %>% filter(grepl(as.character(time_per()),Month) & HrofDay >= start_hour() & HrofDay <= end_hour() &
                            grepl(as.character(select_city()),City))
    } else {
      temp_sub %>% filter(HrofDay >= start_hour() & HrofDay <= end_hour() & 
                            grepl(as.character(select_city()),City))
    }
  })
  output$countBox <- renderUI({
    if (is.null(select_city())) {
      infoBox("Number of Observations",0,icon=icon("th-list"),fill=TRUE,width=12)
    } else {
      infoBox("Number of Observations",nrow(temp_city()),icon=icon("th-list"),fill=TRUE,width=12)
    }
  })
  output$completeBox <- renderUI({
    if (is.null(select_city())) {
      infoBox("Percent Completeness","N/A",icon=icon("percent"),fill=TRUE,width=12)
    } else {
      if (avg_per()=="Annual") {
        infoBox("Percent Completeness",round(100*nrow(temp_city())/(365*(end_hour()-start_hour()+1)),1),
                icon=icon("percent"),fill=TRUE,width=12)
      } else if (avg_per()=="Monthly") {
          if (time_per()=="February") {
            infoBox("Percent Completeness",round(100*nrow(temp_city())/(28*(end_hour()-start_hour()+1)*5),1),
                    icon=icon("percent"),fill=TRUE,width=12)
          } else if (time_per() %in% c("April","June","September","November")) {
            infoBox("Percent Completeness",round(100*nrow(temp_city())/(30*(end_hour()-start_hour()+1)*5),1),
                    icon=icon("percent"),fill=TRUE,width=12)
          } else {
            infoBox("Percent Completeness",round(100*nrow(temp_city())/(31*(end_hour()-start_hour()+1)*5),1),
                    icon=icon("percent"),fill=TRUE,width=12)
          }
      } else {
        infoBox("Percent Completeness",round(100*nrow(temp_city())/(365*(end_hour()-start_hour()+1)*5),1),
                icon=icon("percent"),fill=TRUE,width=12)
      }
    }    
  })
  output$map <- renderLeaflet({
    switch(input$pol,
           temp = leaflet(data=temp_avg()) %>% addTiles() %>%
             addCircleMarkers(~Longitude,~Latitude,color=~pal_temp(Temp_avg),label=~round(Temp_avg,digits=0),
                              labelOptions = labelOptions(noHide = TRUE,textOnly=TRUE,direction="center"),
                              popup=~City,layerId=~City),
           O3 = leaflet(data=temp_avg()) %>% addTiles() %>%
             addCircleMarkers(~Longitude,~Latitude,color=~pal_O3(O3_avg),label=~round(O3_avg,digits=0),
                              labelOptions = labelOptions(noHide = TRUE,textOnly=TRUE,direction="center"),
                              popup=~City,layerId=~City),
           NO2 = leaflet(data=temp_avg()) %>% addTiles() %>%
             addCircleMarkers(~Longitude,~Latitude,color=~pal_NO2(NO2_avg),label=~round(NO2_avg,digits=0),
                              labelOptions = labelOptions(noHide = TRUE,textOnly=TRUE,direction="center"),
                              popup=~City,layerId=~City))
    
  })
  observeEvent(input$map_marker_click, {
               selected_city<-input$map_marker_click$id
               })
  observeEvent(input$map_marker_click, {
    output$dist <- renderPlot({
      switch(input$pol,
             temp = subset(temp_city(),!is.na(Temp)) %>% 
               ggplot(aes(x=Temp,fill=factor(Group,levels=c("Snow","Rain","Fog","Cloudy","Clear")))) + 
               geom_histogram(binwidth=5,color="#333333",alpha=0.5) +
               guides(fill=guide_legend(title="Description")) + 
               scale_fill_manual(values=c("Snow"="#6600ff","Rain"="#0066ff","Fog"="#00ff99","Cloudy"="#ffff00",
                                          "Clear"="#ff6600")) + xlab("Temperature (deg F)"),
             O3 = subset(temp_city(),!is.na(O3)) %>% 
               ggplot(aes(x=O3,fill=factor(Group,levels=c("Snow","Rain","Fog","Cloudy","Clear")))) + 
               geom_histogram(binwidth=5,color="#333333",alpha=0.5) +
               guides(fill=guide_legend(title="Description")) + 
               scale_fill_manual(values=c("Snow"="#6600ff","Rain"="#0066ff","Fog"="#00ff99","Cloudy"="#ffff00",
                                          "Clear"="#ff6600")) + xlab("Ozone (ppb)"),
             NO2 = subset(temp_city(),!is.na(NO2)) %>% 
               ggplot(aes(x=NO2,fill=factor(Group,levels=c("Snow","Rain","Fog","Cloudy","Clear")))) + 
               geom_histogram(binwidth=5,color="#333333",alpha=0.5) +
               guides(fill=guide_legend(title="Description")) + 
               scale_fill_manual(values=c("Snow"="#6600ff","Rain"="#0066ff","Fog"="#00ff99","Cloudy"="#ffff00",
                                          "Clear"="#ff6600")) + xlab("NO2 (ppb)"))
    })
  })
  observeEvent(input$map_marker_click, {
    output$hexbin <- renderPlot({
      ggplot(data=temp_city(),aes(x=NO2,y=O3)) + stat_binhex(aes(color=..count..)) +
        theme(legend.position='none') + scale_fill_continuous(type="viridis")
    })
  })
  temp_city_wind <- reactive({drop_na(temp_city())})
  observeEvent(input$map_marker_click, {
    output$wrose <- renderPlot({
    windrose(temp_city_wind()$Wspd,temp_city_wind()$Wdir,n_directions=12,
             speed_cuts=c(1,3,5,8,12,20,50),col_pal="YlOrRd")
    })
  })
})
