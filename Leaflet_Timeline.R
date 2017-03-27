library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
table(year(minidat$ARRESTTIME))
#2014 2016 2017 
#3 2347  380
# We ignore the arrest happends during 2014
data_a1<-select(minidat,Lon=X,Lat=Y,Date=ARRESTTIME)
date<-data_a1$Date
date<-date[year(date)>2015]
date<-as.Date(date)
qplot(as.character(date))+geom_bar()

data_a1<-filter(data_a1,year(date)>2015)
data_a1$Date<-as.Date(data_a1$Date)

#shinyapp:
  ui <- fluidPage(
    sliderInput("time", "date",min(date), 
                max(date),
                value = max(date),
                step=1,
                animate=T),
    leafletOutput("mymap")
  )

server <- function(input, output, session) {
  points <- reactive({
    data_a1 %>% 
      filter(Date==input$time)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addCircleMarkers(data=points(), radius = 6,
                       color = "red",
                       stroke = FALSE, fillOpacity = 0.2)%>%
    setView(lng = -79.995888, lat = 40.440624, zoom = 10)
  })
}

shinyApp(ui, server)
