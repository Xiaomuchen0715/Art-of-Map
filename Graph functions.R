#What do they have right now 
# https://public.tableau.com/profile/alleghenycountydhsdare#!/vizhome/UCRCityofPittsburgh2005-August2015/UCRDB
# https://pittsburghpa.shinyapps.io/BurghsEyeView/?_inputs_&dept_select=null&filter_select=%22%22&hier=null&map_bounds=%7B%22north%22%3A40.5696322335956%2C%22east%22%3A-79.5197296142578%2C%22south%22%3A40.2800495082348%2C%22west%22%3A-80.3986358642578%7D&map_center=%7B%22lng%22%3A-79.9594934304115%2C%22lat%22%3A40.4250801873534%7D&map_zoom=11&offense_select=null&origin_select=null&report_select=%22311%20Requests%22&req.type=null&result_select=null&search=%22%22&toggle311=true&toggleArrests=true&toggleAssets=true&toggleBlotter=true&toggleCitations=true&toggleViolations=true&usage_select=null&violation_select=null
install.packages("leaflet")
install.packages("shinydashboard")
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(shiny)
library(lubridate)
library(shinydashboard)
#Basic Graph 
minidat$ShortCode[minidat$ShortCode=="13(a)"]<-"Controlled Substance"
minidat$ShortCode[minidat$ShortCode=="27"]<-"Assult"
minidat$ShortCode[minidat$ShortCode=="9"]<-"Inchoate"
minidat$ShortCode[minidat$ShortCode=="39"]<-"Theft"
minidat$ShortCode[minidat$ShortCode=="55"]<-"Riot"
minidat$ShortCode[minidat$ShortCode=="51"]<-"Obstructing Officials"
minidat$ShortCode[minidat$ShortCode=="35"]<-"Burglary"
minidat$ShortCode[minidat$ShortCode=="37"]<-"Robbery"
minidat$ShortCode[minidat$ShortCode=="49"]<-"Falsification and Intimidation"
minidat$ShortCode[minidat$ShortCode=="43"]<-"Offenses Against the Family"
minidat$ShortCode[minidat$ShortCode=="61"]<-"Firearms"
minidat$ShortCode[minidat$ShortCode=="59"]<-"Public Indecency"

#rearrange data
minidat<-minidat%>%
  mutate(Weekday=wday(minidat$ARRESTTIME,label = TRUE, abbr = FALSE))%>%
  mutate(Hours=hour(minidat$ARRESTTIME))%>%
mutate(Date=as.Date(minidat$ARRESTTIME))%>%
mutate(incidentType=as.factor(minidat$ShortCode))%>%
mutate(content = paste("<b>Type of offense:</b>", "<b><font color=darkyellow>",minidat$ShortCode, "</b></font><br>",
                         "<b>Incident Location:</b>","<br>", 
                         "Adress:", minidat$INCIDENTLOCATION,"<br>",
                         "Neighborhood:", minidat$INCIDENTNEIGHBORHOOD, "<br>",
                         "<b>Suspect Profile:</b>", "<br",
                         "Race:", minidat$RACE, "<br>",
                         "Race:", minidat$RACE, "<br>",
                         "Gender:", minidat$GENDER, "<br>",
                         "Arrested at", minidat$ARRESTLOCATION, "at",minidat$ARRESTTIME))
#map
#The color system we use 
#Types
#Assult: Red
#13(a): Blue
#9: Orange
#39: Purple

#Race
#White:"White"
#Black:"Black"
#Other:"Gray"

#Age
table()

#set the color for different offense type
pal <- colorFactor(c("yellow","gray","red","green","orange","pink",
                     "brown","cyan","magenta","blue","pink","turquoise"), 
                   domain = c("Controlled Substance","Theft","Assult","Riot","Inchoate","Obstructing Officials",
                              "Burglary","Robbery","Falsification and Intimidation","Offenses Against the Family",
                              "Firearms","Public Indecency"))


#add widgets to the map
ui <- dashboardPage( dashboardHeader(title = p("Crimes and Offenses in City of Pittsburgh"), titleWidth = 400),
                    sidebar = dashboardSidebar(disable = T),
                    dashboardBody(
                      fluidRow(
                        dashboard<-column(width =4,
                               # width set to NULL when use column-based layout
                               box(width = NULL , title =tagList(shiny::icon("filter",class = 'fa-lg'), "Filter Data") ,
                                   solidHeader = T, collapsible = T, status = 'primary',
                                   # create a select-input widget for crime type selection
                                   selectizeInput('crimeType', 
                                               label = "Crime and Offense Types",width = 380,
                                               choices = list("Controlled Substance","Theft","Assult","Riot","Inchoate","Obstructing Officials",
                                                              "Burglary","Robbery","Falsification and Intimidation","Offenses Against the Family",
                                                              "Firearms","Public Indecency"),
                                               selected = c("Controlled Substance"),
                                               multiple = T),
                                                 
                                   # create a date-range-input widget
                                   dateRangeInput('dates', label = "Date Range",width = 380,
                                                  start = '2016-01-01', end = '2017-03-31',
                                                  min = "2016-01-01", max = "2017-03-31"
                                   ),
                                   # create a select-input widget for day of week selection
                                   selectizeInput('day_of_week','Days of Week', width = 380,
                                                  choices = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                                                  selected = c('Monday'),
                                                  multiple = T),
                                   # create a slder-input widget for time of day selection
                                   sliderInput('time_of_day','Time of Day', min = 0, max = 23,width = 380,
                                               value = c(0,23), step = 1),
                                   # create a submit-button for user explicitly confirm data input
                                   submitButton(text = "Submit",icon =icon('filter'))
                               )
                        )
                        , map <- column(width =8,
                                        box(width = NULL, solidHeader = TRUE,
                                            leafletOutput('crimeMap',height = 500)))
                      )
                    ))

server <- function(input, output) {
  # Create a reactive expression to filter data set per user requests
  filteredData <- reactive({
      minidat %>%
      filter(ShortCode %in% input$crimeType ) %>%
      filter(Date > input$dates[1] & Date < input$dates[2]) %>%
      filter(Weekday %in% input$day_of_week) %>%
      filter(Hours >= input$time_of_day[1] & Hours <= input$time_of_day[2])
  })
  # Use Leaflet to render crime map
  output$crimeMap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        ~X,
        ~Y,
        radius = 6,
        color = ~pal(ShortCode),
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~content
      )
  })
}

shinyApp(ui,server)




