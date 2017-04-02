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
library(jsonlite)

#set the color for different offense type

legendname<-c("Assult",
              "Possessing Controlled Substance",
              "Inchoate Crimes",
              "Theft",
              "Riot and Related Offenses",
              "Arson, Criminal Mischef And Other Property Destructions",
              "Burglary",
              "Falsification and Intimidation",
              "Robbery",
              "Firearms and Other Dangerous Articles",
              "Offenses Against the Family",
              "Sexual Offenses")

pal <- colorFactor(c("red","blue","orange","purple","turquoise","magenta",
                     "gray","green","violet","brown","white","pink"),
                   domain = c("Assult",
                              "Possessing Controlled Substance",
                              "Inchoate Crimes",
                              "Theft",
                              "Riot and Related Offenses",
                              "Arson, Criminal Mischef And Other Property Destructions",
                              "Burglary",
                              "Falsification and Intimidation",
                              "Robbery",
                              "Firearms and Other Dangerous Articles",
                              "Offenses Against the Family",
                              "Sexual Offenses"))


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
                                               label = "Crime and Offense Types",width = 300,
                                               choices = list("Assult",
                                                              "Possessing Controlled Substance",
                                                              "Inchoate Crimes",
                                                              "Theft",
                                                              "Riot and Related Offenses",
                                                              "Arson, Criminal Mischef And Other Property Destructions",
                                                              "Burglary",
                                                              "Falsification and Intimidation",
                                                              "Robbery",
                                                              "Firearms and Other Dangerous Articles",
                                                              "Offenses Against the Family",
                                                              "Sexual Offenses"),
                                               selected = c("Possessing Controlled Substance"),
                                               multiple = T),
                                                 
                                   # create a date-range-input widget
                                   dateRangeInput('dates', label = "Date Range",width = 300,
                                                  start = '2016-01-01', end = '2017-03-31',
                                                  min = "2016-01-01", max = "2017-03-31"
                                   ),
                                   # create a select-input widget for day of week selection
                                   selectizeInput('day_of_week','Days of Week', width = 300,
                                                  choices = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                                                  selected = c('Monday'),
                                                  multiple = T),
                                   # create a slder-input widget for time of day selection
                                   sliderInput('time_of_day','Time of Day', min = 0, max = 23,width = 300,
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
      minidata %>%
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
        popup = ~content)%>%
        addLegend(position = "bottomright",
                              pal = pal,values = legendname
          )
      
  })
}

shinyApp(ui,server)




