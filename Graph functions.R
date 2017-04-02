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

#use ZIP Code to estimate crime rate
minidata$Zipcode<-sub('.*,\\s*','', minidata$INCIDENTLOCATION)
table(minidata$Zipcode)

#17 of the location without exact zipcode
minidata$INCIDENTLOCATION[minidata$Zipcode=="PA"]
#I add them by myself
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "800 Block RECTENWALD ST PITTSBURGH, PA"]<-"PA 15210"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "1100 Block N MURTLAND ST PITTSBURGH, PA"]<-"PA 15208"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "E Liberty BL & Centre AV Pittsburgh, PA"]<-"PA 15206"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "200 Block Forbes AV Pittsburgh, PA"]<-"PA 15213"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "Wyndotte ST & Deraud ST Pittsburgh, PA"]<-"PA 15219"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "300 Block Mt Pleasant RD Pittsburgh, PA"]<-"PA 15214"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "2300 Block Noblestown RD Pittsburgh, PA"]<-"PA 15205"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "3800 Block PERRYSVILLE AV PITTSBURGH, PA"]<-"PA 15205"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "Berry ST & Charlton ST Pittsburgh, PA"]<-"PA 15205"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "500 Block GREENFIELD AV PITTSBURGH, PA"]<-"PA 15207"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "Crafton BL & Ridenour AV Pittsburgh, PA"]<-"PA 15205"
minidata$Zipcode[minidata$INCIDENTLOCATION==
                   "800 Block Freeport RD Pittsburgh, PA"]<-"PA 15238"
#check it
table(minidata$Zipcode)
minidata<-filter(minidata,!Zipcode=="PA 12508")
table(minidata$Zipcode)

c<-(tapply(minidata$Zipcode,sort(minidata$Zipcode),length))
minidata<-minidata[order(minidata$Zipcode),]
a<-NULL
for(i in c){
  a<-c(a,rep(i,i))
}
minidata$ZipCount<-as.numeric(a)
table(minidata$ZipCount)

#use neighborhood to estimate crime rate
b<-(tapply(minidata$INCIDENTNEIGHBORHOOD,sort(minidata$INCIDENTNEIGHBORHOOD),length))
minidata<-minidata[order(minidata$INCIDENTNEIGHBORHOOD),]
e<-NULL
for(i in b){
  e<-c(e,rep(i,i))
}
minidata$NeighborCount<-as.numeric(e)
hist(minidata$NeighborCount)



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

pal2 <- colorFactor(c("green","black","orange","yellow","turquoise","magenta"),
                   domain = c("White","African America","Hispanic","Asian","Other","Unknow"))

pal3 <-colorFactor(c("red","blue","yellow"),
                  domain = c("Female","Male","Unknow"))

#add widgets to the map
header <- dashboardHeader(title = p("Crime and Offenses of Pittsburgh"), titleWidth = 400)
dashboard <- column(width =4,
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
                                       selected = c("Assult",
                                                    "Possessing Controlled Substance",
                                                    "Inchoate Crimes",
                                                    "Theft"),
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
                        #create a selet-input for gender
                        selectizeInput('gender','Gender', width = 300,
                                       choices = c('Male','Female','Unknow'),
                                       selected = c('Male'),
                                       multiple = T),
                        sliderInput('age', label = "Criminal Age",min=1,max=120,width = 300,
                                    value =c(0,1999) ,step=1),
                        selectizeInput('race','Race', width = 300,
                                       choices = c('African America','White','Other','Hispanic',
                                                   'Asian','Unknow'),
                                       selected = c('African America','White'),
                                       multiple = T),
                        # create a submit-button for user explicitly confirm data input
                        submitButton(text = "Submit",icon =icon('filter'))
                    )
)

map <- column(width =8,
              box(width = NULL, solidHeader = TRUE,
                  leafletOutput('crimeMap',height = 500)))

body <- dashboardBody(
  fluidRow(
    dashboard, map
  )
)
  
ui <- dashboardPage(header,
                       sidebar = dashboardSidebar(disable = T),
                       body)

server <- function(input, output) {
  # Create a reactive expression to filter data set per user requests
  filteredData <- reactive({
      minidata %>%
      filter(ShortCode %in% input$crimeType ) %>%
      filter(Date > input$dates[1] & Date < input$dates[2]) %>%
      filter(Weekday %in% input$day_of_week) %>%
      filter(Hours >= input$time_of_day[1] & Hours <= input$time_of_day[2])%>%
     filter(GENDER %in% input$gender)%>%
    filter(AGE >= input$age[1] & AGE <=input$age[2]) %>%
    filter(RACE %in% input$race)
  })
  # Use Leaflet to render crime map
  output$crimeMap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$CartoDB.Positron,group="Base Map") %>%
      addCircleMarkers(
        ~X,
        ~Y,
        radius = ~NeighborCount/30,
        color = ~pal(ShortCode),
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~content,group="By Offense Type")%>%
      addCircleMarkers(
        ~X,
        ~Y,
        radius = ~NeighborCount/30,
        color = ~pal2(RACE),
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~content,group="By Criminal Race")%>%
      addCircleMarkers(
        ~X,
        ~Y,
        radius = ~NeighborCount/30,
        color = ~pal3(GENDER),
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~content,group="By Criminal Gender")%>%
      addLayersControl(
        baseGroups = c("Base Map"),
        overlayGroups = c("By Offense Type", "By Criminal Race",
                          "By Criminal Gender"),
        options = layersControlOptions(collapsed = F)
      )%>%
        addLegend(position = "bottomright",
                              pal = pal,values = ~ShortCode
          )%>%
      addLegend(position = "bottomright",
                pal = pal2,values = ~RACE
      )%>%
      addLegend(position = "bottomright",
                pal = pal3,values = ~GENDER)
  })
}


shinyApp(ui,server)

