shinyUI(fluidPage(
  ###################### first row starts here
  fluidRow(
    column(12,
           # Application title takes all 12 column spaces
           titlePanel("Overview on Arrest Data")
    )),
  
  ###################### 2nd row starts here. 2 columns 
  fluidRow(
    column(3,
           wellPanel(
             selectizeInput("myWeekdays", 
                            label = "Weekdays",
                            choices = levels(heatdat$Weekday), 
                            selected = "Sunday",multiple = T),
             
             selectInput("myDistricts", 
                         label = "Council Districts",
                         choices = levels(heatdat$COUNCIL_DISTRICT), 
                         selected = "3")
           )
    ),
    ###################### 3rd row starts here. 2 columns
    fluidRow(
      column(5, 
             plotOutput("myPlot",height = "550px",width="1000px")
      )
    )
  )
))
