 shinyUI(dashboardPage(
  
  dashboardHeader(title= "Citibike Advertising Opportunities"),
  
  dashboardSidebar(
    sidebarUserPanel(name="Josh Yoon",
                    image= "https://media.licdn.com/mpr/mpr/shrinknp_400_400/AAEAAQAAAAAAAAlFAAAAJGYyMDIzYWM1LTFiNTUtNGNiZi1hY2VmLTNiYjZhYTFhNzgxNA.jpg"),
    sidebarMenu(
      menuItem("Map By Gender", tabName = "map1",icon = icon("map")),
      menuItem("Map By Age", tabName = "map2",icon = icon("map")),
      menuItem("Data", tabName= "data",icon =icon("database")),
      menuItem("Time & Date", tabName= "time",icon =icon("user-circle-o"))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map1",
              fluidRow(
                infoBoxOutput("TBD1"),
                infoBoxOutput("TBD2"),
                infoBoxOutput("TBD3"),
                infoBoxOutput("TBD4"),
                infoBoxOutput("TBD5")
              ),
              fluidRow(
                box(leafletOutput("gendermap")),box(radioButtons("gendermapid", "Choose Gender", c("Male","Female","Both"), selected=NULL))
              )
      ),
      tabItem(tabName = "map2",
              fluidRow(
                box(leafletOutput("agemap")),box(selectInput("agerangeid", "Choose Age Group", c("All",unique(sort(ageRangeVector))), selected="All"))
              )
      ),
      tabItem(tabName = "time",
              fluidRow(
                infoBoxOutput("bestHourBox"),
                infoBoxOutput("bestDayBox")
              ),
              fluidRow(
                box(radioButtons("genderid", "Choose Gender", c("Male","Female"), selected="Male"), sliderInput("ageid", "Choose Age Range", min= 0,max=100, value=c(0,100)))
              ),
              fluidRow(
                box(plotlyOutput("hourChart")), (box(plotlyOutput("dayChart")))
              )
        )
    )
  )
))

#changes needed to Time Trends Tab
  #change column colors by selection
  #change axis labels
  # make it fit better.  widget should be shorter, graph is taller.
  # connect widge to backend to make reactive.  should be example code in practice ui/server
  # error when both values not clicked(read checkbBox documentation.)
  # make reactive and optimize for speed
  # info box outputs are not reactive! when you keep changing sliders numbers don't change
      # default when no inputs is there a default value w can have?
  #make reactive as in Shiny Topics lecture.  
  #save clean df as a rda.

 