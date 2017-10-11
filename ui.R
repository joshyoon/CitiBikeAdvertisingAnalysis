 shinyUI(dashboardPage(
  
  dashboardHeader(title= "Citibike Advertising Opportunities"),
  
  dashboardSidebar(
    sidebarUserPanel(
      name= "Josh Yoon",
      image= "https://media.licdn.com/mpr/mpr/shrinknp_400_400/AAEAAQAAAAAAAAlFAAAAJGYyMDIzYWM1LTFiNTUtNGNiZi1hY2VmLTNiYjZhYTFhNzgxNA.jpg"
      ),
    sidebarMenu(
      menuItem("Introduction", tabName= "intro",icon =icon("book")),
      menuItem("Map By Gender", tabName = "map1",icon = icon("map")),
      menuItem("Map By Age", tabName = "map2",icon = icon("map")),
      menuItem("Time & Date", tabName= "time",icon =icon("user-circle-o"))
      )
    ),
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .user-panel>.info{
        background: rgba(0,0,0,0);
                  }
                    '))),
    tabItems(
      tabItem(tabName = "map1",
              fluidRow(
                box(radioButtons("gendermapid", "Choose Gender", c("Male","Female","Both"), selected=NULL),title="Gender Select", status = "primary", solidHeader = TRUE,
                    width=3),box(leafletOutput("gendermap"), width= 7,title = "Map By Gender", background = "black", solidHeader = TRUE, br(), "*Top 10 Stations Shown According to User Input")
              ),
              fluidRow(
                box(plotlyOutput("genderChart"), title="Male Vs Female Users", status = "primary", solidHeader = TRUE,
                    width=6),
                box(dataTableOutput("Top10Gender"), title="Top 10 Stations", status = "primary", solidHeader = TRUE,
                    width=4)
              )
              
      ),
      tabItem(tabName = "map2",
              fluidRow(
                box(selectInput("agerangeid", "Choose Age Group", c("All",unique(sort(ageRangeVector))), selected="All"), title="Age Range Select", status = "success", solidHeader = TRUE,
                    width=3), box(leafletOutput("agemap"), width= 7,title = "Map By Age Range", background = "black", solidHeader = TRUE, br(), "*Top 10 Stations Shown According to User Input")
              ),
              fluidRow(
                box(plotlyOutput("ageRangeChart"), title="Percentage of Users by Age Range", status = "success", solidHeader = TRUE,
                    width=6),
                box(dataTableOutput("Top10Age"), title="Top 10 Stations", status = "success", solidHeader = TRUE,
                    width=4)
              )
        ),
      tabItem(tabName = "time",
              fluidRow(
                box(width=3, title="User Select Options", status = "success", solidHeader = TRUE, radioButtons("genderid", "Choose Gender", c("Male","Female"), selected="Male"), sliderInput("ageid", "Choose Age Range", min= 0,max=100, value=c(0,100)))
              ),
              fluidRow(
                box(title="Riders By Hour", status = "danger", solidHeader = TRUE, plotlyOutput("hourChart")), box(title="Most Popular Day of the Week", status = "primary", solidHeader = TRUE, plotlyOutput("dayChart"))
              ),
              fluidRow(
                infoBoxOutput("bestHourBox", width =3),
                infoBoxOutput("bestDayBox", width =3)
              )
        )
    )
  )
))


 