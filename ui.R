shinyUI(
  fluidPage(
    dashboardPage(
        dashboardHeader(
        title = "California Solar Apps", titleWidth = 230,
        tags$li("Paul Choi",
              style = "text-align: right; padding-right: 13px; padding-top:17px; font-family: Arial, Helvetica, sans-serif; font-weight: bold;  font-size: 13px;",
              class="dropdown"),
        tags$li(a(href = "https://www.linkedin.com/in/pchoi626/", img(src = "linkedin.png", title = "LinkedIn", height = "19px")),
              class = "dropdown"),
        tags$li(a(href = "https://github.com/pchoi72", img(src = "github.png", title = "Github", height = "20px")),
              class = "dropdown")
        ),
  
        dashboardSidebar(
          width = 230,
          sidebarUserPanel("Paul Choi", image = "paul.jpeg", subtitle = "NYC Data Science Fellow"),
          sidebarMenu(
            menuItem("Time-Series Charts", tabName = "TScharts", icon = icon("chart-line")),
            menuItem("Column Charts", tabName = "ColCharts", icon = icon("chart-bar")),
            menuItem("YoY Charts", tabName = "YoYCharts", icon = icon("percentage")),
            menuItem("County Comparison", tabName = "map", icon = icon("map")),
            menuItem("Data Table", tabName = "data", icon = icon("database"))
            )
        ),

        dashboardBody(
          shinyDashboardThemes(theme = "grey_light"),
          tabItems(
            tabItem(tabName = "TScharts",
                    titlePanel("California Solar Applications in MegaWatts (MW) by Segment"),
                    fluidRow(box(htmlOutput("chart1"), width = 12),
                             box(htmlOutput("chart2"), width = 12),
                             box(htmlOutput("chart3"), width = 12))
                    ),

            tabItem(tabName = "ColCharts",
                    titlePanel("Relative Comparison Charts by Type"),
                    fluidRow(box(htmlOutput("chart4"), width = 12),
                             box(htmlOutput("chart5"), width = 12),
                             box(htmlOutput("chart6"), width = 12))
                    ),
            
            tabItem(tabName = "YoYCharts",
                    titlePanel("Rate of Change by Ticker"),
                    fluidRow(box(htmlOutput("chart7"), width = 12),
                             box(htmlOutput("chart8"), width = 12),
                             box(htmlOutput("chart9"), width = 12),
                             box(htmlOutput("chart10"), width = 12),
                             box(htmlOutput("chart11"), width = 12),
                             box(htmlOutput("chart12"), width = 12),
                             box(htmlOutput("chart13"), width = 12))
                    ),
            
            tabItem(tabName = "map",
                    titlePanel("2019 Applications in Cumulative MWs by County"),
                    fluidRow(box(htmlOutput("map"), width = 6),
                             box(htmlOutput("pie"), width = 6))
                    ),

            tabItem(tabName = "data",
                    titlePanel("California Solar Applications Dataset"),
                    fluidRow(column(3, selectInput("util","Utility", c("All", unique(as.character(solar$Utility))))),
                             column(3, selectInput("inst", "Installer", c("All", unique(as.character(solar$Installer))))),
                             column(3, selectInput("sect", "Sector", c("All", unique(as.character(solar$Sector))))),
                             column(3, selectInput("cty", "County", c("All", unique(as.character(solar$County)))))
                             ),
                    DT::dataTableOutput("table")
                    )
            )
        )
    )
  )
)