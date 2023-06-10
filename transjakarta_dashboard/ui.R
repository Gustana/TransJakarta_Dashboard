dashboardPage(
  skin = "black",

  header <- dashboardHeader(
    title = "Transjakarta Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        text = "Detail",
        tabName = "detail",
        icon = icon("circle-info")
      ),
      menuItem(
        text = "Database",
        tabName = "database",
        icon = icon("database")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          width = 12,
          valueBoxOutput(outputId = "busiest_month", width = 4),
          valueBoxOutput(outputId = "busiest_route", width = 4),
          valueBoxOutput(outputId = "total_passengers", width = 4),
          valueBoxOutput(outputId = "num_of_routes", width = 4),
          valueBoxOutput(outputId = "num_of_transportation_type", width = 4),
          valueBoxOutput(outputId = "num_of_routes_with_no_passenger", width = 4),
        ),
        hr(),
        fluidRow(
          box(
            width = 6,
            h3("Routes List"),
            hr(),
            dataTableOutput(outputId = "table_routes_list")
          ),
          box(
            width = 6,
            h3("Routes with No Passengers"),
            hr(),
            selectInput(
              inputId = "dashboard_month_filter",
              label = "Select Month",
              width = "30%",
              choices = get_month_list()
            ),
            actionButton(
              inputId = "btn_reset_dashboard_month_filter",
              label = "Reset Filter",
              class = "btn-warning"
            ),
            hr(),
            dataTableOutput(outputId = "table_num_of_routes_with_no_passenger")
          ),
        ),
        hr(),
          
        fluidRow(
          box(
            width = 12,
            selectizeInput(
              inputId = "dashboard_route_filter",
              label = "Select Route",
              width = "30%",
              multiple = T,
              choices = levels(data$trayek)
            ),
            actionButton(
              inputId = "btn_reset_dashboard_route_filter", 
              label = "Reset Filter",
              class = "btn-warning"
              ),
            hr(),
            plotlyOutput(outputId = "passenger_volume_chart")
          ),
        )
      ),
      tabItem(
        tabName = "detail",
        
      fluidRow(
        column(
          width = 9,
          box(
            width = NULL,
            plotlyOutput(outputId = "top10_busiest_routes_chart"),
            hr(),
            plotlyOutput(outputId = "top5_least_busy_routes_chart")
          )
        ),
        column(
          width = 3,
          box(
            width = NULL,
            selectInput(
              inputId = "routes_month_filter",
              label = "Select Month",
              choices = get_month_list()
              ),
            actionButton(
              inputId = "btn_reset_transport_type_month_filter",
              label = "Reset Filter",
              class = "btn-warning"
              ),
            ),
          box(
            width = NULL,
            plotlyOutput(outputId = "transportation_type_proportion_chart")
            ),
          box(
            width = NULL,
            h4("List of Transportation Type"),
            dataTableOutput(outputId = "table_list_of_transportation_type")
            )
          ),
        ),
      ),
      tabItem(
        tabName = "database",
        fluidRow(
          box(
            width = 12,
            dataTableOutput(outputId = "database")
          )
        )
      )
    )
  )
)