library(shinydashboard)

source('read_metadata.R')

dataset <- list.dirs(
  path = 'data',
  full.names = TRUE,
  recursive = TRUE
) %>% 
  magrittr::extract(-1) %>%  ## Remove the 'data' directory itself
  purrr::map_dfr(load_csv_using_yaml)


# -------- UI --------------
ui <- dashboardPage(
  dashboardHeader(title = "Finance App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analysis", tabName = "analysis", icon = icon("dashboard")),
      menuItem("Trend", tabName = "trend", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "analysis",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "trend",
              h2("Trend content")
      )
    )
  )
)



# -------- SERVER --------------

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)