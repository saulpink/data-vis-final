## Load packages ----
library(tidyverse)
library(shiny)
library(here)
library(sf)

## Load data ----
death_causes <- read_csv(here("data/leading_causes_of_death.csv")) %>% 
  janitor::clean_names()

us_states_map <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
us_states_map$ID <- tolower(us_states_map$ID)
death_causes$state <- tolower(death_causes$state)

death_causes_map <- us_states_map %>%
  left_join(death_causes, by = c("ID" = "state"))

## UI ----
ui <- fluidPage(
  titlePanel("Death Causes in US States"),
  sidebarLayout(
    sidebarPanel(
      selectInput("causeOfDeath", "Cause of Death",
                  choices = unique(death_causes$cause_name)),
      selectInput("year", "Year",
                  choices = unique(death_causes$year),
                  selected = 2017),
      radioButtons("metric", "Metric",
                   choices = list("Age Adjusted Death Rate" = "age_adjusted_death_rate",
                                  "Death Count" = "deaths"),
                   selected = "age_adjusted_death_rate"),
      selectInput("state", "State",
                  choices = unique(death_causes$state))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotOutput("mapPlot")),
                  tabPanel("Trends", plotOutput("trendPlot")),
                  tabPanel("Analysis", verbatimTextOutput("analysisText"))
      )
    )
  )
)

## Server ----
server <- function(input, output) {
  # Map Plot
  output$mapPlot <- renderPlot({
    filtered_data <- death_causes_map %>%
      filter(year == input$year, cause_name == input$causeOfDeath) %>%
      ggplot() +
      geom_sf(aes_string(fill = input$metric), color = "white") +
      scale_fill_viridis_c() +
      labs(title = paste("Death Causes in US States -", input$causeOfDeath, "-", input$year),
           fill = "Metric") +
      theme_void()
    
    filtered_data
  })
  
  # Trend Plot
  output$trendPlot <- renderPlot({
    trend_data <- death_causes %>%
      filter(cause_name == input$causeOfDeath, state == input$state) %>%
      ggplot(aes(x = year, y = !!sym(input$metric), group = 1)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Trend of", input$metric, "for", input$causeOfDeath, "in", input$state),
           x = "Year",
           y = input$metric)
    
    trend_data
  })
  
  # Analysis Text
  output$analysisText <- renderText({
    paste("This section can be used to provide detailed analysis and takeaways based on the data visualized in the app. ",
          "You can discuss trends, notable findings, and any other insights derived from the visualization. ",
          "This might include observations on how specific causes of death have evolved over time, ",
          "regional differences, or the impact of public health policies.")
  })
}

## Run App ----
shinyApp(ui = ui, server = server)