# app.R - High Speed Rail Explorer
# Run this file to launch the Shiny app

# Load libraries
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(ggiraph)
library(htmlwidgets)
library(gganimate)
library(ggthemes)
library(hrbrthemes)
library(rmarkdown)
library(forcats)

# -------------------- Data Loading & Preparation --------------------
# Load your cleaned data
hsr <- read.csv("hsr.csv") %>%
  select(Line, Length_km, Opening_year, country, Status, Maximum.speed)

hsr_2026 <- hsr %>% 
  filter(Opening_year<=2026) %>% 
  group_by(country) %>% 
  summarise(total_km = sum(Length_km))


hsr_2000 <- hsr %>% 
  filter(Opening_year<=2000) %>% 
  group_by(country) %>% 
  summarise(total_km = sum(Length_km))

hsr_2000 <- hsr_2000 %>%
  mutate(
    country = as.character(country),
    is_us = country == "United_States")

# Prepare summary data for the app
hsr_summary <- hsr %>%
  group_by(country) %>%
  summarise(
    Total_HSR_km = sum(Length_km, na.rm = TRUE),
    Number_of_Lines = n(),
    Avg_Line_Length = mean(Length_km, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_HSR_km))

# Data for time trend (cumulative HSR over years)
hsr_trend <- hsr %>%
  filter(!is.na(Opening_year)) %>%
  group_by(Opening_year, country) %>%
  summarise(Yearly_km = sum(Length_km, na.rm = TRUE), .groups = "drop") %>%
  complete(Opening_year = seq(min(Opening_year, na.rm = TRUE), 2037, by = 1), 
           country = unique(country), fill = list(Yearly_km = 0)) %>%
  group_by(country) %>%
  arrange(Opening_year) %>%
  mutate(Cumulative_km = cumsum(Yearly_km))



# Question 1 --------------------------------------------------------------

# -------------------- UI --------------------
ui <- fluidPage(
  titlePanel("Global High Speed Rail (HSR) Explorer"),
    tabPanel("Country Rankings",
             sidebarLayout(
               sidebarPanel(
                 h4("Filter Data"),
                 sliderInput("year_filter", "Select Year:",
                             min = 1990,
                             max = 2030,
                             step = 1,
                             value = 2025),  # Single year, not a range
                 hr()
               ),
               mainPanel(
                 h3("Total HSR Kilometer by Country"),
                 plotlyOutput("rank_plot", height = "600px"),
                 br()
               )
             )
    )
  )

# -------------------- Server --------------------
server <- function(input, output, session) {
  
  # Reactive data for single year filter - cumulative up to that year
  filtered_data <- reactive({
    year <- input$year_filter
    hsr %>%
      filter(Opening_year <= year) %>%  # All lines opened ON or BEFORE selected year
      group_by(country) %>%
      summarise(total_km = sum(Length_km, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_km)) %>%
      mutate(rank = row_number())
  })
  
  # Plot 1: Rankings
  output$rank_plot <- renderPlotly({
    
    df <- filtered_data() %>%
      mutate(
        country = fct_reorder(country, total_km),
        is_us = country == "United_States"
      )
    
    p <- ggplot(df, aes(
      x = total_km,
      y = country,
      fill = is_us
    )) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey85")) +
      guides(fill = "none") +
      theme_minimal() +
      labs(
        title = paste("Total Operational High Speed Rail Per Country in", input$year_filter),
        x = "Total HSR in Km",
        y = ""
      )
    
    ggplotly(p)
    
  })
  
  # Rankings
  output$rank_table <- renderDT({
    df <- filtered_data() %>%
      mutate(total_km = round(total_km, 0)) %>%
      select(Rank = rank, Country = country, `Total HSR (km)` = total_km)
    datatable(df, options = list(pageLength = 15, dom = 'tip'))
  })
  
  # Trend data reactive
  trend_data <- reactive({
    req(input$countries)
    hsr_trend %>%
      filter(country %in% input$countries)
  })
}

# Run the app
shinyApp(ui = ui, server = server)