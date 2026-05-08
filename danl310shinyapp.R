# app.R - High Speed Rail Explorer
# Run this file to launch the Shiny app

# Load libraries
library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# -------------------- Data Loading & Preparation --------------------
# Load your cleaned data (adjust path if needed)
# Assuming 'hsr.csv' is in the same folder as app.R
hsr <- read.csv("hsr.csv") %>%
  select(Line, Length_km, Opening_year, country, Status, Maximum.speed)

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
                 br(),
                 DTOutput("rank_table")
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
    df <- filtered_data()
    
    # Highlight US
    df$color <- ifelse(df$country == "United_States", "red", "steelblue")
    
    df <- df %>%
      mutate(
        country = fct_reorder(country, total_km)
      )
    
    p <- ggplot(df, aes(
      x = total_km,
      y = fct_reorder(country, total_km)
    )) +
      geom_col(aes(fill = country)) +
      scale_y_discrete(labels = function(x) {
        ifelse(x == "United_States", "United_States", x)
      }) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(
          color = ifelse(levels(fct_reorder(hsr_2000$country, hsr_2000$total_km)) == "United_States",
                         "red", "black"),
          face = ifelse(levels(fct_reorder(hsr_2000$country, hsr_2000$total_km)) == "United_States",
                        "bold", "plain")
        )
      ) +
      labs(
        title = "Total Operational High Speed Rail Per Country in 2000",
        y = "",
        x= "Total HSR in Km",
        subtitle = "Countries Ahead of the US: 13"
      )+
      scale_fill_viridis_d()
    
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Table 1: Rankings
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