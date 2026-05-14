

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
library(gganimate)
library(tidytext)
library(gifski)

#Data Loading--------------------
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

# cumulative HSR over years
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


# question 2 --------------------------------------------------------------
library(tidyverse)
library(gifski)
library(zoo)  

# Read and prepare data
hsr <- read.csv("hsr.csv") %>%
  select(Line, Length_km, Opening_year, country)

years <- expand.grid(
  Opening_year = 1990:2030,
  country = unique(hsr$country)
)

hsr_anim <- years %>%
  left_join(
    hsr %>%
      group_by(Opening_year, country) %>%
      summarise(km = sum(Length_km, na.rm = TRUE), .groups = "drop"),
    by = c("Opening_year", "country")
  ) %>%
  mutate(km = replace_na(km, 0)) %>%
  arrange(country, Opening_year) %>%
  group_by(country) %>%
  mutate(cumulative_km = cumsum(km)) %>%
  ungroup()

hsr_smoothed <- hsr_anim %>%
  group_by(country) %>%
  mutate(
    # 3-year centered moving average (adjust width for more/less smoothing)
    smoothed_km = rollmean(cumulative_km, k = 3, fill = NA, align = "center"),
    # Fill NA values at edges with original values
    smoothed_km = ifelse(is.na(smoothed_km), cumulative_km, smoothed_km),
    # Optional: add linear interpolation for extra smoothness
    smoothed_km = na.approx(smoothed_km, rule = 2)
  ) %>%
  ungroup()

# Get all unique years
all_years <- unique(hsr_smoothed$Opening_year) %>% sort()

# Create a folder for frames
if(!dir.exists("frames")) dir.create("frames")

for(i in seq_along(all_years)) {
  
  current_year <- all_years[i]
  
  # Filter and order data for this year using smoothed values
  frame_data <- hsr_smoothed %>%
    filter(Opening_year == current_year) %>%
    arrange(desc(smoothed_km)) %>%
    head(15) %>%  # Top 15 countries
    mutate(country = factor(country, levels = rev(unique(country))))
  
  # Create the plot
  p <- ggplot(frame_data, 
              aes(x = smoothed_km, y = country, fill = country)) +
    geom_col(show.legend = FALSE, width = 0.7) +
    geom_text(aes(label = country), 
              hjust = -0.1, size = 4.5, fontface = "bold") +
    geom_text(aes(label = paste0(round(smoothed_km, 0), " km"), 
                  x = smoothed_km - max(smoothed_km) * 0.02), 
              hjust = 1, size = 3.5, color = "white", fontface = "bold") +
    labs(
      title = "Global High Speed Rail Expansion",
      subtitle = paste("Year:", current_year),
      x = "Cumulative HSR (km)",
      y = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16, color = "gray40"),
      axis.text.y = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))
  
  # Save frame
  ggsave(paste0("frames/frame_", sprintf("%03d", i), ".png"), 
         p, width = 10, height = 7, dpi = 100)
  
  # Progress check
  if(i %% 5 == 0) cat("Rendered year", current_year, "(", i, "/", length(all_years), ")\n")
}

# Combine all frames into GIF
png_files <- list.files("frames", pattern = "frame_.*\\.png$", full.names = TRUE) %>% sort()

gifski(
  png_files,
  gif_file = "hsr_race_chart.gif",
  width = 1000,
  height = 700,
  delay = 0.2,
  loop = TRUE,
  progress = TRUE
)