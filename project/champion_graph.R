library(tidyverse)
library(xgboost)

# Manually fix DayNum for 2026 championship game
# Championship teams: WTeamID = 1276, LTeamID = 1163

# Find the current max DayNum for 2026
current_max_day <- data$MNCAATourneyCompactResults %>%
  filter(Season == 2026) %>%
  pull(DayNum) %>%
  max(na.rm = TRUE)

cat("Current max DayNum for 2026:", current_max_day, "\n")

# Update the championship game to have the highest DayNum
data$MNCAATourneyCompactResults <- data$MNCAATourneyCompactResults %>%
  mutate(
    DayNum = ifelse(
      Season == 2026 & 
        ((WTeamID == 1276 & LTeamID == 1163) | 
           (WTeamID == 1163 & LTeamID == 1276)),
      current_max_day + 1,  # Make it the highest
      DayNum
    )
  )

# Verify the fix
champ_game_2026 <- data$MNCAATourneyCompactResults %>%
  filter(Season == 2026, 
         (WTeamID == 1276 & LTeamID == 1163) | 
           (WTeamID == 1163 & LTeamID == 1276))

cat("\nUpdated championship game:\n")
print(champ_game_2026)

# Verify it's now the max DayNum
new_max_day <- data$MNCAATourneyCompactResults %>%
  filter(Season == 2026) %>%
  pull(DayNum) %>%
  max(na.rm = TRUE)

cat("\nNew max DayNum for 2026:", new_max_day, "\n")
cat("Championship game DayNum is now max:", champ_game_2026$DayNum == new_max_day, "\n")
# Years + number of brackets
set.seed(123)
years <- 2021:2026
n <- 100
pairwise_lookup <- read.csv("pairwise_lookup.csv")

# Store all champion picks
all_champion_picks <- list()

# FIXED: Helper function to get actual champion using tournament results (most reliable)
get_actual_champion <- function(year, data) {
  # Use tournament results directly - most reliable method
  results <- data$MNCAATourneyCompactResults %>% filter(Season == year)
  
  if(nrow(results) > 0) {
    # Get the last game by DayNum (championship game)
    max_day <- max(results$DayNum, na.rm = TRUE)
    champ_game <- results %>% filter(DayNum == max_day)
    
    if(nrow(champ_game) > 0) {
      champ_id <- champ_game$WTeamID[1]
      champ_team <- data$MTeams %>% filter(TeamID == champ_id)
      
      if(nrow(champ_team) > 0) {
        return(tibble(Year = year, 
                      ActualChampionID = champ_id, 
                      ActualChampion = champ_team$TeamName[1]))
      }
    }
  }
  
  # Fallback: try bracket_year_historical
  bracket <- tryCatch({
    bracket_year_historical(year, data)
  }, error = function(e) {
    return(NULL)
  })
  
  if(!is.null(bracket) && nrow(bracket) > 0) {
    max_round <- max(bracket$GameRound, na.rm = TRUE)
    champ_game <- bracket %>% filter(GameRound == max_round)
    
    if(nrow(champ_game) > 0 && !is.na(champ_game$Winner[1])) {
      champ_team <- data$MTeams %>% filter(TeamID == champ_game$Winner[1])
      if(nrow(champ_team) > 0) {
        return(tibble(Year = year, 
                      ActualChampionID = champ_game$Winner[1], 
                      ActualChampion = champ_team$TeamName[1]))
      }
    }
  }
  
  return(tibble(Year = year, ActualChampionID = NA, ActualChampion = NA))
}

# FIXED: Helper to extract champions from pipeline results (dynamic round detection)
get_champs <- function(results, model, year) {
  if(is.null(results) || is.null(results$picked$brackets)) {
    return(tibble())
  }
  
  # Dynamically find the championship round (highest GameRound)
  max_round <- max(results$picked$brackets$GameRound, na.rm = TRUE)
  
  champ_data <- results$picked$brackets %>%
    filter(GameRound == max_round) %>%
    group_by(BracketID) %>%
    slice(1) %>%
    ungroup()
  
  if(nrow(champ_data) == 0) {
    return(tibble())
  }
  
  champ_data %>%
    left_join(data$MTeams, by = c("Winner" = "TeamID")) %>%
    transmute(
      Year = year,
      Model = model,
      BracketID,
      ChampionID = Winner,
      Champion = TeamName,
      WinProb
    )
}

# Create actual champions lookup table FIRST
cat("Creating actual champions lookup table...\n")
actual_champs <- bind_rows(lapply(years, function(s) get_actual_champion(s, data)))
print(actual_champs)

# Loop through each season
for (s in years) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Processing year:", s, "\n")
  
  # Get actual champion for this year
  current_actual <- actual_champs %>% filter(Year == s)
  cat("Actual Champion:", current_actual$ActualChampion, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Skip if no champion data
  if(is.na(current_actual$ActualChampionID)) {
    cat("Skipping", s, "- No champion data available\n")
    next
  }
  
  # Get actual bracket results for scoring
  actual_winners <- bracket_year_historical(s, data) %>% 
    select(Slot, Winner, GameRound)
  
  # Run models with error handling - ALL using "random" pick_method
  cat("→ Running linear regression PCA...\n")
  results_linear <- tryCatch({
    run_pipeline(
      season = s,
      data = data,
      team_season_stats = team_season_stats,
      actual_slot_winners = actual_winners,
      n_generate = n,
      n_pick = n,
      pick_method = "random",
      model_type = "linear_pca"
    )
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    NULL
  })
  
  cat("→ Running Ranger...\n")
  results_ranger <- tryCatch({
    run_pipeline(
      season = s,
      data = data,
      team_season_stats = team_season_stats,
      actual_slot_winners = actual_winners,
      n_generate = n,
      n_pick = n,
      pick_method = "random",
      model_type = "ranger"
    )
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    NULL
  })
  
  cat("→ Running Logistic...\n")
  results_logistic <- tryCatch({
    run_pipeline(
      season = s,
      data = data,
      team_season_stats = team_season_stats,
      actual_slot_winners = actual_winners,
      n_generate = n,
      n_pick = n,
      pick_method = "random",
      model_type = "logistic"
    )
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    NULL
  })
  
  cat("→ Running XGBoost...\n")
  results_xgb <- tryCatch({
    run_pipeline(
      season = s,
      data = data,
      team_season_stats = team_season_stats,
      actual_slot_winners = actual_winners,
      n_generate = n,
      n_pick = n,
      pick_method = "random",
      model_type = "xgboost",
      xgb_params = list(nrounds = 100, max_depth = 6, eta = 0.3)
    )
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    NULL
  })
  
  cat("→ Running ELO Consensus...\n")
  results_elo <- tryCatch({
    run_pipeline(
      season = s,
      data = data,
      team_season_stats = team_season_stats,
      actual_slot_winners = actual_winners,
      n_generate = n,
      n_pick = n,
      pick_method = "random",
      model_type = "elo",
      elo_pairwise_lookup = pairwise_lookup
    )
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    NULL
  })
  
  # cat("→ Running ELO Underconf...\n")
  # results_elo_underconf <- tryCatch({
  #   run_pipeline(
  #     season = s,
  #     data = data,
  #     team_season_stats = team_season_stats,
  #     actual_slot_winners = actual_winners,
  #     n_generate = n,
  #     n_pick = n,
  #     pick_method = "random",
  #     model_type = "elo",
  #     elo_pairwise_lookup = pairwise_lookup,
  #     elo_gamma = 0.25
  #   )
  # }, error = function(e) {
  #   cat("  Error:", e$message, "\n")
  #   NULL
  # })
  
  # Extract champion picks from all 5 models
  yearly_champs <- bind_rows(
    get_champs(results_ranger, "Ranger", s),
    get_champs(results_logistic, "Logistic", s),
    get_champs(results_xgb, "XGBoost", s),
    get_champs(results_elo, "Consensus", s),
    get_champs(results_linear, "linear_pca",s)
    # ,
    # get_champs(results_elo_underconf, "Consensus_underconf", s)
  )
  
  cat("✓ Collected", nrow(yearly_champs), "champion picks\n")
  
  all_champion_picks[[as.character(s)]] <- yearly_champs
}

# Combine all years
combined_champs <- bind_rows(all_champion_picks)

# Join with actual champions
combined_champs <- combined_champs %>%
  left_join(actual_champs, by = "Year") %>%
  mutate(Correct = ChampionID == ActualChampionID)

# =========================
# SUMMARY OUTPUT
# =========================

cat("\n\n", paste(rep("=", 60), collapse = ""), "\n")
cat("FINAL RESULTS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Check if we have data for 2021
cat("\n2021 Data:\n")
print(combined_champs %>% filter(Year == 2021))

# Accuracy by model (all 5 models)
accuracy_summary <- combined_champs %>%
  group_by(Model) %>%
  summarise(
    Accuracy = mean(Correct, na.rm = TRUE) * 100,
    Correct = sum(Correct, na.rm = TRUE),
    Total = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Accuracy))

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("ACCURACY BY MODEL (All 5 Models)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
print(accuracy_summary)

# Accuracy by year for all models
year_summary <- combined_champs %>%
  group_by(Year, Model) %>%
  summarise(
    Accuracy = mean(Correct, na.rm = TRUE) * 100,
    Correct = sum(Correct, na.rm = TRUE),
    Total = n(),
    .groups = "drop"
  )

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("ACCURACY BY YEAR\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
print(year_summary)

# =========================
# VISUALIZATIONS
# =========================

# Plot 1: Champion accuracy by model and year (all 5 models)
# Use year_summary instead of accuracy_summary for faceted plot
year_summary <- year_summary %>%
  mutate(Model = ifelse(Model == "Ranger", "Random_Forest", Model))

p2_faceted <- ggplot(year_summary, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Accuracy, 1), "%\n")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~Year, ncol = 2, scale="free_x") +
  labs(
    title = "Champion Prediction Accuracy by Model and Year",
    subtitle = paste("All 5 Models |", n, "brackets/year"),
    x = "Model",
    y = "Accuracy (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p2_faceted+scale_fill_viridis_d())

# Plot 2: Overall accuracy bar chart (all 5 models)
accuracy_summary <- accuracy_summary %>%
  mutate(Model = ifelse(Model == "Ranger", "Random_Forest", Model))
p2 <- ggplot(accuracy_summary, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Accuracy, 1), "%\n(", Correct, "/", Total, ")")), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = "Overall Champion Prediction Accuracy by Model",
    subtitle = paste("All 5 Models | Years:", paste(years, collapse = ", "), "|", n, "brackets/year"),
    x = "Model",
    y = "Accuracy (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(accuracy_summary$Accuracy, na.rm = TRUE) + 10)

print(p2+scale_fill_viridis_d())

# # Optional: Save results
write.csv(combined_champs, "champion_predictions_all_models2.csv", row.names = FALSE)
write.csv(accuracy_summary, "accuracy_summary_all_models2.csv", row.names = FALSE)