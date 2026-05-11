# model predictors function --------------------------------------
get_model_predictors <- function() {
  c(
    #"WinPct", 
    "AvgPoints", "AvgOppPoints", 
    "FG2_Pct", "FG3_Pct", "FT_Pct", 
    #"ThreeP_Rate", 
    "OR_pg", "DR_pg", 
    #"Ast_pg", 
    "TO_pg", 
    #"Stl_pg", 
    #"Blk_pg", 
    #"PF_pg", 
    #"YearsNCAA", 
    "CareerTourneyWins",
    #"Momentum",
    "SOV",
    "AdjEM", "AdjOE", "AdjDE", "RankAdjEM",
    "AdjTempo"
  )
}

# NEW FUNCTION: Get bracket structure for simulation (no results needed)----------
get_bracket_structure <- function(season, data) {
  
  # Get seeds for the season
  seeds <- data$MNCAATourneySeeds %>%
    filter(Season == season)
  
  # Get slot structure
  slots <- data$MNCAATourneySlots %>%
    filter(Season == season)
  
  # Get round mapping
  round_map <- data$MNCAATourneySeedRoundSlots %>%
    select(GameSlot, GameRound) %>%
    distinct()
  
  # Join to get rounds
  slots_with_rounds <- slots %>%
    left_join(round_map, by = c("Slot" = "GameSlot")) %>%
    arrange(GameRound)
  
  # Create a lookup for seeds to team IDs
  seed_to_team <- seeds %>%
    select(Seed, TeamID) %>%
    deframe()
  
  # For each slot, determine the teams (where possible)
  result <- slots_with_rounds %>%
    rowwise() %>%
    mutate(
      # StrongSeed is always a seed
      Team1 = seed_to_team[StrongSeed],
      
      # WeakSeed could be a seed OR a slot reference
      Team2 = ifelse(
        WeakSeed %in% names(seed_to_team),
        seed_to_team[WeakSeed],
        NA_integer_  # Slot reference - will be filled during simulation
      )
    ) %>%
    ungroup()
  
  # Return ALL slots
  result %>%
    select(Season, Slot, Team1, Team2, GameRound, StrongSeed, WeakSeed)
}

# Modified bracket_year for historical use (when you need actual results)
bracket_year_historical <- function(season, data) {
  
  seeds <- data$MNCAATourneySeeds %>%
    filter(Season == season)
  
  results <- data$MNCAATourneyCompactResults %>%
    filter(Season == season)
  
  round_map <- data$MNCAATourneySeedRoundSlots %>%
    select(GameSlot, GameRound) %>%
    distinct()
  
  slots <- data$MNCAATourneySlots %>%
    filter(Season == season) %>%
    left_join(round_map, by = c("Slot" = "GameSlot")) %>%
    arrange(GameRound)
  
  resolve_slot <- function(slot_name, winners) {
    if (slot_name %in% names(winners)) {
      return(winners[[slot_name]])
    }
    team <- seeds %>%
      filter(Seed == slot_name) %>%
      pull(TeamID)
    if (length(team) == 0) NA_integer_ else team
  }
  
  winners <- list()
  games <- vector("list", nrow(slots))
  
  for (i in seq_len(nrow(slots))) {
    slot <- slots$Slot[i]
    team1 <- resolve_slot(slots$StrongSeed[i], winners)
    team2 <- resolve_slot(slots$WeakSeed[i], winners)
    
    res <- results %>%
      filter(WTeamID %in% c(team1, team2), LTeamID %in% c(team1, team2))
    
    winner <- if (nrow(res) > 0) res$WTeamID[1] else NA_integer_
    winners[[slot]] <- winner
    
    games[[i]] <- tibble(
      Season = season,
      Slot = slot,
      StrongSeed = slots$StrongSeed[i],  # ADD THIS
      WeakSeed = slots$WeakSeed[i],      # ADD THIS
      Team1 = team1,
      Team2 = team2,
      Winner = winner,
      GameRound = slots$GameRound[i]
    )
  }
  
  bind_rows(games) %>%
    arrange(GameRound) %>% 
    filter(GameRound != 0)
}

#build training data function ------
build_training_data_balanced <- function(
    data,
    team_season_stats,
    start_season = 2003,
    train_until_season
) {
  
  # 1. Get tournament games
  tourney_games <- data$MNCAATourneyCompactResults %>%
    filter(
      Season >= start_season,
      Season < train_until_season,
      Season != 2020
    ) %>%
    select(Season, WTeamID, LTeamID)
  
  # 2. Randomly choose orientation for each game
  set.seed(123)  # For reproducibility
  
  games <- tourney_games %>%
    rowwise() %>%
    mutate(
      # Random coin flip: TRUE = winner is Team1, FALSE = loser is Team1
      winner_as_team1 = sample(c(TRUE, FALSE), 1),
      
      # Set Team1, Team2, and Win accordingly
      Team1 = ifelse(winner_as_team1, WTeamID, LTeamID),
      Team2 = ifelse(winner_as_team1, LTeamID, WTeamID),
      Win   = ifelse(winner_as_team1, 1, 0)   # 1 if Team1 is the actual winner
    ) %>%
    ungroup() %>%
    select(Season, Team1, Team2, Win)
  
  # 3. Join Team1 stats
  all_stats_cols <- setdiff(names(team_season_stats), c("Season", "TeamID"))
  
  games <- games %>%
    left_join(
      team_season_stats %>% 
        select(Season, TeamID, all_of(all_stats_cols)),
      by = c("Season", "Team1" = "TeamID")
    ) %>%
    rename_with(~ paste0(.x, "_1"), .cols = all_of(all_stats_cols))
  
  # 4. Join Team2 stats
  games <- games %>%
    left_join(
      team_season_stats %>% 
        select(Season, TeamID, all_of(all_stats_cols)),
      by = c("Season", "Team2" = "TeamID")
    ) %>%
    rename_with(~ paste0(.x, "_2"), .cols = all_of(all_stats_cols))
  
  # 5. Drop rows with missing stats 
  #  games <- games %>%
  #    drop_na()
  #comment this out for now since it should fail if there are NA's
  
  # 6. Build difference features for numeric predictors
  predictors <- get_model_predictors()  # list of base stat names
  
  diff_list <- list()
  for(pred in predictors){
    col_1 <- paste0(pred, "_1")
    col_2 <- paste0(pred, "_2")
    if(all(c(col_1, col_2) %in% names(games))){
      diff_list[[pred]] <- games[[col_1]] - games[[col_2]]
    }
  }
  diff_data <- as.data.frame(diff_list)
  
  # 7. Add non-difference features
  non_diff_features <- data.frame(
    #Team_Strength_Total = games$AdjEM_1 + games$AdjEM_2,
    Win = games$Win
  )
  
  # 8. Combine all features
  final_data <- cbind(non_diff_features, diff_data)
  final_data$Season <- games$Season   # keep season for CV splits
  
  # 9. Drop zero-variance columns from diff features only
  diff_cols <- names(diff_data)
  if(length(diff_cols) > 0) {
    zero_var_diff <- sapply(final_data[, diff_cols, drop = FALSE], 
                            function(col) var(col, na.rm = TRUE) == 0)
    if(any(zero_var_diff)) {
      final_data <- final_data[, !names(final_data) %in% names(zero_var_diff)[zero_var_diff]]
    }
  }
  
  return(final_data)
}

# points per round function -----------------------------------------------
points_for_round <- function(round) {
  dplyr::case_when(
    round == 1 ~ 1,
    round == 2 ~ 1,
    round == 3 ~ 2,
    round == 4 ~ 3,
    round == 5 ~ 5,
    round == 6 ~ 8,
    TRUE ~ 0
  )
}

# PREPARE SINGLE GAME PREDICTION ROW-------------------------------
prepare_prediction_data <- function(
    team1_id,
    team2_id,
    team_season_stats,
    season,
    predictors,               # full list of column names needed (non-diff + diff)
    diff_predictor_names,      # names from get_model_predictors() (the ones that become diffs)
    non_diff_names             # non-diff features like "Team_Strength_Total"
) {
  t1 <- team_season_stats %>% filter(Season == season, TeamID == team1_id)
  t2 <- team_season_stats %>% filter(Season == season, TeamID == team2_id)
  if (nrow(t1) != 1 || nrow(t2) != 1) return(NULL)
  
  # 1. Difference features (one for each in diff_predictor_names)
  diff_features <- list()
  for (p in diff_predictor_names) {
    diff_features[[p]] <- as.numeric(t1[[p]] - t2[[p]])
  }
  
  # 2. Non-difference features (must match those created in training)
  non_diff_features <- list()
  #  if ("Team_Strength_Total" %in% non_diff_names) {
  #    non_diff_features$Team_Strength_Total <- as.numeric(t1$AdjEM + t2$AdjEM)
  #  }
  # Can add diff features here
  
  # 3. Combine in the same order as training predictors
  all_features <- c(non_diff_features, diff_features)
  row <- as.data.frame(all_features)
  
  # 4. Check that all predictors are present
  missing_cols <- setdiff(predictors, names(row))
  if (length(missing_cols) > 0) {
    stop("Missing columns in prediction data: ", paste(missing_cols, collapse = ", "))
  }
  
  # 5. Ensure correct column order and numeric type
  row <- row[, predictors, drop = FALSE]
  row[] <- lapply(row, as.numeric)
  row
}

# MODIFIED: MONTE CARLO BRACKET GENERATION (using ranger model)----------------------
# MODIFIED: MONTE CARLO BRACKET GENERATION (supports multiple models with ELO gamma)
generate_monte_carlo_brackets <- function(
    season,
    data,
    team_season_stats,
    prediction_models,          
    predictors,
    diff_predictor_names,
    non_diff_names,
    n_generate = 500,
    seed = 123,
    is_future = FALSE,
    elo_lookup = NULL,
    elo_gamma = 1.0  # ADD THIS PARAMETER
) {
  set.seed(seed)
  
  model_type <- prediction_models$model_type
  cat("  Using model type:", model_type, "\n")
  if (grepl("elo", model_type)) {
    cat("  ELO Gamma:", elo_gamma, "\n")
  }
  
  # Get bracket structure
  if (is_future) {
    full_bracket <- get_bracket_structure(season, data)
    full_bracket$Winner <- NA_integer_
  } else {
    full_bracket <- bracket_year_historical(season, data)
  }
  
  # Split by round
  play_in_games <- full_bracket %>% filter(GameRound == 0)
  first_round <- full_bracket %>% filter(GameRound == 1)
  later_rounds <- full_bracket %>% filter(GameRound > 1) %>% arrange(GameRound)
  
  cat("→ Play-in games:", nrow(play_in_games), "\n")
  if (!is_future && nrow(play_in_games) > 0) {
    cat("→ Play-in winners known:", sum(!is.na(play_in_games$Winner)), "of", nrow(play_in_games), "\n")
  }
  
  # Model-agnostic prediction function (with ELO gamma support)
  predict_game <- function(t1, t2) {
    if (grepl("elo", model_type)) {
      p <- elo_lookup %>%
        filter(year == season, TeamA == t1, TeamB == t2) %>%
        pull(matchup_prob)
      
      if (length(p) == 0) p <- 0.5
      
      if (elo_gamma != 1.0) {
        p_raw <- pmax(pmin(p, 1 - 1e-10), 1e-10)
        log_odds <- log(p_raw / (1 - p_raw))
        prob <- 1 / (1 + exp(-elo_gamma * log_odds))
      } else {
        prob <- p
      }
      
    } else if (model_type == "linear_pca") {
      pred <- prepare_prediction_data(
        t1, t2, team_season_stats, season,
        predictors, diff_predictor_names, non_diff_names
      )
      
      if (is.null(pred)) return(list(winner = t1, prob = 0.5))
      
      pred_df <- as.data.frame(pred)
      
      # Project matchup into the PCA space learned on training data
      pc_vals <- predict(prediction_models$pca_model, newdata = pred_df)
      pc_df <- data.frame(
        PC1 = pc_vals[, 1],
        PC2 = pc_vals[, 2]
      )
      
      # Linear regression prediction
      prob <- predict(prediction_models$model, newdata = pc_df)
      
      # Keep probability in [0, 1]
      prob <- pmin(pmax(prob, 0), 1)
      
    } else {
      pred <- prepare_prediction_data(
        t1, t2, team_season_stats, season,
        predictors, diff_predictor_names, non_diff_names
      )
      
      if (is.null(pred)) return(list(winner = t1, prob = 0.5))
      
      pred_df <- as.data.frame(pred)
      
      if (model_type == "ranger") {
        rf_pred <- predict(prediction_models$model, data = pred_df)
        prob <- rf_pred$predictions[[1]]
        
      } else if (model_type == "logistic") {
        prob <- predict(prediction_models$model, newdata = pred_df, type = "response")
        
      } else if (model_type == "xgboost") {
        x_pred <- as.matrix(pred_df[, prediction_models$predictors, drop = FALSE])
        prob <- predict(prediction_models$model, xgb.DMatrix(x_pred))
      }
    }
    
    winner <- ifelse(runif(1) < prob, t1, t2)
    list(winner = winner, prob = prob)
  }
  
  cat("→ Generating", n_generate, "brackets...\n")
  all <- vector("list", n_generate)
  
  for (b in seq_len(n_generate)) {
    if (b %% 25 == 0 || b == 1 || b == n_generate)
      cat("  Bracket", b, "of", n_generate, "\n")
    
    winners <- list()
    games <- list()
    g <- 1
    
    # Handle play-in games
    if (nrow(play_in_games) > 0) {
      for (i in seq_len(nrow(play_in_games))) {
        p <- play_in_games[i, ]
        
        if (!is_future && !is.na(p$Winner)) {
          winners[[as.character(p$Slot)]] <- p$Winner
          games[[g]] <- data.frame(
            BracketID = b, Slot = as.character(p$Slot),
            Team1 = p$Team1, Team2 = p$Team2, Winner = p$Winner,
            GameRound = 0, WinProb = 1.0, stringsAsFactors = FALSE
          )
          g <- g + 1
        } else if (!is.na(p$Team1) && !is.na(p$Team2)) {
          res <- predict_game(p$Team1, p$Team2)
          winners[[as.character(p$Slot)]] <- res$winner
          games[[g]] <- data.frame(
            BracketID = b, Slot = as.character(p$Slot),
            Team1 = p$Team1, Team2 = p$Team2, Winner = res$winner,
            GameRound = 0, WinProb = res$prob, stringsAsFactors = FALSE
          )
          g <- g + 1
        } else {
          winners[[as.character(p$Slot)]] <- NA_integer_
        }
      }
    }
    
    # First round
    for (i in seq_len(nrow(first_round))) {
      r <- first_round[i, ]
      
      if (is_future && is.na(r$Team2)) {
        play_in_slot <- r$WeakSeed
        play_in_winner <- winners[[as.character(play_in_slot)]]
        
        if (!is.na(play_in_winner)) {
          res <- predict_game(r$Team1, play_in_winner)
          winners[[as.character(r$Slot)]] <- res$winner
          games[[g]] <- data.frame(
            BracketID = b, Slot = as.character(r$Slot),
            Team1 = r$Team1, Team2 = play_in_winner, Winner = res$winner,
            GameRound = 1, WinProb = res$prob, stringsAsFactors = FALSE
          )
          g <- g + 1
        } else {
          winners[[as.character(r$Slot)]] <- NA_integer_
        }
      } else if (!is.na(r$Team1) && !is.na(r$Team2)) {
        res <- predict_game(r$Team1, r$Team2)
        winners[[as.character(r$Slot)]] <- res$winner
        games[[g]] <- data.frame(
          BracketID = b, Slot = as.character(r$Slot),
          Team1 = r$Team1, Team2 = r$Team2, Winner = res$winner,
          GameRound = 1, WinProb = res$prob, stringsAsFactors = FALSE
        )
        g <- g + 1
      } else {
        winners[[as.character(r$Slot)]] <- NA_integer_
      }
    }
    
    # Later rounds
    for (i in seq_len(nrow(later_rounds))) {
      s <- later_rounds[i, ]
      
      team1 <- winners[[as.character(s$StrongSeed)]]
      team2 <- winners[[as.character(s$WeakSeed)]]
      
      if (!is.null(team1) && !is.null(team2) && !is.na(team1) && !is.na(team2)) {
        res <- predict_game(team1, team2)
        winners[[as.character(s$Slot)]] <- res$winner
        games[[g]] <- data.frame(
          BracketID = b, Slot = as.character(s$Slot),
          Team1 = team1, Team2 = team2, Winner = res$winner,
          GameRound = s$GameRound, WinProb = res$prob, stringsAsFactors = FALSE
        )
        g <- g + 1
      } else {
        winners[[as.character(s$Slot)]] <- NA_integer_
      }
    }
    
    all[[b]] <- bind_rows(games)
  }
  
  brackets <- bind_rows(all)
  
  stats <- brackets %>%
    group_by(BracketID) %>%
    summarise(
      GamesSimulated = n(),
      ExpectedPoints = sum(points_for_round(GameRound) * WinProb, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("✓ Bracket generation complete\n")
  cat("  Simulated", mean(stats$GamesSimulated), "games per bracket\n")
  
  list(brackets = brackets, stats = stats)
}

# DIVERSE PICKER -------------------------------

select_diverse_brackets <- function(mc, n_pick = 10, min_variance = 20, 
                                    max_attempts = 1000, method = "diverse") {
  all_brackets <- mc$brackets
  all_ids <- sort(unique(all_brackets$BracketID))
  stats_df <- mc$stats
  
  # For alldiverse, we ignore n_pick and try to get all that qualify
  is_alldiverse <- method == "alldiverse"
  
  # DON'T return all IDs immediately for alldiverse
  # We need to actually do the selection
  if (!is_alldiverse && n_pick >= length(all_ids)) {
    return(all_ids)
  }
  
  # Get unique slots - they're already character from generation step
  all_slots <- sort(unique(all_brackets$Slot))
  bracket_ids <- sort(unique(all_brackets$BracketID))
  
  winner_wide <- all_brackets %>%
    select(BracketID, Slot, Winner) %>%
    tidyr::complete(BracketID = bracket_ids, Slot = all_slots) %>%
    pivot_wider(id_cols = BracketID, names_from = Slot, values_from = Winner) %>%
    arrange(BracketID)
  
  winners_mat <- as.data.frame(winner_wide, stringsAsFactors = FALSE)
  rownames(winners_mat) <- winners_mat$BracketID
  winners_mat$BracketID <- NULL
  winners_mat <- as.matrix(winners_mat)
  mode(winners_mat) <- "character"
  
  slot_points_df <- all_brackets %>%
    distinct(Slot, GameRound) %>%
    mutate(Points = points_for_round(GameRound)) %>%
    arrange(match(Slot, colnames(winners_mat)))
  
  points_vec <- rep(0, ncol(winners_mat))
  names(points_vec) <- colnames(winners_mat)
  matched <- match(slot_points_df$Slot, names(points_vec))
  points_vec[matched[!is.na(matched)]] <- slot_points_df$Points[!is.na(matched)]
  
  pair_diff_points <- function(idx_a, idx_b) {
    w1 <- winners_mat[idx_a, ]
    w2 <- winners_mat[idx_b, ]
    has_pick <- (!is.na(w1) & w1 != "NA" & w1 != "") | (!is.na(w2) & w2 != "NA" & w2 != "")
    different <- (w1 != w2) & has_pick
    sum(points_vec[different], na.rm = TRUE)
  }
  
  diffs_to_selected <- function(candidate_idx, selected_indices) {
    sapply(selected_indices, function(si) pair_diff_points(candidate_idx, si))
  }
  
  first_id <- stats_df %>% arrange(desc(ExpectedPoints)) %>% dplyr::slice(1) %>% pull(BracketID)
  selected_ids <- c(first_id)
  selected_indices <- which(bracket_ids %in% selected_ids)
  
  attempts <- 0
  
  # For alldiverse, we want ALL that meet threshold, so n_pick is effectively infinity
  # But we still need a stopping condition
  max_to_select <- ifelse(is_alldiverse, length(all_ids), n_pick)
  
  while (length(selected_ids) < max_to_select && attempts < max_attempts) {
    remaining_ids <- setdiff(all_ids, selected_ids)
    if (length(remaining_ids) == 0) break
    remaining_indices <- which(bracket_ids %in% remaining_ids)
    
    min_diffs <- sapply(remaining_indices, function(ci) {
      diffs <- diffs_to_selected(ci, selected_indices)
      if (length(diffs) == 0) return(0)
      min(diffs, na.rm = TRUE)
    })
    names(min_diffs) <- remaining_ids
    
    candidates <- remaining_ids[min_diffs >= min_variance]
    
    if (length(candidates) > 0) {
      next_id <- stats_df %>%
        filter(BracketID %in% candidates) %>%
        arrange(desc(ExpectedPoints)) %>%
        dplyr::slice(1) %>%
        pull(BracketID)
      
      selected_ids <- c(selected_ids, next_id)
      selected_indices <- which(bracket_ids %in% selected_ids)
    } else {
      # No more candidates meet the threshold
      if (is_alldiverse) {
        break  # Exit the loop - we've found all that meet min_variance
      } else {
        # For regular diverse, pick the best available even if below threshold
        max_min_val <- max(min_diffs, na.rm = TRUE)
        best_candidates <- names(min_diffs)[min_diffs == max_min_val]
        next_id <- stats_df %>%
          filter(BracketID %in% best_candidates) %>%
          arrange(desc(ExpectedPoints)) %>%
          dplyr::slice(1) %>%
          pull(BracketID)
        
        selected_ids <- c(selected_ids, next_id)
        selected_indices <- which(bracket_ids %in% selected_ids)
      }
    }
    attempts <- attempts + 1
  }
  
  # Only pad with top expected if we're in "diverse" mode and couldn't meet n_pick
  if (!is_alldiverse && length(selected_ids) < n_pick) {
    remaining_ids <- setdiff(all_ids, selected_ids)
    needed <- n_pick - length(selected_ids)
    if (length(remaining_ids) > 0) {
      additional <- stats_df %>%
        filter(BracketID %in% remaining_ids) %>%
        arrange(desc(ExpectedPoints)) %>%
        dplyr::slice_head(n = needed) %>%
        pull(BracketID)
      selected_ids <- c(selected_ids, additional)
    }
  }
  
  selected_ids
}

# PICK BRACKETS (top/diverse/random) -------------------------------
pick_brackets <- function(mc, n_pick = 10, method = "diverse", min_variance = 20) {
  if (method == "top_expected") {
    ids <- mc$stats %>% arrange(desc(ExpectedPoints)) %>% dplyr::slice(1:n_pick) %>% pull(BracketID)
  } else if (method == "diverse") {
    ids <- select_diverse_brackets(mc, n_pick = n_pick, min_variance = min_variance, 
                                   method = "diverse")
  } else if (method == "alldiverse") {
    # For alldiverse, ignore n_pick completely
    ids <- select_diverse_brackets(mc, n_pick = Inf, min_variance = min_variance, 
                                   method = "alldiverse")
  } else if (method == "random") {
    ids <- sample(mc$stats$BracketID, min(n_pick, nrow(mc$stats)))
  } else {
    stop("Unknown pick method")
  }
  
  list(
    brackets = mc$brackets %>% filter(BracketID %in% ids),
    stats = mc$stats %>% filter(BracketID %in% ids)
  )
}

# ATTACH TEAM NAMES ------------------------------
attach_team_names <- function(brackets_df, teams_df) {
  df <- brackets_df %>%
    left_join(teams_df %>% select(TeamID, TeamName), by = c("Team1" = "TeamID")) %>%
    rename(Team1Name = TeamName) %>%
    left_join(teams_df %>% select(TeamID, TeamName), by = c("Team2" = "TeamID")) %>%
    rename(Team2Name = TeamName) %>%
    left_join(teams_df %>% select(TeamID, TeamName), by = c("Winner" = "TeamID")) %>%
    rename(WinnerName = TeamName)
  df
}

# SCORE BRACKETS -------------------------------
score_brackets <- function(picked_brackets, actual_slot_winners) {
  actual_slot_winners <- actual_slot_winners %>% mutate(Slot = as.character(Slot))
  picks <- picked_brackets %>% mutate(Slot = as.character(Slot))
  
  merged <- picks %>%
    left_join(actual_slot_winners %>% select(Slot, ActualWinner = Winner), by = "Slot") %>%
    mutate(
      PointsRound = points_for_round(GameRound),
      Correct = (as.character(Winner) == as.character(ActualWinner)),
      PointsEarned = ifelse(Correct, PointsRound, 0)
    )
  
  scores <- merged %>%
    group_by(BracketID) %>%
    summarise(TotalPoints = sum(PointsEarned, na.rm = TRUE), .groups = "drop")
  
  list(pick_details = merged, scores = scores)
}

# MODIFIED: run pipeline function with model selection----
run_pipeline <- function(
    season,
    data,
    team_season_stats,
    actual_slot_winners = NULL,
    scoring = TRUE,        
    n_generate = 500,
    n_pick = 10,
    pick_method = "diverse",
    min_variance = 20,
    seed = 123,
    model_type = "ranger",
    elo_pairwise_lookup = NULL,
    elo_gamma = 1.0,
    xgb_params = NULL
) {
  
  cat("====================================\n")
  cat("Running pipeline for season", season, "\n")
  cat("Model type:", model_type, "\n")
  if (grepl("elo", model_type)) {
    cat("ELO Gamma:", elo_gamma, "\n")
  }
  cat("====================================\n")
  
  max_historical <- max(data$MNCAATourneyCompactResults$Season, na.rm = TRUE)
  is_future <- season > max_historical
  
  cat("→ Building training data (pre-", season, ")...\n", sep = "")
  
  train_data <- build_training_data_balanced(
    data = data,
    team_season_stats = team_season_stats %>% filter(Season < season),
    start_season = 2003,
    train_until_season = season
  )
  
  predictors <- setdiff(names(train_data), c("Win", "Season"))
  base_predictors <- get_model_predictors()
  diff_predictor_names <- base_predictors
  non_diff_names <- c()
  
  prediction_models <- list()
  
  # -------------------- MODEL TRAINING --------------------
  
  if (model_type == "ranger") {
    
    set.seed(seed)
    rf_model <- ranger(
      dependent.variable.name = "Win",
      data = train_data[, c(predictors, "Win")],
      num.trees = 500,
      mtry = 4,
      min.node.size = 1,
      probability = TRUE,
      num.threads = 1,
      seed = seed,
      importance = "impurity"
    )
    
    prediction_models <- list(
      model = rf_model,
      model_type = "ranger",
      predictors = predictors
    )
    
  } else if (model_type == "logistic") {
    
    logit_model <- glm(
      Win ~ .,
      data = train_data[, c(predictors, "Win")],
      family = binomial(link = "logit")
    )
    
    prediction_models <- list(
      model = logit_model,
      model_type = "logistic",
      predictors = predictors
    )
    
  } else if (model_type == "xgboost") {
    
    x_train <- as.matrix(train_data[, predictors])
    y_train <- train_data$Win
    
    params <- list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = ifelse(is.null(xgb_params$max_depth), 6, xgb_params$max_depth),
      eta = ifelse(is.null(xgb_params$eta), 0.1, xgb_params$eta),
      subsample = ifelse(is.null(xgb_params$subsample), 0.9, xgb_params$subsample),
      colsample_bytree = ifelse(is.null(xgb_params$colsample_bytree), 0.8, xgb_params$colsample_bytree),
      min_child_weight = ifelse(is.null(xgb_params$min_child_weight), 1, xgb_params$min_child_weight),
      gamma = ifelse(is.null(xgb_params$gamma), 0.1, xgb_params$gamma),
      nthread = 1
    )
    
    dtrain <- xgb.DMatrix(x_train, label = y_train)
    
    set.seed(seed)
    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = ifelse(is.null(xgb_params$nrounds), 100, xgb_params$nrounds),
      verbose = 0
    )
    
    prediction_models <- list(
      model = xgb_model,
      model_type = "xgboost",
      predictors = predictors
    )
    
  } else if (model_type == "linear_pca") {
    
    # ---- PCA + Linear Regression MODEL ----
    
    pca_x <- train_data[, predictors]
    
    keep <- complete.cases(pca_x) & !is.na(train_data$Win)
    pca_x <- pca_x[keep, , drop = FALSE]
    pca_y <- train_data$Win[keep]
    
    pca_model <- prcomp(pca_x, center = TRUE, scale. = TRUE)
    
    pca_scores <- as.data.frame(pca_model$x[, 1:2, drop = FALSE])
    colnames(pca_scores) <- c("PC1", "PC2")
    
    lm_data <- cbind(Win = pca_y, pca_scores)
    
    lm_model <- lm(Win ~ PC1 + PC2, data = lm_data)
    
    prediction_models <- list(
      model = lm_model,
      model_type = "linear_pca",
      predictors = c("PC1", "PC2"),
      pca_model = pca_model
    )
    
  } else if (model_type == "elo" ||
             model_type == "elo_underconf" ||
             model_type == "elo_overconf") {
    
    if (is.null(elo_pairwise_lookup)) {
      stop("elo_pairwise_lookup must be provided for elo model type")
    }
    
    prediction_models <- list(
      model_type = model_type,
      pairwise_lookup = elo_pairwise_lookup,
      season = season,
      gamma = elo_gamma
    )
    
  } else {
    stop("Unknown model_type. Choose: 'ranger', 'logistic', 'xgboost', 'linear_pca', 'elo', 'elo_underconf', or 'elo_overconf'")
  }
  
  mc <- generate_monte_carlo_brackets(
    season, data, team_season_stats,
    prediction_models = prediction_models,
    predictors = predictors,
    diff_predictor_names = diff_predictor_names,
    non_diff_names = non_diff_names,
    n_generate = n_generate,
    seed = seed,
    is_future = is_future,
    elo_lookup = elo_pairwise_lookup,
    elo_gamma = elo_gamma
  )
  
  picked <- pick_brackets(mc, n_pick, method = pick_method, min_variance = min_variance)
  
  picked$brackets <- attach_team_names(picked$brackets, data$MTeams)
  
  seed_lookup <- data$MNCAATourneySeeds %>%
    filter(Season == season) %>%
    select(TeamID, Seed) %>%
    mutate(Seed = as.character(Seed))
  
  picked$brackets <- picked$brackets %>%
    left_join(seed_lookup, by = c("Team1" = "TeamID")) %>% rename(Team1Seed = Seed) %>%
    left_join(seed_lookup, by = c("Team2" = "TeamID")) %>% rename(Team2Seed = Seed) %>%
    left_join(seed_lookup, by = c("Winner" = "TeamID")) %>% rename(WinnerSeed = Seed)
  
  if (scoring && !is.null(actual_slot_winners)) {
    picked$scores <- score_brackets(picked$brackets, actual_slot_winners)
  } else {
    picked$scores <- list(scores = tibble())
  }
  
  cat("✓ Pipeline complete\n")
  
  list(
    mc = mc,
    picked = picked,
    model = prediction_models
  )
}