library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(ranger)  
library(stringr)
library(tidyr)
library(glmnet)

# loading data into directory  --------------------------------
# Set new data directory
data_dir <- "project/csvs"

files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

data <- files %>%
  set_names(~ str_remove(basename(.), "\\.csv$")) %>%
  map(~ read_csv(.x, show_col_types = FALSE))

# # Load KenPom file
# kenpom_final <- read.csv("Rstudio work/kenpom_tournament_ready.csv") 
#commented out for now for later in the script

# adding 2026 to team season stats ----------------------------------------

library(dplyr)

games_2026 <- tibble(
  Season = 2026,
  DayNum = 136,
  WTeam = c(
    "Howard","Texas","Prairie View A&M","Miami (Ohio)",
    "TCU","Nebraska","Louisville","High Point","Duke","Vanderbilt",
    "Michigan State","Arkansas","VCU","Michigan","Texas","Texas A&M",
    "Illinois","Saint Louis","Gonzaga","Houston",
    "Kentucky","Texas Tech","Arizona","Virginia","Iowa State","Alabama",
    "Utah State","Tennessee","Iowa","St. John's","Purdue","UCLA",
    "Florida","Kansas","Miami (Fla.)","UConn",
    "Michigan","Michigan State","Duke","Houston","Texas","Illinois",
    "Nebraska","Arkansas",
    "Purdue","Iowa State","St. John's","Tennessee","Iowa","Arizona",
    "UConn","Alabama",
    "Purdue","Iowa","Arizona","Illinois",
    "Duke","Michigan","UConn","Tennessee",
    "Illinois","Arizona",
    "Michigan","UConn",
    "UConn","Michigan",
    "Michigan"
  ),
  WScore = c(
    86,68,67,89,
    66,76,83,83,71,78,
    92,97,82,101,79,63,
    105,102,73,78,
    89,91,92,82,108,90,
    86,78,67,79,104,75,
    114,68,80,82,
    95,77,81,88,74,76,
    74,94,
    79,82,67,79,73,78,
    73,90,
    79,77,109,65,
    80,90,67,76,
    71,79,
    95,73,
    71,91,
    69
  ),
  LTeam = c(
    "UMBC","NC State","Lehigh","SMU",
    "Ohio State","Troy","South Florida","Wisconsin","Siena","McNeese",
    "North Dakota State","Hawai'i","North Carolina","Howard","BYU","Saint Mary's",
    "Penn","Georgia","Kennesaw State","Idaho",
    "Santa Clara","Akron","Long Island University","Wright State","Tennessee State","Hofstra",
    "Villanova","Miami (Ohio)","Clemson","UNI","Queens","UCF",
    "Prairie View A&M","Cal Baptist","Missouri","Furman",
    "Saint Louis","Louisville",
    "TCU","Texas A&M","Gonzaga","VCU",
    "Vanderbilt","High Point",
    "Miami (Fla.)","Kentucky","Kansas","Virginia","Florida","Utah State",
    "UCLA","Texas Tech",
    "Texas","Nebraska","Arkansas","Houston",
    "St. John's","Alabama","Michigan State","Iowa State",
    "Iowa","Purdue",
    "Tennessee","Duke",
    "Illinois","Arizona",
    "UConn"
  ),
  LScore = c(
    83,66,55,79,
    64,47,79,82,65,68,
    67,78,78,80,71,50,
    70,77,64,47,
    84,71,58,73,74,70,
    76,56,61,53,71,71,
    55,60,66,71,
    72,69,
    58,57,68,55,
    72,88,
    69,63,65,72,72,66,
    57,65,
    77,71,88,55,
    75,77,63,62,
    59,64,
    62,72,
    62,73,
    63
  ),
  NumOT = c(
    0,0,0,0,
    0,0,0,0,0,0,
    0,0,1,0,0,0,
    0,0,0,0,
    1,0,0,0,0,0,
    0,0,0,0,0,0,
    0,0,0,0,
    0,0,0,0,0,0,
    0,0,
    0,0,0,0,0,0,
    0,0,
    0,0,0,0,
    0,0,0,0,
    0,0,
    0,0,
    0,0,
    0
  ),
  WLoc = "N"
)

Wteam_ids <- c(
  1224, 1400, 1341, 1275, 
  1395, 1304, 1257, 1219, 1181, 1435,
  1277, 1116, 1433, 1276, 1400, 1401,
  1228, 1387, 1211, 1222,
  1246, 1403, 1112, 1438, 1235, 1104,
  1429, 1397, 1234, 1385, 1345, 1417,
  1196, 1242, 1274, 1163,
  1276, 1277, 1181, 1222, 1400, 1228, 1304, 1116,
  1345, 1235, 1385, 1397, 1234, 1112, 1163, 1104,
  1345, 1234, 1112, 1228, 1181, 1276, 1163, 1397,
  1228, 1112, 1276, 1163, 1163, 1276, 1276
)

Lteam_ids <- c(
  1420, 1301, 1250, 1374, 1326, 1407, 1378, 1458, 1373, 1270,
  1295, 1218, 1314, 1224, 1140, 1388, 1335, 1208, 1244, 1225,
  1365, 1103, 1254, 1460, 1398, 1220, 1437, 1275, 1155, 1320,
  1474, 1416, 1341, 1465, 1281, 1202, 1387,
  1257, 1395, 1401, 1211, 1433,
  1435, 1219,
  1274, 1246, 1242, 1438, 1196, 1429,
  1417, 1403,
  1400, 1304, 1116, 1222,
  1385, 1104, 1277, 1235,
  1234, 1345, 1397, 1181, 1228, 1112, 1163
)

games_2026 <- games_2026 %>%
  mutate(
    WTeamID = Wteam_ids,
    LTeamID = Lteam_ids
  ) %>%
  select(Season, DayNum, WTeamID, WScore, LTeamID, LScore, WLoc, NumOT)

data$MNCAATourneyCompactResults <- bind_rows(
  data$MNCAATourneyCompactResults,
  games_2026
)

write.csv(
  data$MNCAATourneyCompactResults,
  file = "csv/MNCAATourneyCompactResults.csv",
  row.names = FALSE
)



# cleaning public picks ---------------------------------------------------

consensus_2026 <- read.csv("csv/Public Picks.csv") %>% 
  rename(round1 = R64,
         round2 = R32,
         round3 = S16,
         round4 = E8,
         round5 = F4,
         round6 = FINALS,
         year   = YEAR,
         name   = TEAM) %>% 
  mutate(across(starts_with("round"), ~
                  as.numeric(gsub("[^0-9.]", "", .)) / 100
  ))

consensus_2025 <- read.csv("Rstudio work/consensus_2025.csv") %>% mutate(year = 2025)
consensus_2024 <- read.csv("Rstudio work/consensus_2024.csv") %>% mutate(year = 2024)
consensus_2023 <- read.csv("Rstudio work/consensus_2023.csv") %>% mutate(year = 2023)
consensus_2022 <- read.csv("Rstudio work/consensus_2022.csv") %>% mutate(year = 2022)
consensus_2021 <- read.csv("Rstudio work/consensus_2021.csv") %>% mutate(year = 2021)

consensus_2016 <- read.csv("Rstudio work/consensus_2016.csv") %>% mutate(year = 2016)
consensus_2017 <- read.csv("Rstudio work/consensus_2017.csv") %>% mutate(year = 2017)
consensus_2018 <- read.csv("Rstudio work/consensus_2018.csv") %>% mutate(year = 2018)

# COMBINE ALL CONSENSUS DATA----------------------------
consensus1 <- bind_rows(
  consensus_2016,
  consensus_2017,
  consensus_2018,
  consensus_2021,
  consensus_2022,
  consensus_2023,
  consensus_2024,
  consensus_2025,
  consensus_2026
)

# FIX PLAY-IN GAMES-------------------------
consensus_2021_2026 <- consensus1 %>%
  select(year, name, round1, round2, round3, round4, round5, round6) %>%
  mutate(name = case_when(
    name == "MSM/Texas Southern" ~ "Texas Southern",
    name == "Michigan State/UCLA" ~ "UCLA",
    name == "NORF/APP" ~ "Norfolk St",
    name == "Wichita State/Drake" ~ "Drake",
    name == "AMCC/SMO" ~ "Texas A&M Corpus Christi",
    name == "ASU/NEV" ~ "Arizona St",
    name == "MSST/PITT" ~ "Pitt",
    name == "TXSO/FDU" ~ "FDU",
    TRUE ~ name
  ))

# CLEAN NAMES---------------------------
consensus_clean <- consensus_2021_2026 %>%
  mutate(TeamName = tolower(trimws(name)))

# SPELLINGS TABLE-------------------------------
spellings <- data$MTeamSpellings %>%
  mutate(TeamNameSpelling = tolower(trimws(
    iconv(TeamNameSpelling, from = "latin1", to = "UTF-8", sub = " ")
  ))) %>%
  group_by(TeamNameSpelling) %>%
  summarise(TeamID = first(TeamID), .groups = "drop")

# MANUAL TEAM MAPPINGS-------------------------------
manual_team_map <- c(
  # Louisiana Lafayette
  "louisiana lafayette" = 1418, "louisiana-lafayette" = 1418, "ull" = 1418,
  # Illinois Chicago
  "illinois chicago" = 1227, "illinois-chicago" = 1227, "uic" = 1227,
  # Texas A&M Corpus Christi
  "texas a&m corpus chris" = 1394, "texas a&m-corpus christi" = 1394,
  "a&m-corpus chris" = 1394, "texas a&m-cc" = 1394, "corpus christi" = 1394,
  "texas a&m corpus christi" = 1394, "texas a&m cc" = 1394,
  # Mississippi Valley State
  "mississippi valley st." = 1290, "mississippi valley state" = 1290, "miss valley st." = 1290,
  # Arkansas Pine Bluff
  "arkansas pine bluff" = 1115, "ark pine bluff" = 1115, "uapb" = 1115,
  # Arkansas Little Rock
  "arkansas little rock" = 1114, "ark little rock" = 1114, "ualr" = 1114,
  "little rock" = 1114, "ar-little rock" = 1114,
  # Cal State Bakersfield
  "cal st. bakersfield" = 1167, "cal state bakersfield" = 1167,
  "csu bakersfield" = 1167, "cs bakersfield" = 1167, "bakersfield" = 1167,
  # Southeast Missouri State
  "southeast missouri st." = 1369, "southeast missouri state" = 1369,
  "se missouri st" = 1369, "se missouri state" = 1369, "semo" = 1369,
  # Texas State
  "texas state" = 1402, "texas st." = 1402, "southwest texas st." = 1402, "southwest texas state" = 1402,
  # UT Rio Grande Valley
  "ut rio grande valley" = 1410, "texas-rio grande valley" = 1410, "utrgv" = 1410,
  "texas pan american" = 1410, "texas-pan american" = 1410,
  # IUPUI / IU Indy
  "iupui" = 1237, "iu indy" = 1237, "indianapolis" = 1237,
  # LIU
  "liu" = 1254, "long island" = 1254, "long island university" = 1254, "liu brooklyn" = 1254,
  # Queens
  "queens" = 1474, "queens nc" = 1474, "queens university" = 1474,
  # Saint Francis (NY)
  "st. francis ny" = 1383, "st francis ny" = 1383, "saint francis ny" = 1383,
  "st. francis (ny)" = 1383, "saint francis (ny)" = 1383,
  # Saint Francis (PA) - CORRECTED to 1384
  "st. francis pa" = 1384, "st francis pa" = 1384, "saint francis pa" = 1384,
  "st. francis (pa)" = 1384, "saint francis (pa)" = 1384,
  # Green Bay
  "green bay" = 1453, "wisconsin green bay" = 1453, "wisconsin-green bay" = 1453,
  # Milwaukee
  "milwaukee" = 1454, "wisconsin milwaukee" = 1454, "wisconsin-milwaukee" = 1454,
  # Utah Tech / Dixie State
  "utah tech" = 1469, "dixie st." = 1469, "dixie state" = 1469,
  # Texas A&M Commerce
  "texas a&m commerce" = 1477, "tx a&m commerce" = 1477, "east texas a&m" = 1477,
  # Missouri State
  "missouri state" = 1283, "missouri st." = 1283, "southwest missouri st." = 1283,
  # McNeese
  "mcneese" = 1270, "mcneese st." = 1270, "mcneese state" = 1270,
  # Nicholls
  "nicholls" = 1311, "nicholls st." = 1311, "nicholls state" = 1311,
  # Troy
  "troy" = 1407, "troy st." = 1407,
  # Utah Valley
  "utah valley" = 1430, "utah valley st." = 1430, "utah valley state" = 1430,
  # Detroit Mercy
  "detroit mercy" = 1178, "detroit" = 1178, "detroit-mercy" = 1178,
  # SIU Edwardsville
  "siue" = 1188, "siu edwardsville" = 1188, "southern illinois edwardsville" = 1188,
  # UMKC / Kansas City
  "umkc" = 1282, "kansas city" = 1282, "missouri-kansas city" = 1282,
  # Purdue Fort Wayne
  "purdue fort wayne" = 1236, "ipfw" = 1236, "fort wayne" = 1236, "pfw" = 1236,
  # Charleston
  "charleston" = 1158, "college of charleston" = 1158,
  # CSUN (Cal State Northridge)
  "csun" = 1169, "cal st. northridge" = 1169, "cal state northridge" = 1169,
  # Middle Tennessee - FIXED to 1292 (not conflicting with CSUN)
  "middle tennessee" = 1292, "middle tennessee st." = 1292, "mtsu" = 1292, "mid tennessee" = 1292,
  # Louisiana Monroe
  "louisiana monroe" = 1419,
  # Additional tournament-relevant teams (ALL IDs CORRECTED)
  "bethune-cookman" = 1398,
  "maryland - e. shore" = 1271,  # Maryland Eastern Shore
  "tarleton st" = 1470,
  "tarleton state" = 1470,
  "st. thomas" = 1472,
  "winston salem st." = 1445,
  "tennessee martin" = 1404,  # UT Martin
  # New additions
  "abil christian" = 1101, "abilene christian" = 1101,
  "fsu" = 1199, "florida state" = 1199,
  "osu" = 1326, "ohio state" = 1326,
  "uva" = 1438, "virginia" = 1438,
  "csu fullerton" = 1168, "cs fullerton" = 1168,
  "j'ville st" = 1240, "jacksonville st." = 1240,
  "miami" = 1274, "miami fl" = 1274,
  "fau" = 1194, "florida atlantic" = 1194,
  "kennesaw st" = 1244, "kennesaw state" = 1244,
  "western ky" = 1443, "western kentucky" = 1443,
  "mount st marys" = 1291, "mount st. marys" = 1291,
  # Extras (ALL IDs CORRECTED)
  "cal" = 1143,
  "saint joe's" = 1386,
  "south dakota st" = 1355,
  "ksu" = 1243,
  "msm" = 1291,
  "uri" = 1348
)

# CONVERT TO DATAFRAME-------------------------------
manual_mappings <- data.frame(
  TeamName = names(manual_team_map),
  TeamID = as.integer(manual_team_map),
  stringsAsFactors = FALSE
) %>%
  distinct(TeamName, .keep_all = TRUE)

# EXACT MATCH-------------------------------
exact_consensus <- consensus_clean %>%
  left_join(spellings, by = c("TeamName" = "TeamNameSpelling")) %>%
  filter(!is.na(TeamID))

# MANUAL MATCH-------------------------------
manual_consensus <- consensus_clean %>%
  anti_join(exact_consensus, by = "TeamName") %>%
  left_join(manual_mappings, by = "TeamName") %>%
  filter(!is.na(TeamID))

# FINAL LOOKUP-------------------------------
consensus_with_ids <- bind_rows(exact_consensus, manual_consensus)

# SAFETY CHECK-------------------------------
missing_ids <- consensus_clean %>%
  anti_join(consensus_with_ids, by = c("year", "name", "round1", "round2", "round3", "round4", "round5", "round6", "TeamName"))

if(nrow(missing_ids) > 0){
  cat("WARNING: These teams are still missing TeamIDs:\n")
  print(missing_ids %>% select(year, name, TeamName) %>% distinct())
  stop("ERROR: Some teams are still missing TeamIDs")
} else {
  cat("all teams successfully matched with TeamIDs")
}


# READY FOR MODELING-------------------------------
df <- consensus_with_ids %>%
  mutate(across(starts_with("round"), as.numeric))

# 1. BUILD TEAM STRENGTH (FROM ROUND PROBS)-------------------------------
df_strength <- df %>%
  mutate(
    strength =
      1  * round1 +
      2  * round2 +
      4  * round3 +
      8  * round4 +
      16 * round5 +
      32 * round6
  ) %>%
  group_by(year) %>%
  mutate(
    # normalize within season
    strength = strength / max(strength, na.rm = TRUE)
  ) %>%
  ungroup()


# 2. CONVERT TO ELO-STYLE RATING-------------------------------
df_strength <- df_strength %>%
  mutate(
    elo = 1500 + 400 * log(strength + 1e-6)
  )

# 3. CREATE ALL PAIRWISE MATCHUPS -------------------------------
pairwise_probs <- df_strength %>%
  select(year, TeamID, elo) %>%
  group_by(year) %>%
  group_modify(~{
    
    teams <- .x
    
    expand_grid(
      TeamA = teams$TeamID,
      TeamB = teams$TeamID
    ) %>%
      filter(TeamA != TeamB) %>%
      
      # Join Team A
      left_join(teams, by = c("TeamA" = "TeamID")) %>%
      rename(elo_A = elo) %>%
      
      # Join Team B
      left_join(teams, by = c("TeamB" = "TeamID")) %>%
      rename(elo_B = elo) %>%
      
      # 4. ELO WIN PROBABILITY-------------------------------
    mutate(
      matchup_prob = 1 / (1 + 10 ^ ((elo_B - elo_A) / 500)) #adjust to 400 for more overconfidence
    )
    
  }) %>%
  ungroup()

# 5. OPTIONAL: QUICK LOOKUP TABLE-------------------------------
pairwise_lookup <- pairwise_probs %>%
  select(year, TeamA, TeamB, matchup_prob)

write_csv(pairwise_lookup, "pairwise_lookup.csv")

# cleaning kenpom to add to team_season_stats -----------------------------
# Clean, streamlined version that ensures all tournament teams (2003+) have TeamIDs
# Now includes AdjOE, AdjDE, AdjTempo and RankAdjEM

library(dplyr)
library(stringdist)
library(readr)
library(purrr)
library(tibble)
library(randomForest)
library(stringr)
library(tidyr)
library(glmnet)

data_dir <- "C:/Users/aaron/OneDrive/Marchmadness2025project/csv"
files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

data <- files %>%
  set_names(~ str_remove(basename(.), "\\.csv$")) %>%
  map(~ read_csv(.x, show_col_types = FALSE))

# --- 1. LOAD KENPOM DATA WITH ALL STATS ---
kenpom1 <- read.csv("C:/Users/aaron/OneDrive/Marchmadness2025project/kenpom/INT_KenPom_Summary_Pre_Tourny.csv") %>%
  select(Season, TeamName, AdjEM, RankAdjEM, AdjOE, AdjDE, AdjTempo) %>%
  mutate(TeamName = tolower(trimws(TeamName)))

#---------- 2026 years kenpom
kenpom2 <- read.csv("C:/Users/aaron/OneDrive/Marchmadness2025project/kenpom/INT_KenPom_Summary_2026.csv") %>%
  select(Season, TeamName, AdjEM, RankAdjEM, AdjOE, AdjDE, AdjTempo) %>%
  filter(Season==2026) %>% 
  mutate(TeamName = tolower(trimws(TeamName)))

kenpom <- bind_rows(kenpom1,kenpom2)

# Load spellings and team info
spellings <- data$MTeamSpellings %>%
  mutate(TeamNameSpelling = tolower(trimws(
    iconv(TeamNameSpelling, from = "latin1", to = "UTF-8", sub = " ")
  ))) %>%
  distinct(TeamNameSpelling, .keep_all = TRUE)

# --- 2. COMPREHENSIVE MANUAL MAPPINGS ---
# These handle all known name variations and historical name changes
manual_mappings <- data.frame(
  TeamName = c(
    # Louisiana Lafayette variations
    "louisiana lafayette", "louisiana-lafayette", "ull",
    # Illinois Chicago variations
    "illinois chicago", "illinois-chicago", "uic",
    # Texas A&M Corpus Christi
    "texas a&m corpus chris", "texas a&m-corpus christi", "a&m-corpus chris", "texas a&m-cc", "corpus christi",
    # Mississippi Valley State
    "mississippi valley st.", "mississippi valley state", "miss valley st.",
    # Arkansas Pine Bluff
    "arkansas pine bluff", "ark pine bluff", "uapb",
    # Arkansas Little Rock
    "arkansas little rock", "ark little rock", "ualr", "little rock",
    # Cal State Bakersfield
    "cal st. bakersfield", "cal state bakersfield", "csu bakersfield", "cs bakersfield", "bakersfield",
    # Southeast Missouri State
    "southeast missouri st.", "southeast missouri state", "se missouri st", "se missouri state", "semo",
    # Texas State
    "texas state", "texas st.", "southwest texas st.", "southwest texas state",
    # UT Rio Grande Valley
    "ut rio grande valley", "texas-rio grande valley", "utrgv", "texas pan american", "texas-pan american",
    # IUPUI / IU Indy
    "iupui", "iu indy", "indianapolis",
    # LIU
    "liu", "long island", "long island university", "liu brooklyn",
    # Queens
    "queens", "queens nc", "queens university",
    # Saint Francis (NY)
    "st. francis ny", "st francis ny", "saint francis ny", "st. francis (ny)", "saint francis (ny)",
    # Saint Francis (PA)
    "st. francis pa", "st francis pa", "saint francis pa", "st. francis (pa)", "saint francis (pa)",
    # Green Bay
    "green bay", "wisconsin green bay", "wisconsin-green bay",
    # Milwaukee
    "milwaukee", "wisconsin milwaukee", "wisconsin-milwaukee",
    # Utah Tech / Dixie State
    "utah tech", "dixie st.", "dixie state",
    # Texas A&M Commerce
    "texas a&m commerce", "tx a&m commerce", "east texas a&m",
    # Missouri State
    "missouri state", "missouri st.", "southwest missouri st.",
    # McNeese
    "mcneese", "mcneese st.", "mcneese state",
    # Nicholls
    "nicholls", "nicholls st.", "nicholls state",
    # Troy
    "troy", "troy st.",
    # Utah Valley
    "utah valley", "utah valley st.", "utah valley state",
    # Detroit Mercy
    "detroit mercy", "detroit", "detroit-mercy",
    # SIU Edwardsville
    "siue", "siu edwardsville", "southern illinois edwardsville",
    # UMKC / Kansas City
    "umkc", "kansas city", "missouri-kansas city",
    # Purdue Fort Wayne
    "purdue fort wayne", "ipfw", "fort wayne", "pfw",
    # Charleston
    "charleston", "college of charleston",
    # CSUN
    "csun", "cal st. northridge", "cal state northridge",
    # Middle Tennessee
    "middle tennessee", "middle tennessee st.", "mtsu",
    # Additional tournament-relevant teams
    "louisiana monroe", "st francis (pa)", "bethune-cookman", 
    "maryland - e. shore", "tarleton st", "tarleton state",
    "st. thomas", "tarleton st.", "winston salem st.", "tennessee martin"
  ),
  TeamID = c(
    rep(1418, 3), rep(1227, 3), rep(1394, 5), rep(1290, 3),
    rep(1115, 3), rep(1114, 4), rep(1167, 5), rep(1369, 5),
    rep(1402, 4), rep(1410, 5), rep(1237, 3), rep(1254, 4),
    rep(1474, 3), rep(1383, 5), rep(1382, 5),
    rep(1453, 3), rep(1454, 3), rep(1469, 3), rep(1477, 3),
    rep(1283, 3), rep(1270, 3), rep(1311, 3), rep(1407, 2),
    rep(1430, 3), rep(1178, 3), rep(1188, 3), rep(1282, 3),
    rep(1236, 4), rep(1158, 2), rep(1169, 3), rep(1292, 3),
    # Additional IDs
    1398, 1384, 1126, 1271, 1470, 1470, 1384, 1468, 1475, 1404
  ),
  stringsAsFactors = FALSE
) %>%
  distinct(TeamName, .keep_all = TRUE)

# --- 3. EXACT MATCHES ---
exact <- kenpom %>%
  left_join(spellings, by = c("TeamName" = "TeamNameSpelling")) %>%
  filter(!is.na(TeamID)) %>%
  mutate(method = "exact")

# --- 4. MANUAL MATCHES (apply first) ---
manual <- kenpom %>%
  filter(TeamName %in% manual_mappings$TeamName) %>%
  left_join(manual_mappings, by = "TeamName") %>%
  mutate(method = "manual")

# --- 5. FUZZY MATCH FOR REMAINING ---
already_matched <- unique(c(exact$TeamName, manual$TeamName))
fuzzy_candidates <- kenpom %>%
  filter(!TeamName %in% already_matched) %>%
  distinct(TeamName) %>%
  pull(TeamName)

fuzzy_list <- list()
for (team in fuzzy_candidates) {
  dists <- stringdist(team, spellings$TeamNameSpelling, method = "jw")
  best <- which.min(dists)
  if (dists[best] <= 0.2) {
    # Get the matched spelling and TeamID
    matched_spelling <- spellings$TeamNameSpelling[best]
    matched_id <- spellings$TeamID[best]
    
    # Retrieve the full KenPom row for that team (Season, AdjEM, RankAdjEM, AdjOE, AdjDE, AdjTempo)
    full_row <- kenpom %>% filter(TeamName == team)
    if (nrow(full_row) > 0) {
      fuzzy_list[[team]] <- full_row %>%
        mutate(
          TeamID = matched_id,
          matched_spelling = matched_spelling,
          dist = dists[best],
          method = "fuzzy"
        )
    }
  }
}
fuzzy <- bind_rows(fuzzy_list)

# --- 6. COMBINE ALL MATCHES ---
# Join back to full KenPom data (keeping all stats including RankAdjEM)
kenpom_final <- bind_rows(
  exact %>% select(Season, TeamName, AdjEM, RankAdjEM, AdjOE, AdjDE, AdjTempo, TeamID, method),
  manual %>% select(Season, TeamName, AdjEM, RankAdjEM, AdjOE, AdjDE, AdjTempo, TeamID, method),
  fuzzy %>% select(Season, TeamName, AdjEM, RankAdjEM, AdjOE, AdjDE, AdjTempo, TeamID, method)
) %>%
  distinct(Season, TeamID, .keep_all = TRUE) %>%   # one row per team-season
  arrange(Season, TeamID)

# --- 8. FINAL VERIFICATION ---
cat("\n", strrep("=", 60), "\n")
cat("FINAL KENPOM DATASET\n")
cat(strrep("=", 60), "\n")
cat("Rows:", nrow(kenpom_final), "\n")
cat("Unique teams:", n_distinct(kenpom_final$TeamID), "\n")
cat("Seasons:", min(kenpom_final$Season, na.rm=TRUE), "-", max(kenpom_final$Season, na.rm=TRUE), "\n")

# Verify tournament teams (2003+)
tourney_teams <- data$MNCAATourneySeeds %>%
  filter(Season >= 2003) %>%
  distinct(TeamID) %>%
  pull(TeamID)

missing <- setdiff(tourney_teams, kenpom_final$TeamID)
if (length(missing) == 0) {
  cat("\n✅ TOURNAMENT TEAMS (2003+): ALL", length(tourney_teams), "PRESENT\n")
} else {
  cat("\n⚠️ Missing tournament teams:", length(missing), "\n")
  cat("Missing IDs:", paste(missing, collapse = ", "), "\n")
}


# --- 9. SAVE FINAL DATASET (optional) ---
write.csv(kenpom_final, "kenpom_tournament_ready.csv", row.names = FALSE)
# cat("\n📁 Saved to kenpom_tournament_ready.csv\n")

# Preview
cat("\nFirst few rows:\n")


print(head(kenpom_final, 10))


# team season stats -------------------------------------------------------
kenpom_final <- read.csv("Rstudio work/kenpom_tournament_ready.csv")

rs <- data$MRegularSeasonDetailedResults

team_games <- bind_rows(
  rs %>%
    transmute(
      Season, DayNum,
      TeamID = WTeamID, OpponentID = LTeamID,
      Win = 1, NumOT,
      Points = WScore, OppPoints = LScore,
      FGM = WFGM, FGA = WFGA, FGM3 = WFGM3, FGA3 = WFGA3,
      FTM = WFTM, FTA = WFTA,
      OR = WOR, DR = WDR,
      Ast = WAst, TO = WTO,
      Stl = WStl, Blk = WBlk,
      PF = WPF
    ),
  rs %>%
    transmute(
      Season, DayNum,
      TeamID = LTeamID, OpponentID = WTeamID,
      Win = 0, NumOT,
      Points = LScore, OppPoints = WScore,
      FGM = LFGM, FGA = LFGA, FGM3 = LFGM3, FGA3 = LFGA3,
      FTM = LFTM, FTA = LFTA,
      OR = LOR, DR = LDR,
      Ast = LAst, TO = LTO,
      Stl = LStl, Blk = LBlk,
      PF = LPF
    )
)

team_season_stats <- team_games %>%
  group_by(Season, TeamID) %>%
  summarise(
    Games = n(),
    WinPct = mean(Win),
    AvgPoints = median(Points),
    AvgOppPoints = median(OppPoints),
    FG2_Pct = sum(FGM - FGM3) / sum(FGA - FGA3),
    FG3_Pct = sum(FGM3) / sum(FGA3),
    FT_Pct = sum(FTM) / sum(FTA),
    ThreeP_Rate = sum(FGA3) / sum(FGA),
    OR_pg = median(OR),
    DR_pg = median(DR),
    Ast_pg = median(Ast),
    TO_pg = median(TO),
    Stl_pg = median(Stl),
    Blk_pg = median(Blk),
    PF_pg = median(PF),
    AvgOT = mean(NumOT),
    .groups = "drop"
  )

# Filter to post-2002
team_season_stats <- team_season_stats %>%
  filter(Season >= 2003)

# Strength of victories -----------------------------------------------------
# First, get the total number of teams per season for percentile calculation
teams_per_season <- kenpom_final %>%
  group_by(Season) %>%
  summarise(n_teams = n(), .groups = "drop")

# Prepare opponent KenPom ranks and join team count
opp_kenpom <- kenpom_final %>%
  select(Season, TeamID, Opp_RankAdjEM = RankAdjEM) %>%
  left_join(teams_per_season, by = "Season")

# Compute opponent percentile: (n_teams - rank + 1) / n_teams
opp_kenpom <- opp_kenpom %>%
  mutate(Opp_Percentile = (n_teams - Opp_RankAdjEM + 1) / n_teams) %>%
  select(Season, TeamID, Opp_Percentile)

# Join opponent percentiles to each game
team_games_with_opp <- team_games %>%
  left_join(opp_kenpom, by = c("Season", "OpponentID" = "TeamID"))

# Compute scaled SOV: average percentile of opponents that the team defeated
sov_scaled <- team_games_with_opp %>%
  filter(Win == 1) %>%
  group_by(Season, TeamID) %>%
  summarise(
    SOV_Scaled = mean(Opp_Percentile, na.rm = TRUE),
    .groups = "drop"
  )

# coach feature -----------------------------------------------------------
# Get all coach data from MTeamCoaches
all_coaches <- data$MTeamCoaches %>%
  select(Season, TeamID, CoachName) %>%
  distinct() %>%
  arrange(CoachName, Season)

# Get tournament wins for each coach-season
tourney_wins <- data$MNCAATourneyDetailedResults %>%
  # Get primary coach for the winning team
  inner_join(
    data$MTeamCoaches %>%
      mutate(DaysCoached = LastDayNum - FirstDayNum + 1) %>%
      group_by(Season, TeamID) %>%
      arrange(desc(DaysCoached)) %>%
      dplyr::slice(1) %>%
      ungroup() %>%
      select(Season, TeamID, PrimaryCoach = CoachName),
    by = c("Season", "WTeamID" = "TeamID")
  ) %>%
  # Count wins per primary coach per season
  group_by(PrimaryCoach, Season) %>%
  summarise(SeasonWins = n(), .groups = "drop") %>%
  rename(CoachName = PrimaryCoach)

# Calculate coach career statistics (using ALL historical data)
coach_career_stats <- all_coaches %>%
  select(CoachName, Season) %>%
  distinct() %>%
  left_join(tourney_wins, by = c("CoachName", "Season")) %>%
  mutate(SeasonWins = coalesce(SeasonWins, 0)) %>%
  arrange(CoachName, Season) %>%
  group_by(CoachName) %>%
  mutate(
    # Career wins BEFORE current season
    CareerTourneyWins = lag(cumsum(SeasonWins), default = 0),
    # Years coached BEFORE current season
    YearsNCAA = row_number() - 1,
    .groups = "drop"
  ) %>%
  select(CoachName, Season, YearsNCAA, CareerTourneyWins)

# Get primary coach for each team-season (for seasons >= 2003 to match team_season_stats)
primary_coaches <- data$MTeamCoaches %>%
  filter(Season >= 2003) %>%
  mutate(DaysCoached = LastDayNum - FirstDayNum + 1) %>%
  group_by(Season, TeamID) %>%
  arrange(desc(DaysCoached)) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(Season, TeamID, CoachName)

# Join with existing team_season_stats
team_season_stats <- team_season_stats %>%
  # Remove any existing coach columns if they exist
  select(-any_of(c("CoachName", "YearsNCAA", "CareerTourneyWins"))) %>%
  # Join with primary coaches
  left_join(primary_coaches, by = c("Season", "TeamID")) %>%
  # Join with career stats
  left_join(coach_career_stats, by = c("CoachName", "Season")) %>%
  # Fill NA values with 0
  mutate(
    YearsNCAA = coalesce(YearsNCAA, 0),
    CareerTourneyWins = coalesce(CareerTourneyWins, 0)
  )

# add KenPom advanced stats and sov_scaled------------------------------------------------
team_season_stats <- team_season_stats %>%
  left_join(
    kenpom_final %>% 
      select(Season, TeamID, AdjEM, AdjOE, AdjDE, AdjTempo, RankAdjEM) %>%
      mutate(TeamID = as.integer(TeamID)),
    by = c("Season", "TeamID")
  )

team_season_stats <- team_season_stats %>%
  left_join(sov_scaled, by = c("Season", "TeamID")) %>%
  rename(SOV = SOV_Scaled)

# add seed info -----------------------------------------------------------
# Get seed data and extract numeric seed value - SIMPLIFIED: only raw seed number
seed_data <- data$MNCAATourneySeeds %>%
  filter(Season >= 2003) %>%
  mutate(
    # Extract numeric seed (remove region letters)
    Seed_Num = as.numeric(str_extract(Seed, "\\d+"))
  )

# Join seed information with team_season_stats
team_season_stats <- team_season_stats %>%
  left_join(seed_data %>% select(Season, TeamID, Seed_Num),
            by = c("Season", "TeamID"))

# For teams that didn't make the tournament (no seed), fill with appropriate value
team_season_stats <- team_season_stats %>%
  mutate(
    Seed_Num = ifelse(is.na(Seed_Num), 17, Seed_Num)  # Assign 17 to teams that didn't make the tournament
  )