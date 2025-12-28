library(tidyverse)
library(rvest)
library(wehoop)


#Read in 2025-26 Advanced School Stats for NCAAW from basketball reference
school_advanced_url <- paste0("https://www.sports-reference.com/cbb/seasons/women/2026-advanced-school-stats.html")
school_advanced_html <- read_html(school_advanced_url)
school_advanced_tables <- html_table(school_advanced_html)
school_advanced_off <- school_advanced_tables[[1]]

#Read in 2025-26 Advanced Opponent School Stats for NCAAW from basketball reference
opp_advanced_url <- paste0("https://www.sports-reference.com/cbb/seasons/women/2026-advanced-opponent-stats.html")
opp_advanced_html <- read_html(opp_advanced_url)
opp_advanced_tables <- html_table(opp_advanced_html)
school_advanced_def <- opp_advanced_tables[[1]]

#Change columns names to the data in row 1
colnames(school_advanced_off) <- school_advanced_off[c(1),]

colnames(school_advanced_def) <- school_advanced_def[c(1),]

#Change certain column names so they syntactically work
colnames(school_advanced_off)[c(6, 10:11, 13:14, 16:17, 19:20, 22:34)] <- c("win_per", "conf_W", "conf_L", "home_W", "home_L", "away_W", "away_L", "points_for", "points_against", "pace", "off_rtg", "FT_rate", "rate_3", "TS_per", "TRB_rate", "AST_per", "STL_per", "BLK_per", "EFG_per", "TO_rate", "ORB_rate", "FTM_per_FGA")

colnames(school_advanced_def)[c(6, 10:11, 13:14, 16:17, 19:20, 22:34)] <- c("win_per", "conf_W", "conf_L", "home_W", "home_L", "away_W", "away_L", "points_for", "points_against", "pace", "def_rtg", "def_FT_rate", "def_rate_3", "def_TS_per", "opp_TRB_rate", "opp_AST_per", "opp_STL_per", "opp_BLK_per", "def_EFG_per", "def_TO_rate", "opp_ORB_rate", "def_FTM_per_FGA")


#Remove empty columns from advanced stats (basketball reference)
school_advanced_off <- school_advanced_off |> select(-matches("^NA$"))

school_advanced_def <- school_advanced_def |> select(-matches("^NA$"))

#Now remove rows that are just the variable names
school_advanced_off <- school_advanced_off |> filter(Rk != "Rk")
school_advanced_off <- school_advanced_off |> filter(Rk != 0)
school_advanced_off <- school_advanced_off |> filter(G != "Overall")

school_advanced_def <- school_advanced_def |> filter(Rk != "Rk")
school_advanced_def <- school_advanced_def |> filter(Rk != 0)
school_advanced_def <- school_advanced_def |> filter(G != "Overall")


#All variables started as character, so change variables to numeric where necessary
school_advanced_off <- school_advanced_off |> mutate(across(c(1, 3:29), as.numeric))

school_advanced_def <- school_advanced_def |> mutate(across(c(1, 3:29), as.numeric))

#Replace NA values with 0s in school stats and advanced (basketball reference)
school_advanced_off <- school_advanced_off |> mutate(across(where(is.numeric), ~replace_na(., 0)))

school_advanced_def <- school_advanced_def |> mutate(across(where(is.numeric), ~replace_na(., 0)))


advanced_stats_off <- school_advanced_off |> 
  mutate(
    ORB_rate = ORB_rate/100,
    TO_rate = TO_rate/100
  ) |> 
  dplyr::select(School, off_rtg, EFG_per, TO_rate, ORB_rate, FT_rate)

advanced_stats_def <- school_advanced_def |> 
  mutate(
    DRB_rate = (100 - opp_ORB_rate)/100,
    def_TO_rate = def_TO_rate/100
  ) |> 
  dplyr::select(School, def_rtg, def_EFG_per, def_TO_rate, DRB_rate, def_FT_rate)




# now read in shot type and play type stats from Synergy
# shot types
# read in at rim shots for offenses
at_rim_off_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - At Rim - Team Offensive.csv", skip = 1)

# read in at rim shots for defenses
at_rim_def_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - At Rim - Team Defensive.csv", skip = 1)

# read in runners on offense
runners_off_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Runner - Team Offensive.csv", skip = 1)

# read in runners on defense
runners_def_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Runner - Team Defensive.csv", skip = 1)

# read in hook shots on offense
hook_off_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Hook - Team Offensive.csv", skip = 1)

# read in hook shots on defense
hook_def_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Hook - Team Defensive.csv", skip = 1)

# read in all jump shots on offense
jump_shots_off_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Jump Shots - Team Offensive.csv", skip = 1)

# read in all jump shots on defense
jump_shots_def_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Jump Shots - Team Defensive.csv", skip = 1)

# read in short mid-range shots on offense
short_mid_off_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Short Mid-Range - Team Offensive.csv", skip = 1)

# read in short mid-range shots on defense
short_mid_def_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Short Mid-Range - Team Defensive.csv", skip = 1)

# read in long mid-range shots on offense
long_mid_off_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Long Mid-Range - Team Offensive.csv", skip = 1)

# read in long mid-range shots on defense
long_mid_def_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - Long Mid-Range - Team Defensive.csv", skip = 1)

# read in 3 pointers on offense
threes_off_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - 3 Pointers - Team Offensive.csv", skip = 1)

# read in 3 pointers on defense
threes_def_raw <- read_csv("Synergy Data/Shot Types/College Women 2025-2026 - 3 Pointers - Team Defensive.csv", skip = 1)

# manipulate at rim shots on offense to only get variables we want in the format we want
at_rim_off <- at_rim_off_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    tm_rim_freq = `%Time` / 100,
    tm_rim_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    tm_rim_freq,
    tm_rim_FG_per
  )

# manipulate at rim shots on defense to only get variables we want in the format we want
at_rim_def <- at_rim_def_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    opp_rim_freq = `%Time` / 100,
    opp_rim_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    opp_rim_freq,
    opp_rim_FG_per
  )

# manipulate at rim shots on offense to only get variables we want in the format we want
runners_off <- runners_off_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    tm_runner_freq = `%Time` / 100,
    tm_runner_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    tm_runner_freq,
    tm_runner_FG_per
  )

# manipulate at rim shots on defense to only get variables we want in the format we want
runners_def <- runners_def_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    opp_runner_freq = `%Time` / 100,
    opp_runner_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    opp_runner_freq,
    opp_runner_FG_per
  )

# manipulate hook shots on offense to only get variables we want in the format we want
hook_off <- hook_off_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    tm_hook_freq = `%Time` / 100,
    tm_hook_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    tm_hook_freq,
    tm_hook_FG_per
  )

# manipulate hook shots on defense to only get variables we want in the format we want
hook_def <- hook_def_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    opp_hook_freq = `%Time` / 100,
    opp_hook_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    opp_hook_freq,
    opp_hook_FG_per
  )

# manipulate all jump shots on offense to only get variables we want in the format we want
jump_shots_off <- jump_shots_off_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    tm_jumper_freq = `%Time` / 100,
    tm_jumper_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    tm_jumper_freq,
    tm_jumper_FG_per
  )

# manipulate all jump shots on defense to only get variables we want in the format we want
jump_shots_def <- jump_shots_def_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    opp_jumper_freq = `%Time` / 100,
    opp_jumper_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    opp_jumper_freq,
    opp_jumper_FG_per
  )

# manipulate short mid-range shots on offense to only get variables we want in the format we want
short_mid_off <- short_mid_off_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    tm_short_mid_freq = `%Time` / 100,
    tm_short_mid_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    tm_short_mid_freq,
    tm_short_mid_FG_per
  )

# manipulate short mid-range shots on defense to only get variables we want in the format we want
short_mid_def <- short_mid_def_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    opp_short_mid_freq = `%Time` / 100,
    opp_short_mid_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    opp_short_mid_freq,
    opp_short_mid_FG_per
  )

# manipulate long mid-range shots on offense to only get variables we want in the format we want
long_mid_off <- long_mid_off_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    tm_long_mid_freq = `%Time` / 100,
    tm_long_mid_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    tm_long_mid_freq,
    tm_long_mid_FG_per
  )

# manipulate long mid-range shots on defense to only get variables we want in the format we want
long_mid_def <- long_mid_def_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    opp_long_mid_freq = `%Time` / 100,
    opp_long_mid_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    opp_long_mid_freq,
    opp_long_mid_FG_per
  )

# manipulate three point shots on offense to only get variables we want in the format we want
threes_off <- threes_off_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    tm_three_freq = `%Time` / 100,
    tm_three_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    tm_three_freq,
    tm_three_FG_per
  )

# manipulate three point shots on defense to only get variables we want in the format we want
threes_def <- threes_def_raw |> 
  rename(
    "School" = "Team"
  ) |> 
  mutate(
    opp_three_freq = `%Time` / 100,
    opp_three_FG_per = `FG%` / 100
  ) |> 
  dplyr::select(
    School,
    opp_three_freq,
    opp_three_FG_per
  )

# get the overall frequencies of each level of jump shot (short mid-range, long mid-range, three pointer) as they come in as frequencies within jumpers
all_jumpers_off <- left_join(jump_shots_off, short_mid_off, by = "School") |> 
  left_join(long_mid_off, by = "School") |> 
  left_join(threes_off, by = "School") |> 
  mutate(
    tm_short_mid_freq = tm_jumper_freq * tm_short_mid_freq,
    tm_long_mid_freq = tm_jumper_freq * tm_long_mid_freq,
    tm_three_freq = tm_jumper_freq * tm_three_freq,
  ) |> 
  dplyr::select(
    School,
    tm_short_mid_freq,
    tm_short_mid_FG_per,
    tm_long_mid_freq,
    tm_long_mid_FG_per,
    tm_three_freq,
    tm_three_FG_per,
  )

# do the same for defensive stats
all_jumpers_def <- left_join(jump_shots_def, short_mid_def, by = "School") |> 
  left_join(long_mid_def, by = "School") |> 
  left_join(threes_def, by = "School") |> 
  mutate(
    opp_short_mid_freq = opp_jumper_freq * opp_short_mid_freq,
    opp_long_mid_freq = opp_jumper_freq * opp_long_mid_freq,
    opp_three_freq = opp_jumper_freq * opp_three_freq,
  ) |> 
  dplyr::select(
    School,
    opp_short_mid_freq,
    opp_short_mid_FG_per,
    opp_long_mid_freq,
    opp_long_mid_FG_per,
    opp_three_freq,
    opp_three_FG_per,
  )



# play types
# read in isolation plays for offenses
isos_off_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 - Isolation - Team Offensive.csv", skip = 1)

# read in isolation plays for defenses
isos_def_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 - Isolation - Team Defensive.csv", skip = 1)

# read in cuts on offense
cuts_off_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - Cut - Team Offensive.csv", skip = 1)

# read in cuts on defense
cuts_def_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - Cut - Team Defensive.csv", skip = 1)

# read in hand offs for offense
hand_off_off_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - Hand Off - Team Offensive.csv", skip = 1)

# read in hand offs for defense
hand_off_def_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - Hand Off - Team Defensive.csv", skip = 1)

# read in pick and roll ball handler for offense
pnr_ball_off_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - P&R Ball Handler - Team Offensive.csv", skip = 1)

# read in pick and roll ball handler for defense
pnr_ball_def_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - P&R Ball Handler - Team Defensive.csv", skip = 1)

# read in pick a roll roll man for offense
pnr_roll_off_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - P&R Roll Man - Team Offensive.csv", skip = 1)

# read in pick a roll roll man for  defense
pnr_roll_def_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - P&R Roll Man - Team Defensive.csv", skip = 1)

# read in spot up plays for offense
spot_up_off_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - Spot Up - Team Offensive.csv", skip = 1)

# read in spot up plays for defense
spot_up_def_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - Spot Up - Team Defensive.csv", skip = 1)

# read in transition plays for offense
trans_off_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - Transition - Team Offensive.csv", skip = 1)

# read in transition plays for defense
trans_def_raw <- read_csv("Synergy Data/Play Types/College Women 2025-2026 All excluding Exhibitions - Transition - Team Defensive.csv", skip = 1)


# manipulate isolation plays on offense to only get variables we want in the format we want
isos_off <- isos_off_raw |> 
  rename(
    "School" = "Team",
    "tm_iso_PPP" = "PPP"
  ) |> 
  mutate(
    tm_iso_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    tm_iso_freq,
    tm_iso_PPP
  )

# manipulate isolation plays on defense to only get variables we want in the format we want
isos_def <- isos_def_raw |> 
  rename(
    "School" = "Team",
    "opp_iso_PPP" = "PPP"
  ) |> 
  mutate(
    opp_iso_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    opp_iso_freq,
    opp_iso_PPP
  )

# manipulate cut plays on offense to only get variables we want in the format we want
cuts_off <- cuts_off_raw |> 
  rename(
    "School" = "Team",
    "tm_cut_PPP" = "PPP"
  ) |> 
  mutate(
    tm_cut_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    tm_cut_freq,
    tm_cut_PPP
  )

# manipulate cut plays on defense to only get variables we want in the format we want
cuts_def <- cuts_def_raw |> 
  rename(
    "School" = "Team",
    "opp_cut_PPP" = "PPP"
  ) |> 
  mutate(
    opp_cut_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    opp_cut_freq,
    opp_cut_PPP
  )

# manipulate hand off plays on offense to only get variables we want in the format we want
hand_offs_off <- hand_off_off_raw |> 
  rename(
    "School" = "Team",
    "tm_ho_PPP" = "PPP"
  ) |> 
  mutate(
    tm_ho_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    tm_ho_freq,
    tm_ho_PPP
  )

# manipulate hand off plays on defense to only get variables we want in the format we want
hand_offs_def <- hand_off_def_raw |> 
  rename(
    "School" = "Team",
    "opp_ho_PPP" = "PPP"
  ) |> 
  mutate(
    opp_ho_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    opp_ho_freq,
    opp_ho_PPP
  )

# manipulate pick and roll ball handler plays on offense to only get variables we want in the format we want
pnr_ball_off <- pnr_ball_off_raw |> 
  rename(
    "School" = "Team",
    "tm_pnr_ball_PPP" = "PPP"
  ) |> 
  mutate(
    tm_pnr_ball_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    tm_pnr_ball_freq,
    tm_pnr_ball_PPP
  )

# manipulate pick and roll ball handler plays on defense to only get variables we want in the format we want
pnr_ball_def <- pnr_ball_def_raw |> 
  rename(
    "School" = "Team",
    "opp_pnr_ball_PPP" = "PPP"
  ) |> 
  mutate(
    opp_pnr_ball_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    opp_pnr_ball_freq,
    opp_pnr_ball_PPP
  )

# manipulate pick and roll roll man plays on offense to only get variables we want in the format we want
pnr_roll_off <- pnr_roll_off_raw |> 
  rename(
    "School" = "Team",
    "tm_pnr_roll_PPP" = "PPP"
  ) |> 
  mutate(
    tm_pnr_roll_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    tm_pnr_roll_freq,
    tm_pnr_roll_PPP
  )

# manipulate pick and roll roll man plays on defense to only get variables we want in the format we want
pnr_roll_def <- pnr_roll_def_raw |> 
  rename(
    "School" = "Team",
    "opp_pnr_roll_PPP" = "PPP"
  ) |> 
  mutate(
    opp_pnr_roll_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    opp_pnr_roll_freq,
    opp_pnr_roll_PPP
  )

# manipulate spot up plays on offense to only get variables we want in the format we want
spot_up_off <- spot_up_off_raw |> 
  rename(
    "School" = "Team",
    "tm_spot_up_PPP" = "PPP"
  ) |> 
  mutate(
    tm_spot_up_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    tm_spot_up_freq,
    tm_spot_up_PPP
  )

# manipulate spot up plays on defense to only get variables we want in the format we want
spot_up_def <- spot_up_def_raw |> 
  rename(
    "School" = "Team",
    "opp_spot_up_PPP" = "PPP"
  ) |> 
  mutate(
    opp_spot_up_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    opp_spot_up_freq,
    opp_spot_up_PPP
  )

# manipulate transition plays on offense to only get variables we want in the format we want
trans_off <- trans_off_raw |> 
  rename(
    "School" = "Team",
    "tm_trans_PPP" = "PPP"
  ) |> 
  mutate(
    tm_trans_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    tm_trans_freq,
    tm_trans_PPP
  )

# manipulate transition plays on defense to only get variables we want in the format we want
trans_def <- trans_def_raw |> 
  rename(
    "School" = "Team",
    "opp_trans_PPP" = "PPP"
  ) |> 
  mutate(
    opp_trans_freq = `%Time` / 100
  ) |> 
  dplyr::select(
    School,
    opp_trans_freq,
    opp_trans_PPP
  )


# combine the stats from pick and roll ball handler and pick and roll rol man plays into a single data set
# on offense
pnr_off <- left_join(pnr_ball_off, pnr_roll_off, by = "School") |> 
  mutate(
    tm_pnr_freq = tm_pnr_ball_freq + tm_pnr_roll_freq,
    tm_pnr_PPP = (tm_pnr_ball_PPP * (tm_pnr_ball_freq / (tm_pnr_ball_freq + tm_pnr_roll_freq))) + (tm_pnr_roll_PPP * (tm_pnr_roll_freq / (tm_pnr_ball_freq + tm_pnr_roll_freq)))
  ) |> 
  dplyr::select(
    School, 
    tm_pnr_freq,
    tm_pnr_PPP
  )

# on defense
pnr_def <- left_join(pnr_ball_def, pnr_roll_def, by = "School") |> 
  mutate(
    opp_pnr_freq = opp_pnr_ball_freq + opp_pnr_roll_freq,
    opp_pnr_PPP = (opp_pnr_ball_PPP * (opp_pnr_ball_freq / (opp_pnr_ball_freq + opp_pnr_roll_freq))) + (opp_pnr_roll_PPP * (opp_pnr_roll_freq / (opp_pnr_ball_freq + opp_pnr_roll_freq)))
  ) |> 
  dplyr::select(
    School, 
    opp_pnr_freq,
    opp_pnr_PPP
  )

# read in team data using the wehoop package. This will give use the team names seen on ESPN and team logos
teams_raw <- wehoop::espn_wbb_teams()

# select only the variables we want for the teams data set
teams_select <- teams_raw |> 
  dplyr::select(
    abbreviation, 
    display_name, 
    color, 
    alternate_color,
    logo,
    team
  )

# add Mercyhurst and West Georgia, which weren't originally included
teams <- teams_select |>
  add_row(
    abbreviation = "MERC",
    display_name = "Mercyhurst Lakers",
    color = "07594D",
    alternate_color = "182752",
    logo = "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/2385.png&h=200&w=200",
    team = "Mercyhurst"
  ) |>
  add_row(
    abbreviation = "WGA",
    display_name = "West Georgia Wolves",
    color = "0656A5",
    alternate_color = "DA2128",
    logo = "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/2698.png&h=200&w=200",
     team = "West Georgia"
  ) |> 
  arrange(
    display_name
  )

# update St. Thomas-Minnesota's colors
teams <- teams |> 
  mutate(
    color = ifelse(
      abbreviation == "STMN", 
      "510c76",
      color
    ),
    alternate_color = ifelse(
      abbreviation == "STMN",
      "FFFFFF",
      alternate_color
    )
  )

# change Missouri State from the Bears to the Lady Bears
teams <- teams |> 
  mutate(
    display_name = ifelse(
      abbreviation == "MOST", 
      "Missouri State Lady Bears",
      display_name
    )
  )

# change Grand Canyon from the Lopes to the Antelopes
teams <- teams |> 
  mutate(
    display_name = ifelse(
      abbreviation == "GCU", 
      "Grand Canyon Antelopes",
      display_name
    )
  )

# change from McNeese to McNeese State
teams <- teams |> 
  mutate(
    display_name = ifelse(
      abbreviation == "MCN", 
      "McNeese State Cowgirls",
      display_name
    ),
    team = ifelse(
      abbreviation == "MCN",
      "McNeese State",
      team
    )
  )

# change from Seattle U to Seattle State
teams <- teams |> 
  mutate(
    display_name = ifelse(
      abbreviation == "SEA", 
      "Seattle Redhawks",
      display_name
    ),
    team = ifelse(
      abbreviation == "SEA",
      "Seattle",
      team
    )
  )

# define differences in team names between Synergy and ESPN
team_aliases <- tribble(
  ~synergy_name,                                   ~espn_name,
  "Connecticut Huskies",                         "UConn Huskies",
  "Texas-(Austin) Longhorns",                    "Texas Longhorns",
  "North Carolina State Wolfpack",               "NC State Wolfpack",
  "Texas-(San Antonio) Roadrunners",             "UTSA Roadrunners",
  "Texas-(Rio Grande Valley) Vaqueros",          "UT Rio Grande Valley Vaqueros",
  "Utah Tech University Trailblazers",           "Utah Tech Trailblazers",
  "Appalachian State Mountaineers",              "App State Mountaineers",
  "Wisconsin-(Green Bay) Phoenix",               "Green Bay Phoenix",
  "St. Thomas (MN) Tommies",                     "St. Thomas-Minnesota Tommies",
  "Texas-(Arlington) Mavericks",                 "UT Arlington Mavericks",
  "Louisiana-(Lafayette) Ragin Cajuns",          "Louisiana Ragin' Cajuns",
  "Texas A&M-(Corpus Christi) Islanders",        "Texas A&M-Corpus Christi Islanders",
  "Miami (FL) Hurricanes",                       "Miami Hurricanes",
  "Arkansas-(Pine Bluff) Golden Lions",          "Arkansas-Pine Bluff Golden Lions",
  "Louisiana-(Monroe) Warhawks",                 "UL Monroe Warhawks",
  "Wisconsin-(Milwaukee) Panthers",              "Milwaukee Panthers",
  "Loyola (Chicago) Ramblers",                   "Loyola Chicago Ramblers",
  "Loyola (MD) Greyhounds",                      "Loyola Maryland Greyhounds",
  "Saint Francis (PA) Red Flash",                "Saint Francis Red Flash",
  "Arkansas-(Little Rock) Trojans",              "Little Rock Trojans",
  "North Carolina-Asheville Bulldogs",           "UNC Asheville Bulldogs",
  "North Carolina-Wilmington Seahawks",          "UNC Wilmington Seahawks",
  "Gardner-Webb Runnin Bulldogs",                "Gardner-Webb Runnin' Bulldogs",
  "Nebraska-Omaha Mavericks",                    "Omaha Mavericks",
  "Maryland-Eastern Shore Hawks",                "Maryland Eastern Shore Hawks",
  "Illinois-Chicago Flames",                     "UIC Flames",
  "Tennessee-Martin Skyhawks",                   "UT Martin Skyhawks",
  "Indiana University - Indianapolis Jaguars",   "IU Indianapolis Jaguars",
  "Alcorn State Braves",                         "Alcorn State Lady Braves",
  "Alabama State Hornets",                       "Alabama State Lady Hornets",
  "Grambling State Tigers",                      "Grambling Lady Tigers",
  "Hampton Pirates",                             "Hampton Lady Pirates",
  "Jackson State Tigers",                        "Jackson State Lady Tigers",
  "Morgan State Bears",                          "Morgan State Lady Bears",
  "Northwestern State Demons",                   "Northwestern State Lady Demons",
  "Old Dominion Lady Monarchs",                  "Old Dominion Monarchs",
  "Prairie View A&M Panthers",                   "Prairie View A&M Lady Panthers",
  "Southeastern Louisiana Lions",                "SE Louisiana Lady Lions",
  "South Carolina State Bulldogs",               "South Carolina State Lady Bulldogs",
  "Southern Miss Golden Eagles",                 "Southern Miss Lady Eagles",
  "Stephen F. Austin Lumberjacks",               "Stephen F. Austin Ladyjacks",
  "Tennessee State Tigers",                      "Tennessee State Lady Tigers",
  "UNLV Rebels",                                 "UNLV Lady Rebels",
  "Western Carolina Lady Catamounts",            "Western Carolina Catamounts",
  "Western Kentucky Hilltoppers",                "Western Kentucky Lady Toppers",
  "Albany Great Danes",                          "UAlbany Great Danes",
  "Brigham Young Cougars",                       "BYU Cougars",
  "Bryant University Bulldogs",                  "Bryant Bulldogs",
  "Cal Poly SLO Mustangs",                       "Cal Poly Mustangs",
  "Central Arkansas Bears",                      "Central Arkansas Sugar Bears",
  "Central Connecticut State Blue Devils",       "Central Connecticut Blue Devils",
  "Delaware Fightin Blue Hens",                  "Delaware Blue Hens",
  "Detroit Titans",                              "Detroit Mercy Titans",
  "East Tennessee State Buccaneers",             "East Tennessee State Bucs",
  "Eastern Washington University",               "Eastern Washington Eagles",
  "Evansville Aces",                             "Evansville Purple Aces",
  "Grand Canyon University Antelopes",           "Grand Canyon Antelopes",
  "Hawaii Warriors",                             "Hawai'i Rainbow Wahine",
  "UMKC Kangaroos",                              "Kansas City Roos",
  "Kennesaw State Fighting Owls",                "Kennesaw State Owls",
  "Lindenwood University Lions",                 "Lindenwood Lions",
  "Long Beach State 49ers",                      "Long Beach State Beach",
  "Massachusetts Lowell",                        "UMass Lowell River Hawks",
  "Middle Tennessee State Blue Raiders",         "Middle Tennessee Blue Raiders",
  "Mississippi Rebels",                          "Ole Miss Rebels",
  "Mississippi Valley State Delta Devils",       "Mississippi Valley State Devilettes",
  "N.J.I.T. Highlanders",                        "NJIT Highlanders",
  "Nicholls State Colonels",                     "Nicholls Colonels",
  "Queens University of Charlotte Royals",       "Queens University Royals",
  "Southern Methodist Mustangs",                 "SMU Mustangs",
  "Sam Houston State Bearkats",                  "Sam Houston Bearkats",
  "San Jose State Spartans",                     "San José State Spartans",
  "USC Upstate Spartans",                        "South Carolina Upstate Spartans",
  "Southern University Jaguars",                 "Southern Jaguars",
  "Stonehill College Skyhawks",                  "Stonehill Skyhawks",
  "Valparaiso University",                       "Valparaiso Beacons",
  "Virginia Commonwealth Rams",                  "VCU Rams"
)

# combine all stats found from Synergy
# on offense
synergy_stats_off <- left_join(at_rim_off, runners_off, by = "School") |> 
  left_join(hook_off, by = "School") |> 
  left_join(all_jumpers_off, by = "School") |> 
  left_join(isos_off, by = "School") |> 
  left_join(cuts_off, by = "School") |> 
  left_join(hand_offs_off, by = "School") |> 
  left_join(spot_up_off, by = "School") |> 
  left_join(trans_off, by = "School") |> 
  left_join(pnr_off, by = "School")
  
# on defense
synergy_stats_def <- left_join(at_rim_def, runners_def, by = "School") |> 
  left_join(hook_def, by = "School") |> 
  left_join(all_jumpers_def, by = "School") |> 
  left_join(isos_def, by = "School") |> 
  left_join(cuts_def, by = "School") |> 
  left_join(hand_offs_def, by = "School") |> 
  left_join(spot_up_def, by = "School") |> 
  left_join(trans_def, by = "School") |> 
  left_join(pnr_def, by = "School")

# now combine offense and defense together
synergy_stats <- left_join(synergy_stats_off, synergy_stats_def, by = c("School"))

# combine synergy stats with the team identifying stats from ESPN, using team names from ESPN
synergy_clean_names <- synergy_stats |>
  left_join(team_aliases, by = c("School" = "synergy_name")) |>
  mutate(School = coalesce(espn_name, School)) |>
  select(-espn_name)

stats_without_sports_ref <- synergy_clean_names |> 
  left_join(teams, by = c("School" = "display_name"))


# clean advanced stats from sports reference to have the same team names as on espn
# remove instances where "NCAA" was added to some team names
advanced_stats_off <- advanced_stats_off |> 
  mutate(
    School = str_remove_all(School, "\\s*NCAA\\s*$")
  )

advanced_stats_def <- advanced_stats_def |> 
  mutate(
    School = str_remove_all(School, "\\s*NCAA\\s*$")
  )

# define differences in team names between Sport Reference and ESPN
team_aliases_sports_ref <- tribble(
  ~sports_ref_name,                      ~espn_name,
     "Appalachian State",                    "App State",
     "Brigham Young",                        "BYU",
     "Connecticut",                          "UConn",
     "Miami (FL)",                           "Miami",
     "Southern Mississippi",                 "Southern Miss",
     "Saint Mary's (CA)",                    "Saint Mary's",
     "St. John's (NY)",                      "St. John's",
     "Saint Francis (PA)",                   "Saint Francis",
     "Loyola (IL)",                          "Loyola Chicago",
     "Loyola (MD)",                          "Loyola Maryland",
     "Illinois-Chicago",                     "UIC",
     "Massachusetts-Lowell",                 "UMass Lowell",
     "Maryland-Baltimore County",            "UMBC",
     "Tennessee-Martin",                     "UT Martin",
     "Texas-Rio Grande Valley",              "UT Rio Grande Valley",
     "Louisiana-Monroe",                     "UL Monroe",
     "Southeastern Louisiana",               "SE Louisiana",
     "Southern California",                  "USC",
     "Hawaii",                               "Hawai'i",
     "San Jose State",                       "San José State",
     "Queens (NC)",                          "Queens University",
     "IU Indy",                              "IU Indianapolis",
     "St. Thomas",                           "St. Thomas-Minnesota",
     "Prairie View",                         "Prairie View A&M",
     "FDU",                                  "Fairleigh Dickinson",
     "Albany (NY)",                          "UAlbany",
     "American",                             "American University",
     "Central Connecticut State",            "Central Connecticut",
     "College of Charleston",                "Charleston",
     "Louisiana State",                      "LSU",
     "Maryland-Eastern Shore",               "Maryland Eastern Shore",
     "Mississippi",                          "Ole Miss",
     "Nevada-Las Vegas",                     "UNLV",
     "Nicholls State",                       "Nicholls",
     "Southern Methodist",                   "SMU",
     "Virginia Commonwealth",                "VCU"
)

# combine offensive and defensive advanced stats from sports reference
advanced_stats <- left_join(advanced_stats_off, advanced_stats_def, by = c("School"))

# change names of advanced stats so team names match those stored in team variables of teams and stats_without_sports_ref data sets
advanced_stats_clean <- advanced_stats |>
  left_join(team_aliases_sports_ref, by = c("School" = "sports_ref_name")) |>
  mutate(School = coalesce(espn_name, School)) |>
  select(-espn_name)

# combine all into one data set
matchup_stats_wide <- stats_without_sports_ref |> 
  left_join(advanced_stats_clean, by = c("team" = "School"))

