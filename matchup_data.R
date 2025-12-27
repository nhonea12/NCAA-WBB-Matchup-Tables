library(tidyverse)
library(rvest)


#Read in 2025 Advanced School Stats for NCAAW from basketball reference
school_advanced_url <- paste0("https://www.sports-reference.com/cbb/seasons/women/2025-advanced-school-stats.html")
school_advanced_html <- read_html(school_advanced_url)
school_advanced_tables <- html_table(school_advanced_html)
school_advanced_off <- school_advanced_tables[[1]]

#Read in 2025 Advanced Opponent School Stats for NCAAW from basketball reference
opp_advanced_url <- paste0("https://www.sports-reference.com/cbb/seasons/women/2025-advanced-opponent-stats.html")
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

