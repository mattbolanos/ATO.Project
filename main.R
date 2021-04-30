library(tidyverse)
library(readr)
library(ggrepel)


#Compile list of regular season file names from Synergy folder
season_files <- list.files(pattern = "Season", full.names = FALSE, ignore.case = TRUE)
#Calculate league averages for regular season
reg_season <- function(a_csv) {
  df <- read_csv(a_csv)
  year <- str_sub(sub("_After_Time_Outs.csv*", "", a_csv), -29, -23)
  df <- df %>%
    mutate(reg_season_avg = mean(PPP),
           reg_season_PPP_comp = PPP - reg_season_avg,
           reg_season_PPP = PPP,
           year = year)%>%
    select(Team,reg_season_PPP, reg_season_PPP_comp, reg_season_avg, year)

  write.csv(df, file=paste0("PPP.Season", year, ".csv"))
}

lapply(season_files, FUN = reg_season)

# #Do same process for playoffs
playoff_files <- list.files(pattern = "Playoffs", full.names = FALSE, ignore.case = TRUE)

playoffs <- function(a_csv) {
  df <- read_csv(a_csv)
  year <- str_sub(sub("_After_Time_Outs.csv*", "", a_csv), -29, -23)
  df <- df %>%
    mutate(playoffs_avg = mean(PPP),
           playoffs_PPP_comp = PPP - playoffs_avg,

           playoffs_PPP = PPP,
           year = year)%>%
    select(Team, playoffs_PPP, playoffs_PPP_comp, playoffs_avg, year)

  write.csv(df, file=paste0("PPP.Playoffs", year, ".csv"))
}

lapply(playoff_files, FUN = playoffs)
 
# #Compile into one dataframe
PPP_season_files <- list.files(pattern = "PPP.Season", full.names = FALSE, ignore.case = TRUE)
PPP_playoff_files <- list.files(pattern = "PPP.Playoffs", full.names = FALSE, ignore.case = TRUE)

season_df <- vroom(PPP_season_files)
playoff_df <- vroom(PPP_playoff_files)

data <- left_join(playoff_df, season_df, by=c("Team", "year"))
# 
#Each team's colors
teamcolors <- teamcolors%>%
  select(name, primary)
data <- left_join(data,teamcolors, by = c("Team" = "name"))

# #Regular season PPP and playoff wins
playoff_wins <- seasons_schedule(seasons = 2010:2019, season_types = "Playoffs")%>%
  group_by(slugTeamWinner,slugSeason)%>%
  summarise(playoff_wins = n())

bball_ref <- dictionary_bref_teams()%>%
  select(nameTeamBREF, slugTeamBREF)%>%
  mutate(slugTeamBREF = case_when(
    slugTeamBREF == "NJN" ~ "BKN",
    TRUE ~ slugTeamBREF
  ))

teams <- left_join(teamcolors, bball_ref, by =c("name"= "nameTeamBREF"))

playoff_teams_wins <- left_join(playoff_wins, teams, by = c("slugTeamWinner" = "slugTeamBREF"))%>%
  mutate(Team = name,
         year = slugSeason)%>%
  select(-c(primary,name,slugSeason))
 
playoff_wins_df <-
  left_join(data, playoff_teams_wins, by = c("Team", "year"))

playoff_wins_df <- unique(playoff_wins_df)
playoff_wins_df$playoff_wins[is.na(playoff_wins_df$playoff_wins)] <- 0

#write.csv(playoff_wins_df, "full_df.csv")
full_df <- read_csv("data/full_df.csv")




