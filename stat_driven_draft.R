# Author: Zach Borromeo
# Project Name: AI Final Project
# Date Created: March 20, 2022
# Last Modified: March 20, 20221


# importing all required libraries for this project, primarily to handle data,
# SQL tables, and statistics of tables
library(httr)
library(tidyverse)
library(DBI)
library(janitor)
library(fs)
library(pool)
library(RPostgres)
library(stringdist)
library(fuzzyjoin)
library(tm)
library(matrixStats)

#importing all data related to this project, files are located in the project folder
# of this assignment

current_draft_order <- readxl::read_excel("nfl_draft_order_2022.xlsx") %>%
  clean_names()

team_needs <- readxl::read_excel("nfl_draft_team_needs.xlsx") %>%
  clean_names() %>%
  rename(team = team_name)

top_200_players <- readxl::read_excel("top_200_players.xlsx") %>%
  clean_names()

draft_order_pickless <- current_draft_order %>%
  select(team)

merge_order_needs <- draft_order_pickless %>%
  left_join(team_needs, by="team")

remaining_big_board <- top_200_players

historic_draft_data <- readxl::read_excel("historical_draft_data.xlsx")

stripped_HDD <- historic_draft_data %>% 
  select(Round, Overall, Position)

HDD_df <- data.frame("Round" = numeric(),
                     "Pick" = numeric(),
                     "QB" = numeric(),
                     "RB" = numeric(),
                     "WR" = numeric(),
                     "TE" = numeric(),
                     "OT" = numeric(),
                     "OG" = numeric(),
                     "OC" = numeric(),
                     "EDGE" = numeric(),
                     "DL" = numeric(),
                     "CB" = numeric(),
                     "LB" = numeric(),
                     "S" = numeric(),
                     "K" = numeric(),
                     "P" = numeric())
round <- 1
pick <- 1

for (i in 1:104){
  
  row_i = c(round, pick, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  pick <- pick + 1
  
  if (pick <= 32){
    round <- 1
  }else if (pick <= 64 && pick > 32){
    round <- 2
  }else{
    round <- 3
  }
  
  HDD_df[i, ] <- row_i
}


for (i in 1:nrow(stripped_HDD)){
  
  overall <- as.numeric(stripped_HDD[i, 2])
  
  pick_pos <- stripped_HDD[i, 3]
  
  temp_HDD <- HDD_df %>% 
    filter(Pick == as.numeric(overall))
  
  index <- which(colnames(temp_HDD) == as.character(pick_pos), arr.ind = TRUE)
  
  temp_var <- as.numeric(HDD_df[overall, index])
  temp_var <- as.numeric(temp_var + 1)
  
  HDD_df[overall, index] <- temp_var
  
}

temp_HDD_df <- HDD_df %>% 
  filter(!is.na(Round)) %>% 
  select(-c(Round, Pick))

temp_HDD_df <- temp_HDD_df %>% 
  mutate(Position_Name1 = colnames(temp_HDD_df[max.col(temp_HDD_df, "first")])) %>% 
  mutate(Position_Name2 = colnames(temp_HDD_df[max.col(temp_HDD_df, "last")])) %>%
  mutate(Position_Name3 = colnames(temp_HDD_df[max.col(temp_HDD_df, "random")])) %>%
  mutate(max_val = as.numeric(rowMaxs(as.matrix(temp_HDD_df)))) %>% 
  mutate(Position_Name1 = str_replace_all(Position_Name1, "[[:punct:]]", "")) %>%
  mutate(Position_Name1 = str_replace_all(Position_Name1, "[[:digit:]]", "")) %>% 
  mutate(Position_Name2 = str_replace_all(Position_Name2, "[[:punct:]]", "")) %>%
  mutate(Position_Name2 = str_replace_all(Position_Name2, "[[:digit:]]", "")) %>% 
  mutate(Position_Name3 = str_replace_all(Position_Name3, "[[:punct:]]", "")) %>%
  mutate(Position_Name3 = str_replace_all(Position_Name3, "[[:digit:]]", ""))
  
HDD_df <- HDD_df %>% 
  left_join(temp_HDD_df) %>% 
  filter(!is.na(Round))

round <- 1
pick <- 1

potential_draft_order <- data.frame("Round" = character(),
                                    "Pick" = character(),
                                    "Team" = character(),
                                    "Name" = character(),
                                    "Position" = character(),
                                    "College" = character(),
                                    "Score" = numeric(),
                                    "Score Multiplier" = numeric(),
                                    "Combined Rank" = numeric(),
                                    "Score Multiplier Rank" = numeric(),
                                    "Average Rank" = numeric())

for (i in 1:nrow(merge_order_needs)){
  team_name <- as.character(merge_order_needs[i, 1])
  random_val <- round(runif(1, min=17, max=19))
  
  if (pick <= 32){
    round <- 1
  }else if (pick <= 64 && pick > 32){
    round <- 2
  }else{
    round <- 3
  }

  pick_pos <- HDD_df[i, as.numeric(random_val)]
  
  pick_selection <- remaining_big_board %>% 
    filter(position == as.character(pick_pos)) %>% 
    head(n=1)
  
  if (nrow(pick_selection) == 0){
    pick_selection <- remaining_big_board %>% 
      filter(score_total == max(score_total)) %>% 
      head(n=1)
  }
  
  row_i = c(round, pick, team_name, pick_selection)
  potential_draft_order[i, ] <- row_i
  
  pick_name <- pick_selection %>% 
    select(name)
  
  pick_pos <- pick_selection %>% 
    select(position)
  
  remaining_big_board <- remaining_big_board %>% 
    filter(name != as.character(pick_name))
  
  
  
  pick <- pick + 1
  
  
  
}

