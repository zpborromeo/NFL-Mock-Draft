# Author: Zach Borromeo
# Project Name: AI Final Project
# Date Created: March 19, 2022
# Last Modified: March 19, 2022

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
  
  if (pick <= 32){
    round <- 1
  }else if (pick <= 64 && pick > 32){
    round <- 2
  }else{
    round <- 3
  }
  
  need1 <- as.character(merge_order_needs[i, 3])
  need2 <- as.character(merge_order_needs[i, 4])
  need3 <- as.character(merge_order_needs[i, 5])
  
    potential_pick1 <- remaining_big_board %>% 
      filter(position == need1) %>% 
      filter(score_multiplier == max(score_multiplier)) %>% 
      head(n=1)
    potential_pick2 <- remaining_big_board %>% 
      filter(position == need2) %>% 
      filter(score_multiplier == max(score_multiplier)) %>% 
      head(n=1)
    potential_pick3 <- remaining_big_board %>% 
      filter(position == need3) %>% 
      filter(score_multiplier == max(score_multiplier)) %>% 
      head(n=1)
  
  pp1_score <- potential_pick1 %>% 
    select(score_multiplier)
  pp2_score <- potential_pick2 %>% 
    select(score_multiplier)
  pp3_score <- potential_pick3 %>% 
    select(score_multiplier)
  
  if(dim(pp1_score)[1] == 0){
    pp1_score[1, 1] = 0
  }
  if(dim(pp2_score)[1] == 0){
    pp2_score[1, 1] = 0
  }
  if(dim(pp3_score)[1] == 0){
    pp3_score[1, 1] = 0
  }
  
  temp_max <- pp1_score 
  pick_selection <- potential_pick1
  
  if(temp_max < pp2_score){
    if(pp2_score < pp3_score){
      temp_max <- pp3_score
      pick_selection <- potential_pick3
    }else{
      temp_max <- pp2_score
      pick_selection <- potential_pick2
    }
  }
  
  row_i = c(round, pick, team_name, pick_selection)
  potential_draft_order[i, ] <- row_i
  
  pick_name <- pick_selection %>% 
    select(name)
  
  pick_pos <- pick_selection %>% 
    select(position)
  
  remaining_big_board <- remaining_big_board %>% 
    filter(name != as.character(pick_name))
  
  #updating team needs based on pick selections
  
  temp_team_needs <- team_needs %>% 
    filter(team == team_name)
  
  index <- which(temp_team_needs == as.character(pick_pos), arr.ind = TRUE)
  
  col_index <- as.numeric(index[, 2])
  
  row_i <- c(temp_team_needs[1:as.numeric((col_index - 1))], temp_team_needs[as.numeric((col_index + 1)):11], "NA")
  
  team_needs <- team_needs %>% 
    filter(team != team_name)
  
  team_needs[32, ] <- row_i
  
  merge_order_needs <- draft_order_pickless %>% 
    left_join(team_needs, by="team")
  
  pick <- pick + 1
  
  
  
}



