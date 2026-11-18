# Assignment 2

# set the working directory
setwd("~/Downloads/DSA 515/HW 2")

## PART 1 - Clean the Dataset

# load libraries
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# read into the csv file
df <- read_csv("hockey_raw.csv")
glimpse(df)

# replace missing 'Position' with "Unknown"
df <- replace_na(df, list(Position = "Unknown"))

# replace missing 'Shots' with the column median
shots_med <- median(df$Shots, na.rm = TRUE)
df$Shots <- ifelse(is.na(df$Shots), shots_med, df$Shots)

# replace 'N/A' in 'Goals' with zero and then replace missing 'Goals' with the column median
df$Goals[df$Goals == "N/A"] <- "0"
df$Goals <- parse_number(df$Goals)
goals_med <- median(df$Goals, na.rm = TRUE)
df$Goals <- ifelse(is.na(df$Goals), goals_med, df$Goals)

# remove fully duplicated rows
df <- distinct(df)

# remove duplicates based on 'Player_Name' and 'Date' only
df <- distinct(df, Player_Name, Date, .keep_all = TRUE)

# convert 'Date' to proper data format
df$Date <- parse_date_time(df$Date, orders = "mdy")

# remove commas from 'Salary' and convert to numeric
# note: 'Salary' already does not contain commas and it is already numeric
df$Salary <- as.numeric(df$Salary)

# ensure 'Shots' and 'Goals' are numeric types
df$Shots <- as.numeric(df$Shots)
df$Goals <- as.numeric(df$Goals)

# pivot period goals into long format with new columns 'Period' and 'Goals_Scored'
df <- pivot_longer(
  df, 
  cols = c(
    `1st_Period_Goals`,
    `2nd_Period_Goals`,
    `3rd_Period_Goals`
    ),
  names_to = "Period",
  values_to = "Goals_Scored"
)

# export cleaned data
write_csv(df, "hockey_cleaned.csv")

## PART 2 - Join Additional Data

# read into the additional csv files
players <- read_csv("hockey_players.csv")
teams <- read_csv("hockey_teams.csv")

# ensure keys match in format and data type before joining
df$Player_ID <- as.character(df$Player_ID)
players$Player_ID <- as.character(players$Player_ID)

df$Team_ID <- as.character(df$Team_ID)
teams$Team_ID <- as.character(teams$Team_ID)

# join with players using 'Player_ID'
df <- left_join(df, players, by = "Player_ID")

# join with teams using 'Team_ID'
df <- left_join(df, teams, by = "Team_ID")

# export joined data
write_csv(df, "hockey_final.csv")

## PART 3 - Create Visualizations

# calculate total goals by team
goals_by_team <- df %>%
  distinct(Player_ID, Date, Team_ID, Goals, Team_Name) %>%
  group_by(Team_Name) %>%
  summarise(
    Total_Goals = sum(Goals, na.rm = TRUE)
  )

# bar chart of total goals by team
ggplot(goals_by_team, aes(x = Team_Name, y = Total_Goals, fill = Team_Name)) +
  geom_col() +  
  labs(
    title = "Total Goals by Team",
    x = "Team",
    y = "Total Goals"
  )

# calculate average shots per game over time
avg_shots <- df %>%
  distinct(Player_ID, Date, Shots) %>%
  group_by(Date) %>%
  summarise(
    Average_Shots = mean(Shots, na.rm = TRUE)
  )

# line chart of average shots per game over time
ggplot(avg_shots, aes(x = Date, y = Average_Shots)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Shots per Game Over Time",
    x = "Date",
    y = "Average Shots per Game"
  )