library(tidyverse)
library(dplyr)
#install.packages("effsize")
library(effsize)
library(janitor)
library(ggplot2)
library(stats)
#install.packages("reshape")
library(reshape)
#install.packages("gvlma")
library(gvlma)
#install.packages("pwr")
library(pwr)
#install.packages("CTT")
library(CTT)
#install.packages("psych")
library(psych)
#install.packages("multilevel")
library(multilevel)
library(utils)
library(ROCR)
install.packages("car")
library(car)


swap <- function(DF, n, m)
{
  n <- if (class(n)=="character" & is.na(suppressWarnings(as.integer(n)))) which(colnames(DF)==n) else as.integer(n)
  m <- if (class(m)=="character" & is.na(suppressWarnings(as.integer(m)))) which(colnames(DF)==m) else as.integer(m)
  
  if (!(1<=n & n<=length(DF))) stop( "`n` represents invalid index!" )
  if (!(1<=m & m<=length(DF))) stop( "`m` represents invalid index!" )
  
  return (DF[ if (n==m) 1:length(DF) else c( (if (min(n,m)==1) c() else 1:(min(n,m)-1) ), (if (min(n,m)+1 == max(n,m)) (min(n,m)+1):(max(n,m)-1) else c( max(n,m), (min(n,m)+1):(max(n,m)-1), min(n,m))), (if (max(n,m)==length(DF)) c() else (max(n,m)+1):length(DF) ) ) ])
}
dataset<-read.csv("england-premier-league-teams-2022-to-2023-stats.csv", sep=",",header=TRUE)
matches <- read.csv("england-premier-league-matches-2022-to-2023-stats.csv", sep=",",header=TRUE)
players <- read.csv("teams_fifa23.csv", sep=",",header=TRUE)
players <-  players %>% filter(League == "English Premier League (1)" & Name != "AFC Richmond")

sum <- c()
sum_away <- c()
week <- c()
week_away <- c()
recent_home <- c()
recent_away <- c()
half_away<- c()
half_home<- c()
percentage_away<- c()
percentage_home<- c()
teams <- c(dataset$common_name)
counter_home = 1
counter_away = 1
cut_off = 20
matches$delta_ppg <-matches$Pre.Match.PPG..Home. - matches$Pre.Match.PPG..Away.
matches$btts <- ifelse((matches$away_team_goal_count !=0 & matches$home_team_goal_count !=0 ), 1, 0)
matches$half_home <- ifelse((matches$home_team_goal_count_half_time !=0), 1, 0)
matches$half_away <- ifelse((matches$away_team_goal_count_half_time !=0), 1, 0)

# Initialize a new column to store the sums
training_dataset <-  matches %>% filter(Game.Week <= 21)
training_dataset <- arrange(training_dataset, Game.Week)

## use it for median goal home and away
training_dataset$avarage_goals_home <- NA
training_dataset$avarage_goals_away <- NA
training_dataset$recent_goals_home <- NA
training_dataset$recent_goals_away <- NA

for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      sum<- append(sum, training_dataset$home_team_goal_count [i])
      week<- append(week, mean(sum))
      if (length(sum) >=5){
        recent_home <- append(recent_home, mean(tail(sum,5)))
      }else{
        recent_home <- append(recent_home, mean(sum))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$avarage_goals_home[i]<- week[counter_home]
        training_dataset$recent_goals_home[i] <-recent_home[counter_home]
      }else{
        training_dataset$avarage_goals_home[i] <- week[counter_home-1]
        training_dataset$recent_goals_home[i]<- recent_home[counter_home-1]
      }
      counter_home= counter_home+1 
    } 
    if (training_dataset$away_team_name[i] == teams[k]) {
      sum_away <- append(sum_away, training_dataset$away_team_goal_count [i])
      week_away<- append(week_away, mean(sum_away))
      if (length(sum_away) >=5){
        recent_away <- append(recent_away, mean(tail(sum_away,5)))
      }else{
        recent_away <- append(recent_away, mean(sum_away))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$avarage_goals_away[i]<- week_away[counter_away]
        training_dataset$recent_goals_away[i]<- recent_away[counter_away]
      }else{
        training_dataset$avarage_goals_away[i] <- week_away[counter_away-1]
        training_dataset$recent_goals_away[i]<- recent_away[counter_away-1]
      }
      counter_away = counter_away + 1
    }
  }
  sum_away <-c()
  sum <- c()
  week <- c()
  week_away <- c()
  recent_home <- c()
  recent_away <- c()
  counter_away = 1
  counter_home = 1
}

#use for meedian for goal conceeded at home and away
training_dataset$avarage_conceeded_away<- NA
training_dataset$avarage_conceeded_home <- NA
training_dataset$recent_conceeded_home <- NA
training_dataset$recent_conceeded_away <- NA
for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum<- append(sum, training_dataset$away_team_goal_count [i])
      week<- append(week, mean(sum))
      if (length(sum) >=5){
        recent_home <- append(recent_home, mean(tail(sum,5)))
      }else{
        recent_home <- append(recent_home, mean(sum))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$avarage_conceeded_home[i]<- week[counter_home]
        training_dataset$recent_conceeded_home[i]<- recent_home[counter_home]
      }else{
        training_dataset$avarage_conceeded_home[i] <- week[(counter_home) -1]
        training_dataset$recent_conceeded_home[i]<- recent_home[counter_home-1]
        
      }
      
      counter_home= counter_home+1
      
    }
    if (training_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away<- append(sum, training_dataset$home_team_goal_count [i])
      week_away<- append(week_away, mean(sum_away))
      if (length(sum_away) >=5){
        recent_away <- append(recent_away, mean(tail(sum_away,5)))
      }else{
        recent_away <- append(recent_away, mean(sum_away))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$avarage_conceeded_away[i]<- week_away[counter_away]
        training_dataset$recent_conceeded_away[i]<- recent_away[counter_away]
      }else{
        training_dataset$avarage_conceeded_away[i] <- week_away[(counter_away) -1]
        training_dataset$recent_conceeded_away[i]<- recent_away[counter_away-1]
      }
      counter_away= counter_away+1 
    }
  }
  sum_away <-c()
  sum <- c()
  week <- c()
  week_away <- c()
  recent_home <- c()
  recent_away <- c()
  counter_away = 1
  counter_home = 1
}
##
training_dataset$avarage_shots_home<- NA
training_dataset$avarage_shots_away <- NA
training_dataset$recent_shots_home <- NA
training_dataset$recent_shots_away <- NA

#use it for median shots on target home and aaway
for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum<- append(sum, training_dataset$home_team_shots_on_target [i])
      week<- append(week, median(sum))
      if (length(sum) >=5){
        recent_home <- append(recent_home, mean(tail(sum,5)))
      }else{
        recent_home <- append(recent_home, mean(sum))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$avarage_shots_home[i]<- week[counter_home]
        training_dataset$recent_shots_home[i]<- recent_home[counter_home]
      }else{
        training_dataset$avarage_shots_home[i] <- week[counter_home-1]
        training_dataset$recent_shots_home[i]<- recent_home[counter_home-1]
      }
      counter_home= counter_home+1       
    }
    if (training_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away<- append(sum, training_dataset$away_team_shots_on_target [i])
      week_away<- append(week_away, median(sum_away))
      if (length(sum_away) >=5){
        recent_away <- append(recent_away, mean(tail(sum_away,5)))
      }else{
        recent_away <- append(recent_away, mean(sum_away))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$avarage_shots_away[i]<- week_away[counter_away]
        training_dataset$recent_shots_away[i]<- recent_away[counter_away]
      }else{
        training_dataset$avarage_shots_away[i] <- week_away[counter_away-1]
        training_dataset$recent_shots_away[i]<- recent_away[counter_away-1]
        
      }
      counter_away= counter_away+1 
    }
  }
  sum_away <-c()
  sum <- c()
  week <- c()
  week_away <- c()
  recent_home <- c()
  recent_away <- c()
  counter_away = 1
  counter_home = 1
}
#percentage half time goals
training_dataset$percentage_half_home <- NA
training_dataset$percentage_half_away <- NA
for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      half_home <- append(half_home,training_dataset$half_home[i])
      percentage_home <- append(percentage_home, mean(half_home==1))
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$percentage_half_home[i] <- percentage_home[counter_home]
      }else{
        training_dataset$percentage_half_home[i] <- percentage_home[counter_home-1]
      }
      counter_home= counter_home+1       
    }
    if (training_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      half_away <- append(half_away,training_dataset$half_away[i])
      percentage_away <- append(percentage_away,  mean(half_away==1))
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$percentage_half_away[i] <- percentage_away[counter_away]
      }else{
        training_dataset$percentage_half_away[i] <- percentage_away[counter_away-1]
      }
      counter_away= counter_away+1 
    }
  }
  print(teams[k])
  half_away<- c()
  half_home<- c()
  percentage_away<- c()
  percentage_home<- c()
  counter_away = 1
  counter_home = 1
}
training_dataset$percentage_half_home_conceeded <- NA
training_dataset$percentage_half_away_conceeded <- NA
for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      half_home <- append(half_home,training_dataset$half_away[i])
      percentage_home <- append(percentage_home, mean(half_home==1))
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$percentage_half_home_conceeded[i] <- percentage_home[counter_home]
      }else{
        training_dataset$percentage_half_home_conceeded[i] <- percentage_home[counter_home-1]
      }
      counter_home= counter_home+1       
    }
    if (training_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      half_away <- append(half_away,training_dataset$half_home[i])
      percentage_away <- append(percentage_away,  mean(half_away==1))
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$percentage_half_away_conceeded[i] <- percentage_away[counter_away]
      }else{
        training_dataset$percentage_half_away_conceeded[i] <- percentage_away[counter_away-1]
      }
      counter_away= counter_away+1 
    }
  }
  half_away<- c()
  half_home<- c()
  percentage_away<- c()
  percentage_home<- c()
  counter_away = 1
  counter_home = 1
}
cols_to_remove <- c(1:4, 7, 9:12, 15, 23:30, 35:38, 43:45, 47:54, 56:63, 66)
training_dataset <- training_dataset[,-cols_to_remove]

training_dataset$attack_away <- NA
training_dataset$attack_home <- NA
training_dataset$defence_away <- NA
training_dataset$defence_home <- NA

for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      training_dataset$attack_home[i] =  players[players$Name == teams[k], "Attack"]
      training_dataset$defence_home[i] =  players[players$Name == teams[k], "Defence"]
    }
    if (training_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      training_dataset$attack_away[i] =  players[players$Name == teams[k], "Attack"]
      training_dataset$defence_away[i] = players[players$Name == teams[k], "Defence"]
    }
  }
}
training_dataset$goal_home_team <- NA
training_dataset$goal_away_team <- NA

for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    training_dataset$goal_home_team[i]<- training_dataset$defence_away[i]-training_dataset$attack_home[i]
    training_dataset$goal_away_team[i] <- training_dataset$defence_home[i]-training_dataset$attack_away[i]
  }
}


#col to remove1-4, 7, 9-12, 15, 23-30, 35-38, 43-45, 47-54, 56-63, 66
training_dataset <- swap (training_dataset, 29, 6)
training_dataset <- swap (training_dataset, 30, 7)
training_dataset <- swap (training_dataset, 31, 8)
training_dataset <- swap (training_dataset, 32, 9)
training_dataset <- swap (training_dataset, 33, 10)
training_dataset <- swap (training_dataset, 34, 11)
training_dataset <- swap (training_dataset, 35, 12)
training_dataset <- swap (training_dataset, 36, 13)
training_dataset <- swap (training_dataset, 37, 14)
training_dataset <- swap (training_dataset, 38, 15)
training_dataset <- swap (training_dataset, 39, 16)
training_dataset <- swap (training_dataset, 40, 17)
#MODEL
model_dataset<- training_dataset %>% filter(Game.Week <= cut_off) 
#first generation models
model <- glm(btts ~ delta_ppg  + Home.Team.Pre.Match.xG +  Away.Team.Pre.Match.xG + btts_percentage_pre_match + recent_goals_home+ recent_goals_away+ recent_conceeded_away + recent_conceeded_home + recent_conceeded_away:recent_goals_home+percentage_half_home+percentage_half_away, family =binomial, data = model_dataset)

model2<-glm(btts ~ delta_ppg  + Home.Team.Pre.Match.xG +  Away.Team.Pre.Match.xG +btts_percentage_pre_match + avarage_goals_home+ avarage_goals_away+ avarage_conceeded_away + avarage_conceeded_home + avarage_conceeded_home:avarage_goals_home, family =binomial, data = model_dataset)
#second generation models
model3 <- glm(btts ~ Away.Team.Pre.Match.xG  + goal_home_team+recent_goals_away+percentage_half_home_conceeded + recent_conceeded_home +percentage_half_home+percentage_half_away, family =binomial, data = model_dataset)

model4 <- glm(btts ~ Away.Team.Pre.Match.xG  + delta_ppg+ goal_home_team+ goal_home_team recent_conceeded_home+ percentage_half_home+ percentage_half_away+ percentage_half_home_conceeded+ percentage_half_away_conceeded, family =binomial, data = model_dataset)
#most used 
model5 <- glm(btts ~ Away.Team.Pre.Match.xG  + delta_ppg+ goal_away_team+ recent_conceeded_home+ percentage_half_home+ percentage_half_away, family =binomial, data = model_dataset)


model6 <- glm(btts ~   Home.Team.Pre.Match.xG +btts_percentage_pre_match +avarage_goals_away+ avarage_goals_home+avarage_conceeded_away + avarage_conceeded_home:avarage_goals_home, family =binomial, data = model_dataset)

#
summary(model6)

#vif(model)

#+ avarage_goals_away + avarage_goals_home / + avarage_goals_home+ avarage_goals_away+ avarage_conceeded_away + avarage_conceeded_home + avarage_conceeded_away:avarage_goals_home

testing_dataset  <-  training_dataset %>% filter(Game.Week ==21)




#how do I know which factors to include in a model or to exclude ?? add percentage of both team to score 


predictions <- predict(model6, newdata = testing_dataset, type= "response")
testing_dataset <- data.frame(testing_dataset, predictions)
# Create a binary vector of predictions based on the threshold
binary_predictions <- ifelse(predictions > 0.49, 1, 0)
testing_dataset$bet <- ifelse(testing_dataset$btts == binary_predictions, 1, 0)
percentage_won <-  (sum(testing_dataset$bet == 1) / length((testing_dataset$bet)))*100
print(percentage_won)

bet_lost <- testing_dataset  %>% filter(bet ==0 )
bet_won <- testing_dataset  %>% filter(bet ==1 )
##55<=prediction<=79
bet_lost$bin <- cut(bet_lost$predictions, breaks=c(.0, .10, .20, .30, .40, .50, .60, .70, .80, .90, .99999), labels = c(".0<.1",".1<.2",".2<.3",".3<.4",".4<.5", ".5<.6", ".6<.7", ".7<.8", ".8<.9", ".9<.99"))
bet_won$bin <- cut(bet_won$predictions, breaks=c(.0, .10, .20, .30, .40, .50, .60, .70, .80, .90, .99999), labels = c(".0<.1",".1<.2",".2<.3",".3<.4",".4<.5", ".5<.6", ".6<.7", ".7<.8", ".8<.9", ".9<.99"))

table(bet_lost$bin)
table(bet_won$bin)
bet_lost <- swap (bet_lost, 51, 14)
bet_won <- swap (bet_won, 51, 14)

#manual command for prediction
df_subset_home <- training_dataset[training_dataset$home_team_name %in% c("Arsenal"),]
df_subset_away <- training_dataset[training_dataset$away_team_name %in% c("Cardiff City"), ]
shots_home <-df_subset_home$avarage_shots_home[12]
shots_away <-df_subset_away$avarage_shots_away[11]
avarage_goals_away <- df_subset_away$avarage_goals_away[11]
avarage_goals_home <- df_subset_home$avarage_goals_home[12]
avarage_conceeded_away <- df_subset_away$avarage_conceeded_away[11]
avarage_conceeded_home <- df_subset_home$avarage_conceeded_home[12]
delta_ppg <-matches[231, "delta_ppg"]
predictions <- predict(model, newdata = data.frame(avarage_shots_home = shots_home, avarage_goals_away= avarage_goals_away, avarage_goals_home= avarage_goals_home, avarage_conceeded_home= avarage_conceeded_home, avarage_conceeded_away=avarage_conceeded_away, delta_ppg=delta_ppg), type = "response")
predictions

