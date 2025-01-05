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
install.packages("glmnet")
library(car)
library(MASS)
library(caret)
library(glmnet)

swap <- function(DF, n, m)
{
  n <- if (class(n)=="character" & is.na(suppressWarnings(as.integer(n)))) which(colnames(DF)==n) else as.integer(n)
  m <- if (class(m)=="character" & is.na(suppressWarnings(as.integer(m)))) which(colnames(DF)==m) else as.integer(m)
  
  if (!(1<=n & n<=length(DF))) stop( "`n` represents invalid index!" )
  if (!(1<=m & m<=length(DF))) stop( "`m` represents invalid index!" )
  
  return (DF[ if (n==m) 1:length(DF) else c( (if (min(n,m)==1) c() else 1:(min(n,m)-1) ), (if (min(n,m)+1 == max(n,m)) (min(n,m)+1):(max(n,m)-1) else c( max(n,m), (min(n,m)+1):(max(n,m)-1), min(n,m))), (if (max(n,m)==length(DF)) c() else (max(n,m)+1):length(DF) ) ) ])
}
dataset<-read.csv("italy-serie-a-teams-2022-to-2023-stats.csv", sep=",",header=TRUE)
matches <- read.csv("italy-serie-a-matches-2022-to-2023-stats.csv", sep=",",header=TRUE)
fifa <- read.csv("teams_fifa23.csv", sep=",",header=TRUE)
fifa <-  fifa %>% filter(League == "Italian Serie A (1)")
fifa[2, "Name"] = "Monza"
fifa[8, "Name"] = "Inter Milan"
fifa[15, "Name"] = "Torino"
fifa[16, "Name"] = "Sampdoria"
fifa[17, "Name"] = "Sassuolo"
fifa[18, "Name"] = "Cremonese"
fifa[19, "Name"] = "Salernitana"
fifa[20, "Name"] = "Udinese"
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
btts_home<- c()
btts_away<-c()
team <- c(dataset$common_name)
counter_home = 1
counter_away = 1
cut_off = 15
#to do positive delta ppg
matches$delta_ppg <- matches$Pre.Match.PPG..Home. - matches$Pre.Match.PPG..Away.
matches$delta_ppg <- scale(matches$delta_ppg)

matches$btts <- ifelse((matches$away_team_goal_count !=0 & matches$home_team_goal_count !=0 ), 1, 0)
matches$half_home <- ifelse((matches$home_team_goal_count_half_time !=0), 1, 0)
matches$half_away <- ifelse((matches$away_team_goal_count_half_time !=0), 1, 0)

# Initialize a new column to store the sums
training_dataset <-  matches %>% filter(Game.Week <= 23)
training_dataset <- arrange(training_dataset, Game.Week)

## use it for mean goal home and away
training_dataset$avarage_goals_home <- NA
training_dataset$avarage_goals_away <- NA
training_dataset$recent_goals_home <- NA
training_dataset$recent_goals_away <- NA

for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == team[k]) {
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
    if (training_dataset$away_team_name[i] == team[k]) {
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

#use for mean for goal conceeded at home and away
training_dataset$avarage_conceeded_away<- NA
training_dataset$avarage_conceeded_home <- NA
training_dataset$recent_conceeded_home <- NA
training_dataset$recent_conceeded_away <- NA
for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == team[k]) {
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
    if (training_dataset$away_team_name[i] == team[k]) {
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
for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == team[k]) {
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
    if (training_dataset$away_team_name[i] == team[k]) {
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
training_dataset$recent_percentage_half_home <- NA
training_dataset$recent_percentage_half_away <- NA
for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == team[k]) {
      half_home <- append(half_home,training_dataset$half_home[i])
      percentage_home <- append(percentage_home, mean(half_home==1))
      if (length(half_home) >=3){
        recent_home <- append(recent_home, mean(tail(half_home,3)==1))
      }else{
        recent_home <- append(recent_home, mean(half_home==1))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$percentage_half_home[i] <- percentage_home[counter_home]
        training_dataset$recent_percentage_half_home[i]<- recent_home[counter_home]
      }else{
        training_dataset$percentage_half_home[i] <- percentage_home[counter_home-1]
        training_dataset$recent_percentage_half_home[i]<- recent_home[counter_home-1]
      }
      counter_home= counter_home+1       
    }
    if (training_dataset$away_team_name[i] == team[k]) {
      # Add the value to the sum for "F"
      half_away <- append(half_away,training_dataset$half_away[i])
      percentage_away <- append(percentage_away,  mean(half_away==1))
      if (length(half_away) >=3){
        recent_away <- append(recent_away, mean(tail(half_away,3)==1))
      }else{
        recent_away <- append(recent_away, mean(half_away==1))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$percentage_half_away[i] <- percentage_away[counter_away]
        training_dataset$recent_percentage_half_away[i]<- recent_away[counter_away]
      }else{
        training_dataset$percentage_half_away[i] <- percentage_away[counter_away-1]
        training_dataset$recent_percentage_half_away[i]<- recent_away[counter_away-1]
        
      }
      counter_away= counter_away+1 
    }
  }
  half_away<- c()
  half_home<- c()
  percentage_away<- c()
  percentage_home<- c()
  recent_home <- c()
  recent_away <- c()
  counter_away = 1
  counter_home = 1
}
training_dataset$percentage_half_home_conceeded <- NA
training_dataset$percentage_half_away_conceeded <- NA
training_dataset$recent_percentage_half_home_conceeded <- NA
training_dataset$recent_percentage_half_away_conceeded<- NA
for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == team[k]) {
      half_home <- append(half_home,training_dataset$half_away[i])
      percentage_home <- append(percentage_home, mean(half_home==1))
      if (length(half_home) >=3){
        recent_home <- append(recent_home, mean(tail(half_home,3)==1))
      }else{
        recent_home <- append(recent_home, mean(half_home==1))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$percentage_half_home_conceeded[i] <- percentage_home[counter_home]
        training_dataset$recent_percentage_half_home_conceeded[i]<- recent_home[counter_home]
      }else{
        training_dataset$percentage_half_home_conceeded[i] <- percentage_home[counter_home-1]
        training_dataset$recent_percentage_half_home_conceeded[i]<- recent_home[counter_home-1]
      }
      counter_home= counter_home+1       
    }
    if (training_dataset$away_team_name[i] == team[k]) {
      # Add the value to the sum for "F"
      half_away <- append(half_away,training_dataset$half_home[i])
      percentage_away <- append(percentage_away,  mean(half_away==1))
      if (length(half_away) >=3){
        recent_away <- append(recent_away, mean(tail(half_away,3)==1))
      }else{
        recent_away <- append(recent_away, mean(half_away==1))
      }
      if (i < (nrow(training_dataset) -nrow(training_dataset[training_dataset[, "Game.Week"] > cut_off, ]))){
        training_dataset$percentage_half_away_conceeded[i] <- percentage_away[counter_away]
        training_dataset$recent_percentage_half_away_conceeded[i]<- recent_away[counter_away]
      }else{
        training_dataset$percentage_half_away_conceeded[i] <- percentage_away[counter_away-1]
        training_dataset$recent_percentage_half_away_conceeded[i]<- recent_away[counter_away-1]
      }
      counter_away= counter_away+1 
    }
  }
  half_away<- c()
  half_home<- c()
  recent_home <- c()
  recent_away <- c()
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

for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == team[k]) {
     training_dataset$attack_home[i] =  teams[teams$common_name == team[k], "attack"]
      training_dataset$defence_home[i] =  teams[teams$common_name == team[k], "defence"]
    }
    if (training_dataset$away_team_name[i] == team[k]) {
      # Add the value to the sum for "F"
      training_dataset$attack_away[i] =  teams[teams$common_name == team[k], "attack"]
      training_dataset$defence_away[i] = teams[teams$common_name == team[k], "defence"]
    }
  }
}
for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
     # Check the value in the condition column
     if (training_dataset$home_team_name[i] == team[k]) {
       training_dataset$attack_home[i] =  fifa[fifa$Name == team[k], "Attack"]
       training_dataset$defence_home[i] =  fifa[fifa$Name == team[k], "Defence"]
     }
     if (training_dataset$away_team_name[i] == team[k]) {
       # Add the value to the sum for "F"
       training_dataset$attack_away[i] =  fifa[fifa$Name == team[k], "Attack"]
       training_dataset$defence_away[i] = fifa[fifa$Name == team[k], "Defence"]
     }
   }
 }
training_dataset$goal_home_team <- NA
training_dataset$goal_away_team <- NA
training_dataset$attack_game<- NA
training_dataset$defence_game<- NA
for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
    training_dataset$goal_home_team[i]<- training_dataset$defence_away[i]-training_dataset$attack_home[i]
    training_dataset$goal_away_team[i] <- training_dataset$defence_home[i]-training_dataset$attack_away[i]
    training_dataset$attack_game[i]<- training_dataset$attack_away[i]+training_dataset$attack_home[i]
    training_dataset$defence_game[i] <- training_dataset$defence_home[i]+training_dataset$defence_away[i]
    
  }
}
training_dataset$away_btts_percentage <- NA
training_dataset$home_btts_percentage <- NA

for (k in 1:length(team)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == team[k]) {
      btts_home <- append(btts_home,training_dataset$btts[i])
      percentage_home <- append(percentage_home, mean(btts_home==1))
      if (counter_home==1){
        training_dataset$home_btts_percentage[i] <- percentage_home[counter_home]
      }else{
        training_dataset$home_btts_percentage[i] <- percentage_home[counter_home-1]
      }
      counter_home= counter_home+1       
    }
    if (training_dataset$away_team_name[i] == team[k]) {
      # Add the value to the sum for "F"
      btts_away <- append(btts_away,training_dataset$btts[i])
      percentage_away <- append(percentage_away,  mean(btts_away==1))
      if (counter_away==1){
        training_dataset$away_btts_percentage[i] <- percentage_away[counter_away]
      }else{
        training_dataset$away_btts_percentage[i] <- percentage_away[counter_away-1]
      }
      counter_away= counter_away+1 
    }
  }
  btts_away<- c()
  btts_home<- c()
  percentage_away<- c()
  percentage_home<- c()
  counter_away = 1
  counter_home = 1
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
training_dataset <- swap (training_dataset, 53, 18)
training_dataset <- swap (training_dataset, 55, 19)
training_dataset <- swap (training_dataset, 56, 20)
training_dataset$goal_away_team<- scale(training_dataset$goal_away_team)
training_dataset$goal_home_team<- scale(training_dataset$goal_home_team)
#scaling of below variables reduces performance 
#training_dataset$goal_away_team<- scale(training_dataset$defence_game)
#training_dataset$goal_home_team<- scale(training_dataset$recent_conceeded_home)
#training_dataset$goal_away_team<- scale(training_dataset$percentage_half_home)
#training_dataset$goal_home_team<- scale(training_dataset$percentage_half_away)
#training_dataset$goal_away_team<- scale(training_dataset$percentage_half_home_conceeded)
#training_dataset$goal_home_team<- scale(training_dataset$percentage_half_away_conceeded)



#MODEL
model_dataset<- training_dataset %>% filter(Game.Week <= cut_off) 
model_dataset1<- training_dataset %>% filter(Game.Week <= cut_off) 
model_dataset <- model_dataset[,sapply(model_dataset, is.numeric)]
cols_to_keep <- c(4:19, 23, 24, 32:54)
model_dataset <- model_dataset[,cols_to_keep]

#first generation models
model <- glm(btts ~ delta_ppg  + Home.Team.Pre.Match.xG +  Away.Team.Pre.Match.xG + btts_percentage_pre_match + recent_goals_home+ recent_goals_away+ recent_conceeded_away + recent_conceeded_home + recent_conceeded_away:recent_goals_home+percentage_half_home+percentage_half_away, family =binomial, data = model_dataset)

model2<-glm(btts ~ delta_ppg  + Home.Team.Pre.Match.xG +  Away.Team.Pre.Match.xG +btts_percentage_pre_match + avarage_goals_home+ avarage_goals_away+ avarage_conceeded_away + avarage_conceeded_home + avarage_conceeded_home:avarage_goals_home+percentage_half_away+percentage_half_home , family =binomial, data = model_dataset)
#second generation models
model3 <- glm(btts ~ Away.Team.Pre.Match.xG  +goal_away_team+recent_goals_away+percentage_half_home_conceeded + recent_conceeded_home +percentage_half_home+percentage_half_away, family =binomial, data = model_dataset)
#most used 
model4 <- glm(btts ~ Away.Team.Pre.Match.xG  +delta_ppg+ + goal_away_team+ recent_conceeded_home+ percentage_half_home+ percentage_half_away+ percentage_half_home_conceeded+ percentage_half_away_conceeded , family =binomial, data = model_dataset)
model7 <- glm(btts ~ Away.Team.Pre.Match.xG  +defence_game+goal_away_team+delta_ppg+ + recent_conceeded_home+ percentage_half_home+ percentage_half_away+ percentage_half_home_conceeded+ percentage_half_away_conceeded, family =binomial, data = model_dataset)
model8 <- glm(btts ~ Away.Team.Pre.Match.xG  +defence_game+goal_away_team+delta_ppg+ + recent_conceeded_home+ percentage_half_home+ percentage_half_home_conceeded, family =binomial, data = model_dataset)


x <- as.matrix(model_dataset[, ])

# Create the response variable vector
y <- model_dataset$btts

# Fit the model
model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = c(0.1, 0.01, 0.001))
predictions <- predict(model, newx = testing_dataset, s = "lambda")
print(model$lambda.min)


full_model <- glm(btts ~ ., data = model_dataset, family = binomial())
#selected_model <- step(full_model, direction = "backward", trace = 0)
step_model <- stepAIC(full_model, direction = "both", trace = 0)

step_model <- update(step_model, ~.-avarage_goals_home )

# Extract the selected features

#

summary(step_model)

vif(step_model)

#+ avarage_goals_away + avarage_goals_home / + avarage_goals_home+ avarage_goals_away+ avarage_conceeded_away + avarage_conceeded_home + avarage_conceeded_away:avarage_goals_home

testing_dataset  <-  training_dataset %>% filter(Game.Week == 23)




#how do I know which factors to include in a model or to exclude ?? add percentage of both team to score 


predictions_model4 <- predict(model4, newdata = testing_dataset, type= "response")
predictions_model7 <- predict(model7, newdata = testing_dataset, type= "response")
predictions_model8  <- predict(model8, newdata = testing_dataset, type= "response")
testing_dataset <- data.frame(testing_dataset, predictions_model4, predictions_model7, predictions_model8)
# Create a binary vector of predictions based on the threshold
binary_predictions_4 <- ifelse(predictions_model4 > 0.49, 1, 0)
binary_predictions_7 <- ifelse(predictions_model7 > 0.49, 1, 0)
binary_predictions_8 <- ifelse(predictions_model8 > 0.49, 1, 0)
binary_predictions_step <- ifelse(predictions_step_model > 0.49, 1, 0)
testing_dataset$bet_model4 <- ifelse(testing_dataset$btts == binary_predictions_4, 1, 0)
testing_dataset$bet_model7 <- ifelse(testing_dataset$btts == binary_predictions_7, 1, 0)
testing_dataset$bet_model8 <- ifelse(testing_dataset$btts == binary_predictions_8, 1, 0)
testing_dataset$bet_step <- ifelse(testing_dataset$btts == binary_predictions_step, 1, 0)
percentage_won_model4 <-  (sum(testing_dataset$bet_model4 == 1) / length((testing_dataset$bet_model4)))*100
percentage_won_model7 <-  (sum(testing_dataset$bet_model7 == 1) / length((testing_dataset$bet_model7)))*100
percentage_won_model8 <-  (sum(testing_dataset$bet_model8 == 1) / length((testing_dataset$bet_model8)))*100
percentage_won_step <-  (sum(testing_dataset$bet_step == 1) / length((testing_dataset$bet_step)))*100
print(paste("model4", percentage_won_model4))
print(paste("model7", percentage_won_model7))
print(paste("model8", percentage_won_model8))
print(paste("step_model", percentage_won_step))
testing_dataset <- swap (testing_dataset, 59, 4)
testing_dataset <- swap (testing_dataset, 60, 5)
testing_dataset <- swap (testing_dataset, 61, 6)
testing_dataset <- swap (testing_dataset, 62, 7)


bet_lost <- testing_dataset  %>% filter(bet_model8 ==0 )
bet_won <- testing_dataset  %>% filter(bet_model8 ==1 )
##55<=prediction<=79
bet_lost$bin <- cut(bet_lost$predictions_model8, breaks=c(.0, .10, .20, .30, .40, .47, .53, .60, .70, .80, .90, .99999), labels = c(".0<.1",".1<.2",".2<.3",".3<.4", ".4<.47", ".47<.53", ".54<.6", ".6<.7", ".7<.8", ".8<.9", ".9<.99"))
bet_won$bin <- cut(bet_won$predictions_model8,  breaks=c(.0, .10, .20, .30, .40, .47, .53, .60, .70, .80, .90, .99999), labels = c(".0<.1",".1<.2",".2<.3",".3<.4", ".4<.47", ".47<.53", ".54<.6", ".6<.7", ".7<.8", ".8<.9", ".9<.99"))

table(bet_lost$bin)
table(bet_won$bin)


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

