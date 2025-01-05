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

swap <- function(DF, n, m)
{
  n <- if (class(n)=="character" & is.na(suppressWarnings(as.integer(n)))) which(colnames(DF)==n) else as.integer(n)
  m <- if (class(m)=="character" & is.na(suppressWarnings(as.integer(m)))) which(colnames(DF)==m) else as.integer(m)
  
  if (!(1<=n & n<=length(DF))) stop( "`n` represents invalid index!" )
  if (!(1<=m & m<=length(DF))) stop( "`m` represents invalid index!" )
  
  return (DF[ if (n==m) 1:length(DF) else c( (if (min(n,m)==1) c() else 1:(min(n,m)-1) ), (if (min(n,m)+1 == max(n,m)) (min(n,m)+1):(max(n,m)-1) else c( max(n,m), (min(n,m)+1):(max(n,m)-1), min(n,m))), (if (max(n,m)==length(DF)) c() else (max(n,m)+1):length(DF) ) ) ])
}
dataset<-read.csv("england-premier-league-teams-2018-to-2019-stats.csv", sep=",",header=TRUE)
matches <- read.csv("england-premier-league-matches-2018-to-2019-stats.csv", sep=",",header=TRUE)
players <- read.csv("england-premier-league-players-2018-to-2019-stats.csv", sep=",",header=TRUE)
sum <- c()
sum_away <- c()
counter_values_away<- c()
counter_values_home<- c()
teams <- c(dataset$common_name)
teams[1]
counter_home = 0
counter_away = 0
# Initialize a new column to store the sums
training_dataset <-  matches %>% filter(Game.Week <= 19)

training_dataset <- swap (training_dataset, 7, 62)
training_dataset <- swap (training_dataset, 4, 8)
training_dataset <- swap (training_dataset, 8, 63)
training_dataset <- swap (training_dataset, 7, 13)
training_dataset <- swap (training_dataset, 8, 14)
training_dataset$avarage_goals_home <- NA
training_dataset$avarage_goals_away <- NA
## use it for median goal home and away
for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum<- append(sum, training_dataset$home_team_goal_count [i])
      training_dataset$avarage_goals_home[i] <- median(sum)     
    } 
    if (training_dataset$away_team_name[i] == teams[k]) {
      sum_away <- append(sum_away, training_dataset$away_team_goal_count [i])
      training_dataset$avarage_goals_away[i] <- median(sum_away)     
      }
  }
  sum_away <-c()
  sum <- c()
}
#use for meedian for goal conceeded at home and away
training_dataset$avarage_conceeded_away<- NA
training_dataset$avarage_conceeded_home <- NA
for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum<- append(sum, training_dataset$away_team_goal_count [i])
      training_dataset$avarage_conceeded_home[i] <- median(sum)
      } 
    if (training_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away <- append(sum_away, training_dataset$home_team_goal_count [i])
      training_dataset$avarage_conceeded_away[i] <- median(sum_away)     
    }
  }
  sum_away <-c()
  sum <- c()
}
##
training_dataset$avarage_shots_home<- NA
training_dataset$avarage_shots_away <- NA
#use it for median shots on target home and aaway
for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum<- append(sum, training_dataset$home_team_shots_on_target [i])
      training_dataset$avarage_shots_home[i] <- median(sum)     
      }
    if (training_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away <- append(sum_away, training_dataset$away_team_shots_on_target [i])
      training_dataset$avarage_shots_away[i] <- median(sum_away)
    }
  }
  sum_away <-c()
  sum <- c()
}

training_dataset$delta_ppg <-training_dataset$Pre.Match.PPG..Home. - training_dataset$Pre.Match.PPG..Away.

#data cleansing
training_dataset$btts <- ifelse((training_dataset$away_team_goal_count !=0 & training_dataset$home_team_goal_count !=0 ), 1, 0)
training_dataset <- swap (training_dataset, 65, 11)
training_dataset <- swap (training_dataset, 66, 12)
training_dataset <- swap (training_dataset, 67, 13)
training_dataset <- swap (training_dataset, 68, 14)
training_dataset <- swap (training_dataset, 69, 15)
training_dataset <- swap (training_dataset, 70, 16)
training_dataset <- swap (training_dataset, 71, 17)
#MODEL
model <- glm(btts ~ team_b_xg+ btts_percentage_pre_match + avarage_shots_home+ avarage_goals_away + avarage_goals_home + avarage_conceeded_away + avarage_conceeded_home+ avarage_shots_away:avarage_goals_away,  family =binomial, data = training_dataset)
summary(model)


testing_dataset  <-  matches %>% filter(Game.Week > 19)
matches$delta_ppg <-matches$Pre.Match.PPG..Home. - matches$Pre.Match.PPG..Away.
matches$btts <- ifelse((matches$away_team_goal_count !=0 & matches$home_team_goal_count !=0 ), 1, 0)


testing_dataset$avarage_goals_home <- NA
testing_dataset$avarage_goals_away <- NA
## use it for median goal home and away
for (k in 1:length(teams)){
  for (i in 1:nrow(testing_dataset)) {
    # Check the value in the condition column
    if (testing_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum<- append(sum, testing_dataset$home_team_goal_count [i])
      counter_values_home <- append(counter_values_home, median(sum))
      # Store the sum in the new column
      if (counter_home!= 0 & counter_home != 17){
        testing_dataset$avarage_goals_home[i] <- counter_values_home[(length(counter_values_home) -1)]
      } else{
        testing_dataset$avarage_goals_home[i] <- median(sum)     
      }
      counter_home = counter_home +1
    } 
    if (testing_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away <- append(sum_away, testing_dataset$away_team_goal_count [i])
      counter_values_away <- append(counter_values_away, median(sum_away))
      # Store the sum in the new column
      if (counter_away!= 0 & counter_away != 17 ){
        testing_dataset$avarage_goals_away[i] <- counter_values_away[(length(counter_values_away) -1)]
      } else{
        testing_dataset$avarage_goals_away[i] <- median(sum_away)     
      }
      print(sum_away)
      counter_away = counter_away +1
    }
  }
  sum <- c()
  counter_values_away<- c()
  counter_values_home<- c()
  counter_away <- 0
  counter_home <- 0
}
#use for meedian for goal conceeded at home and away
testing_dataset$avarage_conceeded_away<- NA
testing_dataset$avarage_conceeded_home <- NA
for (k in 1:length(teams)){
  for (i in 1:nrow(testing_dataset)) {
    # Check the value in the condition column
    if (testing_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum<- append(sum, testing_dataset$away_team_goal_count [i])
      counter_values_home <- append(counter_values_home, median(sum))
      if (counter_home!= 0 & counter_home != 17 ){
        testing_dataset$avarage_conceeded_home[i] <- counter_values_home[(length(counter_values_home) -1)]
      } else{
        testing_dataset$avarage_conceeded_home[i] <- median(sum)
      }
      counter_home = counter_home+1
    } 
    if (testing_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away <- append(sum_away, testing_dataset$home_team_goal_count [i])
      counter_values_away <- append(counter_values_away, median(sum_away))
      if (counter_away!= 0 & counter_away != 17 ){
        testing_dataset$avarage_conceeded_away[i] <- counter_values_away[(length(counter_values_away) -1)]
      } else{
        testing_dataset$avarage_conceeded_away[i] <- median(sum_away)     
      }
      counter_away = counter_away +1
    }
  }
  sum_away <-c()
  sum <- c()
  counter_values_away<- c()
  counter_values_home<- c()
  counter_away <- 0
  counter_home <- 0
}
##
testing_dataset$avarage_shots_home<- NA
testing_dataset$avarage_shots_away <- NA
#use it for median shots on target home and aaway
for (k in 1:length(teams)){
  for (i in 1:nrow(testing_dataset)) {
    # Check the value in the condition column
    if (testing_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum<- append(sum, testing_dataset$home_team_shots_on_target [i])
      counter_values_home <- append(counter_values_home, median(sum))
      
      if (counter_home!= 0 & counter_home != 17){
        testing_dataset$avarage_shots_home[i] <- counter_values_home[(length(counter_values_home) -1)]
      } else{
        testing_dataset$avarage_shots_home[i] <- median(sum)     
      }
      # Store the sum in the new column
      counter_home = counter_home+1
    } 
    if (testing_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away <- append(sum_away, testing_dataset$away_team_shots_on_target [i])
      counter_values_away <- append(counter_values_away, median(sum_away))
      
      if (counter_away!= 0 & counter_away != 17 ){
        testing_dataset$avarage_shots_away[i] <- counter_values_away[(length(counter_values_away) -1)]
      } else{
        testing_dataset$avarage_shots_away[i] <- median(sum_away)
      }
      counter_away = counter_away+1
    }
  }
  sum_away <-c()
  sum <- c()
  counter_values_away<- c()
  counter_values_home<- c()
  counter_away <- 0
  counter_home <- 0
}


#how do I know which factors to include in a model or to exclude ?? add percentage of both team to score 


predictions <- predict(model, newdata = testing_dataset, type= "response")
testing_dataset <- data.frame(testing_dataset, predictions)
# Create a binary vector of predictions based on the threshold
binary_predictions <- ifelse(predictions > 0.38, 1, 0)
testing_dataset$bet <- ifelse(testing_dataset$btts == binary_predictions, 1, 0)
percentage_won <-  (sum(testing_dataset$bet == 1) / length((testing_dataset$bet)))*100
print(percentage_won)
testing_dataset <- swap (testing_dataset, 3, 74)
testing_dataset <- swap (testing_dataset, 2, 73)
testing_dataset <- swap (testing_dataset, 1, 72)





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

