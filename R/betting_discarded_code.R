counter_a <- 1
counter_h <- 1
#use for avarage for goal home and away
for (k in 1:length(teams)){
  for (i in 1:nrow(matches)) {
    # Check the value in the condition column
    if (matches$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum <- sum + matches$home_team_goal_count[i]
      
      # Store the sum in the new column
      matches$avarage_goals_home[i] <- sum/counter_h
      counter_h= counter_h+1 
    } else if (matches$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away <- sum_away + matches$away_team_goal_count[i]
      
      # Store the sum in the new column
      matches$avarage_goals_away[i] <- sum_away /counter_a
      counter_a= counter_a+1 
    }
  }
  sum_away <- 0
  sum <-0
  counter_h <- 1
  counter_a <- 1
}

## use it for avarage conceeded
# Initialize a new column to store the sums
matches$avarage_conceeded_home <- NA
matches$avarage_conceeded_away <- NA
for (k in 1:length(teams)){
  for (i in 1:nrow(matches)) {
    # Check the value in the condition column
    if (matches$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      sum <- sum + matches$away_team_goal_count[i]
      
      # Store the sum in the new column
      matches$avarage_conceeded_home[i] <- sum/counter_h
      counter_h= counter_h+1 
    } else if (matches$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      sum_away <- sum_away + matches$home_team_goal_count[i]
      
      # Store the sum in the new column
      matches$avarage_conceeded_away[i] <- sum_away /counter_a
      counter_a= counter_a+1 
    }
  }
  sum_away <- 0
  sum <-0
  counter_h <- 1
  counter_a <- 1
}



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
      if (counter_home!= 0 & counter_home != 18){
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
      if (counter_away!= 0 & counter_away != 18 ){
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
      if (counter_home!= 0 & counter_home != 18 ){
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
      if (counter_away!= 0 & counter_away != 18 ){
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
      
      if (counter_home!= 0 & counter_home != 18){
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
      
      if (counter_away!= 0 & counter_away != 18 ){
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
training_dataset$avarage_conceeded_away<- NA
training_dataset$avarage_conceeded_home <- NA
for (k in 1:length(teams)){
  for (i in 1:nrow(training_dataset)) {
    # Check the value in the condition column
    if (training_dataset$home_team_name[i] == teams[k]) {
      # Add the value to the sum for "E"
      if (i> (nrow(training_dataset) -10)){
        training_dataset$avarage_conceeded_home[i] <- mean(sum)
      }else{
        sum<- append(sum, training_dataset$away_team_goal_count [i])
        training_dataset$avarage_conceeded_home[i] <- mean(sum)
      }
    } 
    if (training_dataset$away_team_name[i] == teams[k]) {
      # Add the value to the sum for "F"
      if (i> (nrow(training_dataset) -10)){
        training_dataset$avarage_conceeded_away[i] <- mean(sum_away)
      }else{
        sum_away <- append(sum_away, training_dataset$home_team_goal_count [i])
        training_dataset$avarage_conceeded_away[i] <- mean(sum_away)     
      }
    }
  }
  print(teams[k])
  print(mean(sum))
  sum_away <-c()
  sum <- c()
}

if (length(sum_away)>=5){
  recent_away <- append(recent_away, mean(tail(sum_away,5)))
}else{
  recent_away <- append(recent_away, mean(sum_away))
}
#testing_dataset <- testing_dataset[, -(33:34)]
#names(testing_dataset)[names(testing_dataset) == "avarage_shots_away"] = "away_team_shots_on_target"
#names(testing_dataset)[names(testing_dataset) == "avarage_shots_home"] = "home_team_shots_on_target"
