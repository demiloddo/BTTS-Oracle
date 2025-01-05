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
install.packages("XML")
library(rvest)
library(XML)
library(stringi)

get_consonants <- function(str) {
  # use grepl to extract all consonants
  consonants <- str_extract_all(str, "[^aeiouAEIOU[:punct:][:space:]]")[[1]]
  return(consonants)
}
count_consonants <- function(str) {
  # Define a vector of consonants
  consonants <- c("b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z")
  
  # Split the input string into individual characters
  chars <- strsplit(str, "")[[1]]
  
  # Count the number of consonants in the list
  num_consonants <- 0
  for (char in chars) {
    if (char %in% consonants) {
      num_consonants <- num_consonants + 1
    }
  }
  
  return(num_consonants)
}


compare_strings <- function(str1, str2) {
  # Convert the strings to lowercase to make the comparison case-insensitive
  str1 <- tolower(str1) 

  str2 <- tolower(str2)
  str1 <- get_consonants(str1)

  str2 <- get_consonants(str2)
  common_elements <- intersect(str1, str2)
  return(length(common_elements) >= 5)
}


levenshtein_distance <- function(s1, s2) {
  m <- nchar(s1)
  n <- nchar(s2)
  
  if (m == 0) {
    return(n)
  }
  
  if (n == 0) {
    return(m)
  }
  
  d <- matrix(0, nrow = m + 1, ncol = n + 1)
  for (i in 1:(m + 1)) {
    d[i, 1] <- i - 1
  }
  
  for (j in 1:(n + 1)) {
    d[1, j] <- j - 1
  }
  
  for (j in 2:(n + 1)) {
    for (i in 2:(m + 1)) {
      if (substr(s1, i - 1, i - 1) == substr(s2, j - 1, j - 1)) {
        d[i, j] <- d[i - 1, j - 1]
      } else {
        d[i, j] <- min(d[i - 1, j] + 1, d[i, j - 1] + 1, d[i - 1, j - 1] + 1)
      }
    }
  }
  
  return(d[m + 1, n + 1])
}

is_similar <- function(s1, s2, threshold = 2) {
  distance <- levenshtein_distance(s1, s2)
  
  if (distance <= threshold) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

jaccard_distance <- function(str1, str2) {
  str1_chars <- unique(strsplit(str1, "")[[1]])
  str2_chars <- unique(strsplit(str2, "")[[1]])
  common_chars <- intersect(str1_chars, str2_chars)
  total_chars <- union(str1_chars, str2_chars)
  if (length(total_chars) == 0) {
    return(0)
  } else {
    return(1 - length(common_chars) / length(total_chars))
  }
}


    
compare_and_rename_all <- function(df1, col1, col_team1, df2, col2, col_team2, df3) {
  for (i in 1:nrow(df1)) {
    for (j in 1:nrow(df2)) {
     #compare_strings(df1[i, col1], df2[j, col2])
      #is_similar(df1[i, col1], df2[j, col2]
      if ((jaccard_distance(df1[i, col1], df2[j, col2])<0.60) & (df1[i, col_team1]== df2[j, col_team2])) {
        print(df1[i, col1])
        print(df2[j, col2])
        #df1[i, col1] <- df2[j, col2]
        df3 <- rbind(df3, df1[i,])
        
      }
    }
  }
  return(df3)
}

swap <- function(DF, n, m)
{
  n <- if (class(n)=="character" & is.na(suppressWarnings(as.integer(n)))) which(colnames(DF)==n) else as.integer(n)
  m <- if (class(m)=="character" & is.na(suppressWarnings(as.integer(m)))) which(colnames(DF)==m) else as.integer(m)
  
  if (!(1<=n & n<=length(DF))) stop( "`n` represents invalid index!" )
  if (!(1<=m & m<=length(DF))) stop( "`m` represents invalid index!" )
  
  return (DF[ if (n==m) 1:length(DF) else c( (if (min(n,m)==1) c() else 1:(min(n,m)-1) ), (if (min(n,m)+1 == max(n,m)) (min(n,m)+1):(max(n,m)-1) else c( max(n,m), (min(n,m)+1):(max(n,m)-1), min(n,m))), (if (max(n,m)==length(DF)) c() else (max(n,m)+1):length(DF) ) ) ])
}

sum_away <-c()
sum_home <- c()

players <- read.csv("italy-serie-a-players-2022-to-2023-stats.csv", sep=",",header=TRUE)
teams<-read.csv("italy-serie-a-teams-2022-to-2023-stats.csv", sep=",",header=TRUE)
archive <- read.csv("2022-2023 Football Player Stats.csv", sep=";" ,header=TRUE)
archive <- subset(archive, archive[[6]] == "Serie A")
archive$Squad <- replace(archive$Squad, archive$Squad=="Inter", "Inter Milan")
archive$Squad <- replace(archive$Squad, archive$Squad=="Milan", "AC Milan")
lineups <-read.csv("lineups25.csv", sep="," ,header=TRUE)
lineups$team <- replace(lineups$team, lineups$team=="VER", "Hellas Verona")
lineups$team <- replace(lineups$team, lineups$team=="LAZ", "Lazio")
lineups$team <- replace(lineups$team, lineups$team=="SAM", "Sampdoria")
lineups$team <- replace(lineups$team, lineups$team=="CRE", "Cremonese")
lineups$team <- replace(lineups$team, lineups$team=="ROM", "Roma")
lineups$team <- replace(lineups$team, lineups$team=="JUV", "Juventus")
lineups$team <- replace(lineups$team, lineups$team=="TOR", "Torino")
lineups$team <- replace(lineups$team, lineups$team=="NAP", "Napoli")
lineups$team <- replace(lineups$team, lineups$team=="NAP", "Napoli")
lineups$team <- replace(lineups$team, lineups$team=="MON", "Monza")
lineups$team <- replace(lineups$team, lineups$team=="EMP", "Empoli")
lineups$team <- replace(lineups$team, lineups$team=="ATA", "Atalanta")
lineups$team <- replace(lineups$team, lineups$team=="UDI", "Udinese")
lineups$team <- replace(lineups$team, lineups$team=="FIO", "Fiorentina")
lineups$team <- replace(lineups$team, lineups$team=="MIL", "AC Milan")
lineups$team <- replace(lineups$team, lineups$team=="SPE", "Spezia")
lineups$team <- replace(lineups$team, lineups$team=="INT", "Inter Milan")
lineups$team <- replace(lineups$team, lineups$team=="LEC", "Lecce")
lineups$team <- replace(lineups$team, lineups$team=="SAL", "Salernitana")
lineups$team <- replace(lineups$team, lineups$team=="UDI", "Udinese")
lineups$team <- replace(lineups$team, lineups$team=="BOL", "Bologna")
lineups_defenders <- subset(lineups, lineups[[3]] == "D" )
team <- c(teams$common_name)

#FWMF+FWDF+FW -> pure forward
#+DFMF+DFFW  + MF -> midfielders, check if assists or goals >5
#DF = defenders


#defenders <-  players %>% filter(position == "Defender")
defenders <- subset(archive, archive[[4]] == "DF" & archive[[11]] >=400 )
defenders <-  defenders[c("Clr", "Clr", "BlkSh", "TklDriPast", "PasAtt", "PasTotCmp","AerWon", "CarMis", "PasBlocks", "Player","Squad", "Rk", "MP", "Starts", "X90s" )]

new_defenders <- data.frame(matrix(ncol = ncol(defenders), nrow = 0))
colnames(new_defenders) <- colnames(defenders)

defenders$Player <- iconv(defenders$Player, from = "ISO-8859-1", to = "UTF-8")

new_defenders<- compare_and_rename_all(defenders, "Player", "Squad", lineups_defenders, "name", "team", new_defenders)
dup_rows <- duplicated(new_defenders$Player)
new_defenders <- new_defenders[!dup_rows, ]

#Clr : Tackles in which the tackler's team won possession of the ball
#Int : Interceptions
#AerWon : Aerials won
# Clr : Clearances
#BlkSh : Number of times blocking a shot by standing in its path
#CarMis : Number of times a player failed when attempting to gain control of a ball
#PasBlocks : Blocked by the opponent who was standing it the path
#TklDriPast : Number of times dribbled past by an opposing player
defenders$Clr <- scale(defenders$Clr)
defenders$PasAtt <- scale(defenders$PasAtt)
defenders$AerWon <- scale(defenders$AerWon)
defenders$Clr <- scale(defenders$Clr)
defenders$BlkSh <- scale(defenders$BlkSh)
defenders$PasTotCmp <- scale(defenders$PasTotCmp)
#negative factors
defenders$TklDriPast <- scale(defenders$TklDriPast) 
defenders$PasBlocks <- scale(defenders$PasBlocks) 
defenders$CarMis <- scale(defenders$CarMis) 




#calculate team scores on each variable based on the player stats

teams$Clr <- NA
teams$PasAtt <- NA
teams$AerWon <- NA
teams$BlkSh <- NA
teams$PasTotCmp <- NA
teams$TklDriPast <- NA
teams$PasBlocks <- NA
teams$CarMis <- NA
Clr <- c()
PasAtt <- c()
AerWon <- c()
BlkSh <- c()
PasTotCmp <- c()
TklDriPast <- c()
PasBlocks <- c()
CarMis <- c()
for (k in 1:length(team)){
  for (i in 1:nrow(defenders)) {
    if (defenders$Squad[i] == team[k]) {
      Clr <- append(Clr, defenders$Clr[i])
      PasAtt <- append(PasAtt, defenders$PasAtt[i])
      AerWon <- append(AerWon, defenders$AerWon[i])
      BlkSh <- append(BlkSh, defenders$BlkSh[i])
      PasTotCmp <- append(PasTotCmp, defenders$PasTotCmp[i])
      TklDriPast <- append(TklDriPast, defenders$TklDriPast[i])
      PasBlocks <- append(PasBlocks, defenders$PasBlocks[i])
      CarMis <- append(CarMis, defenders$CarMis[i])
    }
  }
  teams$Clr[k] <- mean(Clr)
  teams$PasAtt[k] <- mean(PasAtt)
  teams$AerWon[k] <- mean(AerWon)
  teams$BlkSh[k] <- mean(BlkSh)
  teams$PasTotCmp[k] <- mean(PasTotCmp)
  teams$TklDriPast[k] <- mean(TklDriPast)
  teams$PasBlocks[k] <- mean(PasBlocks)
  teams$CarMis[k] <- mean(CarMis)
  Clr <- c()
  PasAtt <- c()
  AerWon <- c()
  BlkSh <- c()
  PasTotCmp <- c()
  TklDriPast <- c()
  PasBlocks <- c()
  CarMis <- c()
}
teams <- swap (teams, 294, 3)
teams <- swap (teams, 295, 4)
teams <- swap (teams, 296, 5)
teams <- swap (teams, 299, 6)
teams <- swap (teams, 300, 7)
teams <- swap (teams, 301, 8)
teams <- swap (teams, 302, 9)
teams <- swap (teams, 26, 9)
teams <- swap (teams, 43, 12)
#do linear regression based on the goals conceeded by a team to understand weights of each varibale

model <- lm(clean_sheets ~  Clr +PasTotCmp+ PasBlocks+goals_conceded, data = teams)
summary(model)
vif(model)
#consider total goal conceeded by a team with a weight of 30 percent in final overall score
defence<- predict(model, newdata = teams, type= "response")
teams<- data.frame(teams,defence)
teams$defence <- 70 + (teams$defence - min(teams$defence)) / 
  (max(teams$defence) - min(teams$defence)) * 30 
teams <- swap (teams, 302, 10)

#OVERALL MIDFIELDER
midfielders <- subset(archive, (archive[[4]] == "DFMF" | archive[[4]] == "DFFW" | archive[[4]] == "MF" | archive[[4]] == "MFFW" | archive[[4]] == "MFDF") & archive[[11]] >=1000 )
#MIDIFELDER - DEFENSIVE TRAITS 

#MIDIFELDER - OFFENSIVE TRAITS 
#GCA
#PasAss
#GCA : Goal-creating actions
#goals : Shot-creating actions
#GcaPassLive
#CarPrgDist : Total distance, in yards, a player moved the ball while controlling it with their feet towards the opponent's goal

midfielders$Assists <- scale(midfielders$Assists) 
#midfielders <- midfielders %>% 
#  mutate(goals = scale(goals, center = TRUE, scale = TRUE))
midfielders$PasAss <- scale(midfielders$PasAss) 
midfielders$GcaPassLive <- scale(midfielders$GcaPassLive) 
midfielders$Goals <- scale(midfielders$Goals) 
midfielders$CarPrgDist <- scale(midfielders$CarPrgDist) 


teams$Assists <- NA
teams$PasAss <- NA
teams$GcaPassLive <- NA
teams$Goals <- NA
teams$CarPrgDist <- NA


Assists <- c()
PasAss <- c()
GcaPassLive <- c()
Goals <- c()
CarPrgDist <- c()
for (k in 1:length(team)){
  for (i in 1:nrow(midfielders)) {
    if (midfielders$Squad[i] == team[k]) {
      Assists <- append(Goals, midfielders$Assists[i])
      PasAss <- append(PasAss, midfielders$G.Sh[i])
      GcaPassLive <- append(GcaPassLive, midfielders$GcaPassLive[i])
      Goals <- append(Goals, midfielders$Goals[i])
      CarPrgDist <- append(CarPrgDist, midfielders$CarPrgDist[i])
    }
  }
  teams$Assists[k] <- mean(Assists)
  teams$PasAss[k] <- mean(PasAss)
  teams$GcaPassLive[k] <- mean(GcaPassLive)
  teams$Goals[k] <- mean(Goals)
  teams$CarPrgDist[k] <- mean(CarPrgDist)
  Assists <- c()
  PasAss <- c()
  GcaPassLive <- c()
  Goals <- c()
  CarPrgDist <- c()
}

model <- lm(goals_scored_per_match ~   GcaPassLive+ PasAss  + Assists, data = teams)
summary(model)
vif(model)
#consider total goal conceeded by a team with a weight of 30 percent in final overall score



#OVERALL ATTACK; #FWMF+FWDF+FW -> pure forward + #+DFMF+DFFW  + MF -> midfielders, check if assists or goals >5

forwards <- subset(archive, (archive[[4]] == "FWMF" | archive[[4]] == "FWDF" | archive[[4]] == "FW") & archive[[11]] >=900 )
offensive <- subset(archive, (archive[[4]] == "DFMF" | archive[[4]] == "DFFW" | archive[[4]] == "MF" |  archive[[4]] == "MFFW") & archive[[11]] >=900 & (archive[[13]]>=3 | archive[[37]]>=5) )

# Assists= Shots on target (Does not include penalty kicks), G/Sh : Goals per shot,
# Assists, PasAss=  Passes that directly lead to a shot (assisted shots),
#GcaDrib=Successful dribbles that lead to a goal, GcaSh= Shots that lead to another goal-scoring shot
#TouAttPen= GcaPassLive in attacking penalty area; AerWon : Aerials won
#ToAtt : Number of defenders taken on successfully, by dribbling past them


forwards$Assists <- scale(forwards$Assists)
forwards$G.Sh <- scale(forwards$G.Sh)
forwards$Assists <- scale(forwards$Assists)
forwards$PasAss <- scale(forwards$PasAss)
forwards$GcaDrib <- scale(forwards$GcaDrib)
forwards$GcaSh <- scale(forwards$GcaSh)
forwards$AerWon <- scale(forwards$AerWon)
forwards$TouAttPen <- scale(forwards$TouAttPen) 
forwards$GCA <- scale(forwards$GCA) 
forwards$PPA <- scale(forwards$PPA) 


teams$Assists <- NA
teams$G.Sh <- NA
teams$Assists <- NA
teams$GcaDrib <- NA
teams$GcaSh <- NA
teams$AerWon <- NA
teams$TouAttPen <- NA
teams$PPA <- NA
teams$GCA <- NA

Assists <- c()
G.Sh <- c()
Assists <- c()
GcaDrib <- c()
GcaSh <- c()
AerWon <- c()
TouAttPen <- c()
PPA <- c()
GCA <- c()
for (k in 1:length(team)){
  for (i in 1:nrow(forwards)) {
    if (forwards$Squad[i] == team[k]) {
      Assists <- append(Assists, forwards$Assists[i])
      G.Sh <- append(G.Sh, forwards$G.Sh[i])
      Assists <- append(Assists, forwards$Assists[i])
      GcaDrib <- append(GcaDrib, forwards$GcaDrib[i])
      GcaSh <- append(GcaSh, forwards$GcaSh[i])
      AerWon <- append(AerWon, forwards$AerWon[i])
      TouAttPen <- append(TouAttPen, forwards$TouAttPen[i])
      PPA <- append(PPA, forwards$PPA[i])
      GCA<- append(GCA, forwards$GCA[i])
      }
  }
  teams$Assists[k] <- mean(Assists )
  teams$G.Sh[k] <- mean(G.Sh)
  teams$Assists[k] <- mean(Assists)
  teams$GcaDrib[k] <- mean(GcaDrib)
  teams$GcaSh[k] <- mean(GcaSh)
  teams$AerWon[k] <- mean(AerWon)
  teams$TouAttPen[k] <- mean(TouAttPen)
  teams$PPA[k] <- mean(PPA)
  teams$GCA[k] <- mean(GCA)
  Assists <- c()
  G.Sh <- c()
  Assists <- c()
  GcaDrib <- c()
  GcaSh <- c()
  AerWon <- c()
  TouAttPen <- c()
  PPA <- c()
  GCA <-c()
}

teams <- swap (teams, 304, 13)
teams <- swap (teams, 305, 14)
teams <- swap (teams, 306, 15)
teams <- swap (teams, 307, 16)
teams <- swap (teams, 308, 17)
teams <- swap (teams, 309, 18)
teams <- swap (teams, 310, 19)

#GCA OR GcaSh
model_attack <- lm(goals_scored_per_match ~ G.Sh + GCA  + Assists +TouAttPen  , data = teams)
summary(model_attack)
vif(model_attack)
attack <- predict(model_attack, newdata = teams, type= "response")
teams<- data.frame(teams,attack)
teams$attack <- 70 + (teams$attack - min(teams$attack)) / 
  (max(teams$attack) - min(teams$attack)) * 30 
teams <- swap (teams, 314, 21)
teams <- swap (teams, 262, 22)







players <-  players %>% filter(appearances_overall >= 5)
cols_to_remove <- c(2:6)
players <- players[,-cols_to_remove]

players$ per_90_min_home <- NA
players$ per_90_min_away <- NA
for (i in 1:nrow(players)) {
  players$per_90_min_home[i] <- players$minutes_played_home[i]/90
  players$per_90_min_away[i] <- players$minutes_played_away[i]/90
}
players$assists_per_90_home <- NA
players$assists_per_90_away <- NA
for (i in 1:nrow(players)) {
  players$assists_per_90_home[i] <- players$assists_home[i]/players$per_90_min_home[i]
  players$assists_per_90_away[i] <-  players$assists_away[i]/players$per_90_min_away[i]
}

midfielders <- players %>% filter(position == "Midfielder")
midfielders$position <- ifelse((midfielders$goals_overall >= 3 | midfielders$assists_overall >= 3 ),"Forward", "Midfielder" )
# Use subset to extract rows from df1 where the value of column B is "yes"
offensive <- subset(midfielders, position == "Forward")
wrong_position <- subset(offensive, offensive$full_name== "Ángel Di María" | offensive$full_name== "Boulaye Dia" | offensive$full_name== 'Khvicha Kvaratskhelia' 
                         | offensive$full_name =='Felipe Anderson Pereira Gomes' | offensive$full_name== 'Mattia Zaccagni')
offensive <- subset(offensive, offensive$full_name != "Ángel Di María" & offensive$full_name != "Boulaye Dia" & offensive$full_name != 'Khvicha Kvaratskhelia' 
                         & offensive$full_name !='Felipe Anderson Pereira Gomes' & offensive$full_name != 'Mattia Zaccagni')
#&offensive$full_name != 'Jeremie Boga' &offensive$full_name != 'Joaquin Correa')

offensive$goals_per_90_home <- scale(offensive$goals_per_90_home)
offensive$goals_per_90_away <- scale(offensive$goals_per_90_away)

offensive$assists_per_90_home <- scale(offensive$assists_per_90_home)
offensive$assists_per_90_away <- scale(offensive$assists_per_90_away)

offensive$goals_per_90_overall <- scale(offensive$goals_per_90_overall)
offensive$assists_per_90_overall<- scale(offensive$assists_per_90_overall)

offensive$attack_home<- rowSums((.30*offensive$goals_per_90_home)+ (.70*offensive$assists_per_90_home))
offensive$attack_away<- rowSums((.30*offensive$goals_per_90_away)+ (.70*offensive$assists_per_90_away))
offensive$attack_overall<- rowSums((.30*offensive$goals_per_90_overall)+(.70*offensive$assists_per_90_overall))

# Scale the overall score to a range of 1 to 100
offensive$attack_home <- 70 + (offensive$attack_home - min(offensive$attack_home)) / 
  (max(offensive$attack_home) - min(offensive$attack_home)) * 30

offensive$attack_away <- 70+ (offensive$attack_away - min(offensive$attack_away)) / 
  (max(offensive$attack_away) - min(offensive$attack_away)) * 30

offensive$attack_overall <- 70+ (offensive$attack_overall - min(offensive$attack_overall)) / 
  (max(offensive$attack_overall) - min(offensive$attack_overall)) * 30 


# Use rbind to combine df_yes with df2
forwards <- players %>% filter(position == "Forward")
forwards <- rbind(wrong_position, forwards)

forwards$goals_per_90_home <- scale(forwards$goals_per_90_home)
forwards$goals_per_90_away <- scale(forwards$goals_per_90_away)

forwards$assists_per_90_home <- scale(forwards$assists_per_90_home)
forwards$assists_per_90_away <- scale(forwards$assists_per_90_away)

forwards$goals_per_90_overall <- scale(forwards$goals_per_90_overall)
forwards$assists_per_90_overall<- scale(forwards$assists_per_90_overall)

forwards$attack_home<- rowSums((.70*forwards$goals_per_90_home)+ (.30*forwards$assists_per_90_home))
forwards$attack_away<- rowSums((.70*forwards$goals_per_90_away)+(.30*forwards$assists_per_90_away))
forwards$attack_overall<- rowSums((.70*forwards$goals_per_90_overall)+(.30*forwards$assists_per_90_overall))

forwards$attack_home <- 70+ (forwards$attack_home - min(forwards$attack_home)) / 
  (max(forwards$attack_home) - min(forwards$attack_home)) * 30 

forwards$attack_away <- 70 + (forwards$attack_away - min(forwards$attack_away)) / 
  (max(forwards$attack_away) - min(forwards$attack_away)) * 30

forwards$attack_overall <- 70+ (forwards$attack_overall - min(forwards$attack_overall)) / 
  (max(forwards$attack_overall) - min(forwards$attack_overall)) * 30


forwards <- rbind(offensive, forwards)

sum_overall <- c()
teams$attack_home <- NA
teams$attack_away<- NA
teams$attack_overall<-NA

for (k in 1:length(team)){
  for (i in 1:nrow(forwards)) {
         # Check the value in the condition column
    if (team[k] == forwards$Current.Club[i]){
      sum_home <- append(sum_home, forwards$attack_home [i])
      sum_away <- append(sum_away, forwards$attack_away [i]) 
      sum_overall <- append(sum_away, forwards$attack_overall[i])
      teams$attack_home[k]<- mean(sum_home)
      teams$attack_away[k]<- mean(sum_away)
      teams$attack_overall[k]<- mean(sum_overall)
     }
  }
  sum_home<- c()
  sum_away <- c()
  sum_overall <- c()
}


teams <- swap (teams, 304, 12)
teams <- swap (teams, 305, 13)
teams <- swap (teams, 306, 14)

