# BTTS Oracle - The following project is on hold but I am open to collaboration. Feel free to contact me!

In this project, I explore classical machine learning techniques to predict whether both teams in Seria A matches will score. Each file serves distinct purposes in football analytics, ranging from model training to exploratory and experimental work. The BTTS_Oracle contains the models used for predictions. Other files are experimental works and, as such, are still under development. 
---------------------------------------------------------------------------------------------------------------------------------------------
**Python Folder**
Here are descriptive summaries for each of the files found in the Python folder:
1) **BTTS_Oracle.py**
A Python script for analyzing football match data to predict the "Both Teams to Score" (BTTS) outcome. It processes historical match statistics and uses machine learning models for predictions.

2) **BTTS_Oracle.ipynb**
A Jupyter Notebook implementing the BTTS prediction model. It includes data preprocessing, feature engineering, model training, evaluation, and visualization of results.

3) **scrape_formations.py**
A Python script for web scraping football formation data. It collects team lineup and formation information to enhance the dataset used for analysis.
---------------------------------------------------------------------------------------------------------------------------------------------
**R Folder**
Here are descriptive summaries for each of the files found in the R folder:

1) __betting_discarded_code.R__
Contains a series of functions and loops for calculating statistical metrics such as average and median goals, conceded goals, and shots for football matches. The code attempts to process training and testing datasets to compute these statistics for home and away teams, but the logic appears incomplete or deprecated. This file might hold discarded or experimental code not finalized for use​.

2) **betting_premier_league_2023.R**
This script analyzes the 2022-2023 English Premier League season data. It uses advanced statistical methods to calculate metrics like goals, shots, and team performance trends. The file preprocesses data and computes features like average and recent goals and shots for model training. It emphasizes game-week-specific trends and predictive features for betting analysis​.

3) **betting_serieA_2022.R**
Focused on the 2022-2023 Italian Serie A, this script processes team and player data to calculate performance indicators such as goals, shots, and halftime metrics. It uses FIFA player ratings and match statistics to generate predictive features. The file emphasizes league-specific adjustments and uses scaled features for statistical analysis and model building​.

4) **betting.R**
Processes historical Premier League data (2018-2019) to calculate match-level statistics for home and away teams, including goals, shots, and possession metrics. It integrates logistic regression modeling for binary outcomes like "both teams to score." This script serves as a base template for betting analysis with a focus on feature engineering and model evaluation​.

5) **overall_players.R**
Focuses on detailed player-level analysis in Serie A. It includes functions to calculate performance metrics for defenders, midfielders, and forwards. It uses advanced techniques such as Levenshtein distance and Jaccard similarity to clean and match player names across datasets. Outputs are aggregated to create team-level scores for defensive and offensive performance​.
---------------------------------------------------------------------------------------------------------------------------------------------
**Datasets & Research Papers folder**

- The datasets folder contains a collection of datasets from various leagues as highlighted in the file names. There are 4 main types of datasets (specified in the file names):
  1) Matches: Stats regarding all the various leagues' matches. The results of games are available until certain timepoints
  2) Teams: Stats regarding all the teams in the league of interest.
  3) Players: Stats regarding all the players in the league of interest. These are used to calculate more precise team stats.
  4) Fifa: Team stats based on the videogame FIFA

- The research paper folder aims to have empirical research to guide and inspire certain steps of the project 
