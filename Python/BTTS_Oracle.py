import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import RFE
from sklearn.metrics import accuracy_score
from sklearn.pipeline import Pipeline
from sklearn.feature_selection import SelectFromModel
from xgboost import XGBClassifier

# Load datasets
dataset = pd.read_csv("/Users/albertoloddo/Documents/Demetrio 2012/Desktop_Air_2012/BTTS Oracle/Datasets/italy-serie-a-teams-2022-to-2023-stats.csv")
matches = pd.read_csv("/Users/albertoloddo/Documents/Demetrio 2012/Desktop_Air_2012/BTTS Oracle/Datasets/italy-serie-a-matches-2022-to-2023-stats.csv")
fifa = pd.read_csv("/Users/albertoloddo/Documents/Demetrio 2012/Desktop_Air_2012/BTTS Oracle/Datasets/teams_fifa23.csv")

#DATA PREPROCESSING
# Filter FIFA data
fifa = fifa[fifa["League"] == "Italian Serie A (1)"]
fifa.reset_index(drop=True, inplace=True)  # This will reindex the dataset from 0

# List of index positions (from your previous code)
indexes = [1, 7, 14, 15, 16, 17, 18, 19]  # Update this list based on your specific indexes
team_names = ["Monza", "Inter Milan", "Torino", "Sampdoria", "Sassuolo", "Cremonese", "Salernitana", "Udinese"]

# Replacing 'Name' at the specified indices
for idx, team_name in zip(indexes, team_names):
    # Update 'Name' value at each given index
    if idx < len(fifa):  # Ensure the index is within the range of the dataset
        fifa.loc[idx, "Name"] = team_name
    else:
        print(f"Warning: Index {idx} is out of bounds.")


# Create new columns in matches
matches["delta_ppg"] = matches["Pre-Match PPG (Home)"] - matches["Pre-Match PPG (Away)"]
matches["btts"] = np.where((matches["away_team_goal_count"] != 0) & (matches["home_team_goal_count"] != 0), 1, 0)
#half_home and half_away are binary values to know whereas the home team or the away team has scored at half time
matches["half_home"] = np.where((matches["home_team_goal_count_half_time"] != 0), 1, 0)
matches["half_away"] = np.where((matches["away_team_goal_count_half_time"] != 0), 1, 0)

# Datasets of played matches (until the 21st game of the season) where variables are added from existing ones   
preprocessed_df = matches[matches["Game Week"] <= 21].copy()
preprocessed_df = preprocessed_df.sort_values(by="Game Week")

# Initialize variables
team = dataset['common_name']

# The variable `cut_off` represents the point in the dataset where historical data is divided
# from the future games we want to predict. All statistics and features used for model training
# are calculated based on data up to this point, ensuring the model does not have access to
# future information that would otherwise cause data leakage. Predictions are made for games
# occurring after the `cut_off` point
cut_off = 16  

# Initialize lists to store cumulative and recent averages for each team
sum_goals_home, sum_goals_away = [], []
week_home, week_away = [], []
recent_home, recent_away = [], []
counter_home, counter_away = 0, 0
# Loop through each team in the team list
for team_name in team:
    # Process each row in preprocessed_df
    for i, row in preprocessed_df.iterrows():
        # Process home team
        if row['home_team_name'] == team_name:
            sum_goals_home.append(row['home_team_goal_count'])
            week_home.append(np.mean(sum_goals_home))
            recent_home.append(np.mean(sum_goals_home[-5:]) if len(sum_goals_home) >= 5 else np.mean(sum_goals_home))

            # Apply cut_off condition
            if row['Game Week'] <= cut_off:
                # Update values based on current calculations
                preprocessed_df.at[i, 'average_goals_home'] = week_home[counter_home]
                preprocessed_df.at[i, 'recent_goals_home'] = recent_home[counter_home]
            else:
                # Use last pre-cut_off values for post-cut_off games
                preprocessed_df.at[i, 'average_goals_home'] = week_home[counter_home - 1]
                preprocessed_df.at[i, 'recent_goals_home'] = recent_home[counter_home - 1]
            
            counter_home += 1

        # Process away team
        if row['away_team_name'] == team_name:
            sum_goals_away.append(row['away_team_goal_count'])
            week_away.append(np.mean(sum_goals_away))
            recent_away.append(np.mean(sum_goals_away[-5:]) if len(sum_goals_away) >= 5 else np.mean(sum_goals_away))

            # Apply cut_off condition
            if row['Game Week'] <= cut_off:
                # Update values based on current calculations
                preprocessed_df.at[i, 'average_goals_away'] = week_away[counter_away]
                preprocessed_df.at[i, 'recent_goals_away'] = recent_away[counter_away]
            else:
                # Use last pre-cut_off values for post-cut_off games
                preprocessed_df.at[i, 'average_goals_away'] = week_away[counter_away - 1]
                preprocessed_df.at[i, 'recent_goals_away'] = recent_away[counter_away - 1]
            
            counter_away += 1
    sum_goals_home, sum_goals_away = [], [] 
    week_home, week_away = [], []
    recent_home, recent_away = [], []
    counter_home, counter_away = 0, 0

# Loop through each team in the team list
for team_name in team:
    # Process each row in preprocessed_df
    for i, row in preprocessed_df.iterrows():
        # Process home team
        if row['home_team_name'] == team_name:
            sum_goals_home.append(row['away_team_goal_count'])
            week_home.append(np.mean(sum_goals_home))
            recent_home.append(np.mean(sum_goals_home[-5:]) if len(sum_goals_home) >= 5 else np.mean(sum_goals_home))

            # Apply cut_off condition
            if row['Game Week'] <= cut_off:
                # Update values based on current calculations
                preprocessed_df.at[i, 'avarage_conceeded_home'] = week_home[counter_home]
                preprocessed_df.at[i, 'recent_conceeded_home'] = recent_home[counter_home]
            else:
                # Use last pre-cut_off values for post-cut_off games
                preprocessed_df.at[i, 'avarage_conceeded_home'] = week_home[counter_home - 1]
                preprocessed_df.at[i, 'recent_conceeded_home'] = recent_home[counter_home - 1]
            
            counter_home += 1

        # Process away team
        if row['away_team_name'] == team_name:
            sum_goals_away.append(row['home_team_goal_count'])
            week_away.append(np.mean(sum_goals_away))
            recent_away.append(np.mean(sum_goals_away[-5:]) if len(sum_goals_away) >= 5 else np.mean(sum_goals_away))

            # Apply cut_off condition
            if row['Game Week'] <= cut_off:
                # Update values based on current calculations
                preprocessed_df.at[i, 'avarage_conceeded_away'] = week_away[counter_away]
                preprocessed_df.at[i, 'recent_conceeded_away'] = recent_away[counter_away]
            else:
                # Use last pre-cut_off values for post-cut_off games
                preprocessed_df.at[i, 'avarage_conceeded_away'] = week_away[counter_away - 1]
                preprocessed_df.at[i, 'recent_conceeded_away'] = recent_away[counter_away - 1]
            
            counter_away += 1
    sum_goals_home, sum_goals_away = [], [] 
    week_home, week_away = [], []
    recent_home, recent_away = [], []
    counter_home, counter_away = 0, 0

# Loop through each team in the team list
for team_name in team:
    # Process each row in preprocessed_df
    for i, row in preprocessed_df.iterrows():
        # Process home team
        if row['home_team_name'] == team_name:
            sum_goals_home.append(row['home_team_shots_on_target'])
            week_home.append(np.mean(sum_goals_home))
            recent_home.append(np.mean(sum_goals_home[-5:]) if len(sum_goals_home) >= 5 else np.mean(sum_goals_home))

            # Apply cut_off condition
            if row['Game Week'] <= cut_off:
                # Update values based on current calculations
                preprocessed_df.at[i, 'avarage_shots_home'] = week_home[counter_home]
                preprocessed_df.at[i, 'recent_shots_home'] = recent_home[counter_home]
            else:
                # Use last pre-cut_off values for post-cut_off games
                preprocessed_df.at[i, 'avarage_shots_home'] = week_home[counter_home - 1]
                preprocessed_df.at[i, 'recent_shots_home'] = recent_home[counter_home - 1]
            
            counter_home += 1

        # Process away team
        if row['away_team_name'] == team_name:
            sum_goals_away.append(row['away_team_shots_on_target'])
            week_away.append(np.mean(sum_goals_away))
            recent_away.append(np.mean(sum_goals_away[-5:]) if len(sum_goals_away) >= 5 else np.mean(sum_goals_away))

            # Apply cut_off condition
            if row['Game Week'] <= cut_off:
                # Update values based on current calculations
                preprocessed_df.at[i, 'avarage_shots_away'] = week_away[counter_away]
                preprocessed_df.at[i, 'recent_shots_away'] = recent_away[counter_away]
            else:
                # Use last pre-cut_off values for post-cut_off games
                preprocessed_df.at[i, 'avarage_shots_away'] = week_away[counter_away - 1]
                preprocessed_df.at[i, 'recent_shots_away'] = recent_away[counter_away - 1]
            
            counter_away += 1
    sum_goals_home, sum_goals_away = [], [] 
    week_home, week_away = [], []
    recent_home, recent_away = [], []
    counter_home, counter_away = 0, 0


# Initialize the columns in the training dataset
preprocessed_df['percentage_half_home_conceded'] = np.nan
preprocessed_df['percentage_half_away_conceded'] = np.nan
preprocessed_df['recent_percentage_half_home_conceded'] = np.nan
preprocessed_df['recent_percentage_half_away_conceded'] = np.nan

# Loop over each team and calculate percentages
for k in range(len(team)):
    half_home = []
    half_away = []
    recent_home = []
    recent_away = []
    percentage_home = []
    percentage_away = []
    counter_home, counter_away = 0, 0

    for i, row in preprocessed_df.iterrows():
        # Process for home team
        if row['home_team_name'] == team[k]:
            half_home.append(preprocessed_df.loc[i, 'half_away'])
            percentage_home.append(np.mean(np.array(half_home) == 1))
            if len(half_home) >= 3:
                recent_home.append(np.mean(np.array(half_home[-3:]) == 1))
            else:
                recent_home.append(np.mean(np.array(half_home) == 1))
            
            if i < (len(preprocessed_df) - len(preprocessed_df[preprocessed_df['Game Week'] > cut_off])):
                preprocessed_df.loc[i, 'percentage_half_home_conceded'] = percentage_home[counter_home]
                preprocessed_df.loc[i, 'recent_percentage_half_home_conceded'] = recent_home[counter_home]
            else:
                preprocessed_df.loc[i, 'percentage_half_home_conceded'] = percentage_home[counter_home - 1]
                preprocessed_df.loc[i, 'recent_percentage_half_home_conceded'] = recent_home[counter_home - 1]
            
            counter_home += 1

        # Process for away team
        if row['away_team_name'] == team[k]:
            half_away.append(preprocessed_df.loc[i, 'half_home'])
            percentage_away.append(np.mean(np.array(half_away) == 1))
            if len(half_away) >= 3:
                recent_away.append(np.mean(np.array(half_away[-3:]) == 1))
            else:
                recent_away.append(np.mean(np.array(half_away) == 1))
            
            if i < (len(preprocessed_df) - len(preprocessed_df[preprocessed_df['Game Week'] > cut_off])):
                preprocessed_df.loc[i, 'percentage_half_away_conceded'] = percentage_away[counter_away]
                preprocessed_df.loc[i, 'recent_percentage_half_away_conceded'] = recent_away[counter_away]
            else:
                preprocessed_df.loc[i, 'percentage_half_away_conceded'] = percentage_away[counter_away - 1]
                preprocessed_df.loc[i, 'recent_percentage_half_away_conceded'] = recent_away[counter_away - 1]
            
            counter_away += 1

# Initialize additional columns with NaN
preprocessed_df['attack_away'] = np.nan
preprocessed_df['attack_home'] = np.nan
preprocessed_df['defence_away'] = np.nan
preprocessed_df['defence_home'] = np.nan

# Update attack and defense values using the FIFA dataset
for k in range(len(team)):
    team_name = team[k]
    attack_value = fifa.loc[fifa['Name'] == team_name, 'Attack'].values[0]
    defence_value = fifa.loc[fifa['Name'] == team_name, 'Defence'].values[0]
    
    preprocessed_df.loc[preprocessed_df['home_team_name'] == team_name, 'attack_home'] = attack_value
    preprocessed_df.loc[preprocessed_df['home_team_name'] == team_name, 'defence_home'] = defence_value
    preprocessed_df.loc[preprocessed_df['away_team_name'] == team_name, 'attack_away'] = attack_value
    preprocessed_df.loc[preprocessed_df['away_team_name'] == team_name, 'defence_away'] = defence_value

# Calculating goal differences and total attack/defense metrics
preprocessed_df['goal_home_team'] = preprocessed_df['defence_away'] - preprocessed_df['attack_home']
preprocessed_df['goal_away_team'] = preprocessed_df['defence_home'] - preprocessed_df['attack_away']
preprocessed_df['attack_game'] = preprocessed_df['attack_away'] + preprocessed_df['attack_home']
preprocessed_df['defence_game'] = preprocessed_df['defence_home'] + preprocessed_df['defence_away']
#PREDICTIVE MODELLING 

# Define feature set (remove features that are unavailable during gametime) and target variable
X = preprocessed_df.drop(columns=[
    "away_team_shots_on_target", "home_team_shots_on_target", "home_team_goal_count_half_time",
    "away_team_goal_count_half_time", "total_goals_at_half_time", "total_goal_count",
    "home_team_goal_count", "away_team_goal_count", "btts", "Game Week", "timestamp", "date_GMT",
    "status", 'home_team_name', 'away_team_name', 'referee', 'home_team_goal_timings',
    'away_team_goal_timings', 'stadium_name', 'half_home', 'half_away', 'attendance'
])
y = preprocessed_df["btts"]

#Random Forest
# Split the data into training and testing sets
X_train = X[preprocessed_df['Game Week'] <= cut_off]
y_train = y[preprocessed_df['Game Week'] <= cut_off]
X_test = X[preprocessed_df['Game Week'] > cut_off]
y_test = y[preprocessed_df['Game Week'] > cut_off]

# Initialize Random Forest Classifier for RFE
rf = RandomForestClassifier(n_estimators=100, random_state=42)

# Apply RFE for feature selection on training data only
selector = RFE(estimator=rf, n_features_to_select=10)  # Choose the number of features you want to select
selector.fit(X_train, y_train)

# Get the selected feature names from the selector
selected_columns = X_train.columns[selector.support_]

# Transform the training and test data to keep only the selected features
X_train_selected = X_train[selected_columns]
X_test_selected = X_test[selected_columns]

# Train Random Forest on selected features
rf_model = RandomForestClassifier(n_estimators=100, random_state=42)
rf_model.fit(X_train_selected, y_train)

# Make predictions on the test set
y_pred_prob = rf_model.predict_proba(X_test_selected)[:, 1]  # Probability of BTTS (1)

# Apply custom thresholding
y_pred_custom = np.where(y_pred_prob >= 0.6, 1, np.where(y_pred_prob <= 0.4, 0, np.nan))

# Filter out NaN values for both y_pred_custom and y_test
mask = ~np.isnan(y_pred_custom)
y_test_filtered = y_test[mask].reset_index(drop=True)
y_pred_filtered = pd.Series(y_pred_custom[mask]).reset_index(drop=True)

# Calculate accuracy on filtered predictions
accuracy = accuracy_score(y_test_filtered, y_pred_filtered)
print(f"Custom threshold accuracy (excluding values between 0.4 and 0.6): {accuracy * 100:.2f}%")

# Display a comparison DataFrame for manual inspection
comparison_df = pd.DataFrame({
    'True Value (y_test)': y_test_filtered,
    'Predicted Value (y_pred)': y_pred_filtered,
    'Predicted Probability (BTTS)': y_pred_prob[mask]
})
print(comparison_df)

# Get feature importance from the trained Random Forest model
importances = rf_model.feature_importances_
feature_importance_df = pd.DataFrame({
    'Feature': selected_columns,  # Use selected columns names
    'Importance': importances
})
print(feature_importance_df.sort_values(by='Importance', ascending=False))

#XGBoost

# Split the data into training and testing sets
X_train = X[preprocessed_df['Game Week'] <= cut_off]
y_train = y[preprocessed_df['Game Week'] <= cut_off]
X_test = X[preprocessed_df['Game Week'] > cut_off]
y_test = y[preprocessed_df['Game Week'] > cut_off]

# Initialize XGBoost Classifier
xgb_model = XGBClassifier(n_estimators=100, random_state=42)

# Train XGBoost model on the training data
xgb_model.fit(X_train, y_train)

# Make predictions on the test set
y_pred_prob = xgb_model.predict_proba(X_test)[:, 1]  # Probability of BTTS (1)

# Apply custom thresholding
y_pred_custom = np.where(y_pred_prob >= 0.6, 1, np.where(y_pred_prob <= 0.4, 0, np.nan))

# Filter out NaN values for both y_pred_custom and y_test
mask = ~np.isnan(y_pred_custom)
y_test_filtered = y_test[mask].reset_index(drop=True)
y_pred_filtered = pd.Series(y_pred_custom[mask]).reset_index(drop=True)

# Calculate accuracy on filtered predictions
accuracy = accuracy_score(y_test_filtered, y_pred_filtered)
print(f"Custom threshold accuracy (excluding values between 0.4 and 0.6): {accuracy * 100:.2f}%")

# Display a comparison DataFrame for manual inspection
comparison_df = pd.DataFrame({
    'True Value (y_test)': y_test_filtered,
    'Predicted Value (y_pred)': y_pred_filtered,
    'Predicted Probability (BTTS)': y_pred_prob[mask]
})
print(comparison_df)

# Get feature importance from the trained XGBoost model
importances = xgb_model.feature_importances_
feature_importance_df = pd.DataFrame({
    'Feature': X_train.columns,  # Use original X_train columns
    'Importance': importances
})
print(feature_importance_df.sort_values(by='Importance', ascending=False))

#