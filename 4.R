### Prepare

# Load required libraries
library(tidyverse)
library(lubridate)
library(arrow)
library(gender)
library(wru)

# Load data
applications <- read_parquet("C:/Users/ulyan/OneDrive - McGill University/Documents/MMA/Winter II 2023/Org Network Analysis/Exercise 3/672_project_data/app_data_sample.parquet")
edges_sample <- read_csv("C:/Users/ulyan/OneDrive - McGill University/Documents/MMA/Winter II 2023/Org Network Analysis/Exercise 3/672_project_data/edges_sample.csv")


## Add gender variable

# Get unique examiner first names
examiner_names <- applications %>% distinct(examiner_name_first)

# Predict gender based on first names
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(examiner_name_first = name, gender, proportion_female)

# Join gender data back to the main applications dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")


## Add race variable

# Get unique examiner last names
examiner_surnames <- applications %>% select(surname = examiner_name_last) %>% distinct()

# Predict race based on last names
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% as_tibble()

# Select the race with the highest probability for each last name
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

# Join race data back to the main applications dataset
applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))


## Add tenure variable

# Extract examiner IDs and application dates
examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date)

# Convert dates to a consistent format
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

# Calculate the earliest and latest dates for each examiner and their tenure in days
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
  ) %>% 
  filter(year(latest_date)<2018)

# Join tenure data back to the main applications dataset
applications <- applications %>% left_join(examiner_dates, by = "examiner_id")



### Create advice networks from `edges_sample` and calculate centrality scores for all examiners

# Load required libraries
library(igraph)
library(dplyr)

# Create a list of unique examiner IDs from both ego_examiner_id and alter_examiner_id columns
unique_examiner_ids <- unique(c(edges_sample$ego_examiner_id, edges_sample$alter_examiner_id))

# Create an igraph object from the edges_sample data, specifying vertex names
g <- graph_from_data_frame(edges_sample[, c("ego_examiner_id", "alter_examiner_id")], directed = TRUE, vertices = data.frame(name = unique_examiner_ids))

# Calculate degree, betweenness, and closeness centrality for the entire dataset
centrality_entire <- data.frame(
  examiner_id = V(g)$name,
  degree_centrality = degree(g, mode = "out"),
  betweenness_centrality = betweenness(g, directed = TRUE),
  closeness_centrality = closeness(g, mode = "out")
)

# Convert examiner_id in centrality_entire to double
centrality_entire$examiner_id <- as.numeric(centrality_entire$examiner_id)

# Join the centrality data back to the main applications dataset
applications <- applications %>%
  left_join(centrality_entire, by = "examiner_id")



### 1. Create variable for application processing time

## Create a variable 'app_proc_time' that measures the number of days (or weeks) from the application filing date until the final decision on it (patented or abandoned).

# Calculate the processing time
applications <- applications %>%
  mutate(
    final_decision_date = coalesce(patent_issue_date, abandon_date),
    app_proc_time = as.numeric(difftime(final_decision_date, filing_date, units = "days"))
  )



### 2. Linear regression models 

# Remove rows with missing values in degree, betweenness, or closeness centrality
applications_clean <- applications %>%
  filter(!is.na(degree_centrality),
         !is.na(betweenness_centrality),
         !is.na(closeness_centrality))

# Estimate the linear regression model with degree_centrality as the independent variable
degree_model <- lm(
  app_proc_time ~ degree_centrality + gender + race + tenure_days,
  data = applications_clean
)

# Print the summary of the model
summary(degree_model)
# The model has very low explanatory power. The adjusted R-squared value is 0.005376, which means that only about 0.54% of the variation in app_proc_time can be explained by the model. This is quite low, indicating that the model does not fit the data well.


# Betweenness centrality linear regression model
betweenness_model <- lm(
  app_proc_time ~ betweenness_centrality + gender + race + tenure_days,
  data = applications_clean
)
summary(betweenness_model)


# Closeness centrality linear regression model
closeness_model <- lm(
  app_proc_time ~ closeness_centrality + gender + race + tenure_days,
  data = applications_clean
)
summary(closeness_model)


# Combined centrality linear regression model
combined_model <- lm(
  app_proc_time ~ degree_centrality + betweenness_centrality + closeness_centrality + gender + race + tenure_days,
  data = applications_clean
)
summary(combined_model)
# The combined model (including degree, betweenness, and closeness centralities) has an adjusted R-squared of 0.009789, while the closeness_model has an adjusted R-squared of 0.009607. Although the combined model has a slightly higher adjusted R-squared, the improvement is marginal.



### 3. Does this relationship differ by examiner gender?

# Degree centrality model with interaction
degree_gender_interaction <- lm(
  app_proc_time ~ degree_centrality * gender + race + tenure_days,
  data = applications_clean
)
summary(degree_gender_interaction)

# Betweenness centrality model with interaction
betweenness_gender_interaction <- lm(
  app_proc_time ~ betweenness_centrality * gender + race + tenure_days,
  data = applications_clean
)
summary(betweenness_gender_interaction)

# Closeness centrality model with interaction
closeness_gender_interaction <- lm(
  app_proc_time ~ closeness_centrality * gender + race + tenure_days,
  data = applications_clean
)
summary(closeness_gender_interaction)

# Combined model with interaction
combined_gender_interaction <- lm(
  app_proc_time ~ (degree_centrality + betweenness_centrality + closeness_centrality) * gender + race + tenure_days,
  data = applications_clean
)
summary(combined_gender_interaction)
