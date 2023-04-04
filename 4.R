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

## Plot advice networks

# Create an igraph object from the edges_sample data
g <- graph_from_data_frame(edges_sample[, c("ego_examiner_id", "alter_examiner_id")], directed = TRUE)

# Extract the first 3 digits of examiner_art_unit values
applications$workgroup <- substr(applications$examiner_art_unit, 1, 3)

# Create a mapping between examiner_id and workgroup in the applications dataset
examiner_workgroup_mapping <- applications %>%
  select(examiner_id, workgroup) %>%
  distinct()

# Add attributes to vertices in the network
V(g)$workgroup <- examiner_workgroup_mapping$workgroup[match(V(g)$name, examiner_workgroup_mapping$examiner_id)]

# Set plot options
par(mar = c(0, 0, 0, 0))
set.seed(123)

# Create the plot
plot(g,
     vertex.color = "lightblue",
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.5,
     main = "Advice Networks for All Workgroups")

## Calculate centrality scores for all examiners

# Calculate Degree, Betweenness and Closeness Centrality
degree_centrality <- degree(g, mode = "all")
betweenness_centrality <- betweenness(g, directed = TRUE)
closeness_centrality <- closeness(g, mode = "all")

# Add the centrality scores to the vertex attributes
V(g)$degree_centrality <- degree_centrality
V(g)$betweenness_centrality <- betweenness_centrality
V(g)$closeness_centrality <- closeness_centrality

# Merge centrality scores with the examiners' characteristics
centrality_scores <- data.frame(
  examiner_id = as.numeric(V(g)$name), # Convert examiner_id to numeric
  workgroup = V(g)$workgroup,
  degree_centrality = V(g)$degree_centrality,
  betweenness_centrality = V(g)$betweenness_centrality
)

applications_centrality <- applications %>%
  select(examiner_id, gender, race, tenure_days) %>%
  mutate(examiner_id = as.numeric(examiner_id)) %>% # Convert examiner_id to numeric
  inner_join(centrality_scores, by = "examiner_id")

closeness_scores <- data.frame(
  examiner_id = as.numeric(V(g)$name), # Convert examiner_id to numeric
  closeness_centrality = V(g)$closeness_centrality
)

applications_centrality <- applications_centrality %>%
  inner_join(closeness_scores, by = "examiner_id")

# Remove rows with missing values in degree, betweenness, or closeness centrality
applications_centrality_complete <- applications_centrality %>%
  drop_na(degree_centrality, betweenness_centrality, closeness_centrality)

# Examine the results
print(applications_centrality_complete)



### 1. Create variable for application processing time

## Create a variable 'app_proc_time' that measures the number of days (or weeks) from the application filing date until the final decision on it (patented or abandoned).

# Calculate the processing time
applications <- applications %>%
  mutate(
    final_decision_date = coalesce(patent_issue_date, abandon_date),
    app_proc_time = as.numeric(difftime(final_decision_date, filing_date, units = "days"))
  )

# Merge applications and applications_centrality_complete datasets using an inner join
merged_data <- applications %>%
  select(examiner_id, app_proc_time) %>%
  inner_join(applications_centrality_complete, by = "examiner_id")



### 2. Linear regression models 

# Estimate the linear regression model with degree_centrality as the independent variable
degree_model <- lm(
  app_proc_time ~ degree_centrality + gender + race + tenure_days,
  data = merged_data
)

# Print the summary of the model
summary(degree_model)