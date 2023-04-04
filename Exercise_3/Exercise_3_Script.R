### 1. Load the files and add gender, race, and tenure variables for examiners

## Prepare

# Load required libraries
library(tidyverse)
library(lubridate)
library(arrow)
library(gender)
library(wru)

# Load data
applications <- read_parquet("app_data_sample.parquet")
edges_sample <- read_csv("edges_sample.csv")


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



### 2. Pick two workgroups.How do they compare on examiners’ demographics?

# Choose workgroups
workgroup1 <- "161"
workgroup2 <- "162"


## Summary statistics

# Filter the applications dataset for the chosen workgroups
workgroups_data <- applications %>%
  filter(substr(examiner_art_unit, 1, 3) %in% c(workgroup1, workgroup2))

# Summary statistics for demographics
summary_stats <- workgroups_data %>%
  group_by(workgroup = substr(examiner_art_unit, 1, 3)) %>%
  summarise(
    avg_tenure_days = mean(tenure_days, na.rm = TRUE),
    proportion_female = mean(proportion_female, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(across(c(avg_tenure_days, proportion_female), round, 2))

# Print summary statistics
print(summary_stats)


## Let's take a look at demographics in workgroups

# Load required libraries
library(dplyr)

# Filter the applications dataset for the chosen workgroups
workgroups_data <- applications %>%
  filter(substr(examiner_art_unit, 1, 3) %in% c(workgroup1, workgroup2))

# Gender distribution
gender_distribution <- workgroups_data %>%
  group_by(workgroup = substr(examiner_art_unit, 1, 3), gender) %>%
  summarise(count = n()) %>%
  arrange(workgroup, count, .by_group = TRUE)

# Race distribution
race_distribution <- workgroups_data %>%
  group_by(workgroup = substr(examiner_art_unit, 1, 3), race) %>%
  summarise(count = n()) %>%
  arrange(workgroup, count, .by_group = TRUE)

# Tenure distribution (grouped by years)
tenure_distribution <- workgroups_data %>%
  mutate(tenure_years = floor(tenure_days / 365)) %>%
  group_by(workgroup = substr(examiner_art_unit, 1, 3), tenure_years) %>%
  summarise(count = n()) %>%
  arrange(workgroup, tenure_years)

# Display summary tables
print(gender_distribution)
print(race_distribution)
print(tenure_distribution, n=37)



## Plots for demographics

# Load required libraries
library(ggplot2)

# Plot for gender distribution
gender_plot <- workgroups_data %>%
  ggplot(aes(x = gender, fill = gender)) +
  geom_bar() +
  facet_wrap(~substr(examiner_art_unit, 1, 3)) +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count") +
  theme_minimal()

# Plot for race distribution
race_plot <- workgroups_data %>%
  ggplot(aes(x = race, fill = race)) +
  geom_bar() +
  facet_wrap(~substr(examiner_art_unit, 1, 3)) +
  labs(title = "Race Distribution",
       x = "Race",
       y = "Count") +
  theme_minimal()

# Plot for tenure distribution
tenure_plot <- workgroups_data %>%
  ggplot(aes(x = tenure_days)) +
  geom_histogram(binwidth = 30) +
  facet_wrap(~substr(examiner_art_unit, 1, 3)) +
  labs(title = "Tenure Distribution",
       x = "Tenure (days)",
       y = "Count") +
  theme_minimal()

# Display plots
print(gender_plot)
print(race_plot)
print(tenure_plot)



### 3. Create advice networks from `edges_sample` and calculate centrality scores for examiners in selected workgroups

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

# Filter the network to only include the two selected workgroups
g_filtered <- g %>% 
  induced_subgraph(V(g)[V(g)$workgroup %in% c(workgroup1, workgroup2)])

# Set plot options
par(mar = c(0, 0, 0, 0))
set.seed(123)

# Create the plot
plot(g_filtered,
     vertex.color = ifelse(V(g_filtered)$workgroup == workgroup1, "lightblue", "lightgreen"),
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.5,
     main = "Advice Networks for Workgroups 161 and 162")


## Calculate centrality scores for examiners in selected workgroups

# Create a mapping between examiner_id and workgroup in the applications dataset
examiner_workgroup_mapping <- applications %>% 
  select(examiner_id, workgroup) %>% 
  mutate(examiner_id = as.numeric(examiner_id)) %>% # Convert examiner_id to numeric
  distinct()

# Calculate Degree Centrality and Betweenness Centrality
degree_centrality <- degree(g_filtered, mode = "all")
betweenness_centrality <- betweenness(g_filtered, directed = TRUE)

# Add the centrality scores to the vertex attributes
V(g_filtered)$degree_centrality <- degree_centrality
V(g_filtered)$betweenness_centrality <- betweenness_centrality

# Merge centrality scores with the examiners' characteristics
centrality_scores <- data.frame(
  examiner_id = as.numeric(V(g_filtered)$name), # Convert examiner_id to numeric
  workgroup = V(g_filtered)$workgroup,
  degree_centrality = V(g_filtered)$degree_centrality,
  betweenness_centrality = V(g_filtered)$betweenness_centrality
)

applications_centrality <- applications %>%
  select(examiner_id, gender, race, tenure_days) %>% 
  mutate(examiner_id = as.numeric(examiner_id)) %>% # Convert examiner_id to numeric
  inner_join(centrality_scores, by = "examiner_id")

# Examine the results
print(applications_centrality)


## Characterize and discuss the relationship between centrality and other examiners’ characteristics

# TENURE
# Calculate correlations between centrality measures and tenure_days
correlation_results <- applications_centrality %>%
  select(degree_centrality, betweenness_centrality, tenure_days) %>%
  cor(use = "pairwise.complete.obs")

# Print the correlation results
print(correlation_results)

# Plot scatter plots to visualize the relationships
scatter_plot_degree_tenure <- ggplot(applications_centrality, aes(x = tenure_days, y = degree_centrality)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot: Degree Centrality vs Tenure",
       x = "Tenure (days)",
       y = "Degree Centrality") +
  theme_minimal()

scatter_plot_betweenness_tenure <- ggplot(applications_centrality, aes(x = tenure_days, y = betweenness_centrality)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot: Betweenness Centrality vs Tenure",
       x = "Tenure (days)",
       y = "Betweenness Centrality") +
  theme_minimal()

# Display the scatter plots
print(scatter_plot_degree_tenure)
print(scatter_plot_betweenness_tenure)


# RACE/GENDER

# Convert race and gender to numerical values (dummy coding)
dummy_coded_data <- applications_centrality %>%
  mutate(
    race_num = as.numeric(factor(race, levels = unique(race))),
    gender_num = as.numeric(factor(gender, levels = unique(gender)))
  )

# Calculate the correlation matrix
correlation_matrix <- cor(dummy_coded_data[, c("degree_centrality", "betweenness_centrality", "race_num", "gender_num")], use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)


# Add 'gender_num' and 'race_num' columns to the dataframe
applications_centrality <- applications_centrality %>%
  mutate(
    gender_num = case_when(
      gender == "male" ~ 1,
      gender == "female" ~ 2,
      TRUE ~ NA_real_
    ),
    race_num = case_when(
      race == "hispanic" ~ 1,
      race == "black" ~ 2,
      race == "asian" ~ 3,
      race == "white" ~ 4,
      TRUE ~ NA_real_
    )
  )

# Scatter plot for Degree Centrality vs. Gender
degree_gender_plot <- applications_centrality %>%
  ggplot(aes(x = gender_num, y = degree_centrality)) +
  geom_point(alpha = 0.1) +
  labs(title = "Degree Centrality vs. Gender",
       x = "Gender (1 = Male, 2 = Female)",
       y = "Degree Centrality") +
  theme_minimal()

# Display plot
print(degree_gender_plot)

# Scatter plot for Degree Centrality vs. Race
degree_race_plot <- applications_centrality %>%
  ggplot(aes(x = race_num, y = degree_centrality)) +
  geom_point(alpha = 0.1) +
  labs(title = "Degree Centrality vs. Race",
       x = "Race (1 = Hispanic, 2 = Black, 3 = Asian, 4 = White)",
       y = "Degree Centrality") +
  theme_minimal()

# Display plot
print(degree_race_plot)

# Scatter plot for Betweenness Centrality vs. Gender
betweenness_gender_plot <- applications_centrality %>%
  ggplot(aes(x = gender_num, y = betweenness_centrality)) +
  geom_point(alpha = 0.1) +
  labs(title = "Betweenness Centrality vs. Gender",
       x = "Gender (1 = Male, 2 = Female)",
       y = "Betweenness Centrality") +
  theme_minimal()

# Display plot
print(betweenness_gender_plot)

# Scatter plot for Betweenness Centrality vs. Race
betweenness_race_plot <- applications_centrality %>%
  ggplot(aes(x = race_num, y = betweenness_centrality)) +
  geom_point(alpha = 0.1) +
  labs(title = "Betweenness Centrality vs. Race",
       x = "Race (1 = Hispanic, 2 = Black, 3 = Asian, 4 = White)",
       y = "Betweenness Centrality") +
  theme_minimal()

# Display plot
print(betweenness_race_plot)
