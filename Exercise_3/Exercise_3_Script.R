### 1. Load the files and add gender, race, and tenure variables for examiners

## Prepare

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

# We can make the following observations about the two workgroups:

# Gender Distribution:
# Workgroup 161 has 37,275 female and 39,554 male examiners, with 12,966 examiners having unknown gender.
# Workgroup 162 has 51,412 female and 55,380 male examiners, with 34,598 examiners having unknown gender.

# In both workgroups, the number of male examiners is slightly higher than the number of female examiners. However, there are also a considerable number of examiners with unknown gender in both workgroups.

# Race Distribution:
# Workgroup 161 has 1,843 Hispanic, 2,452 Black, 19,528 Asian, and 65,972 White examiners.
# Workgroup 162 has 3,884 Hispanic, 11,023 Black, 35,442 Asian, and 91,041 White examiners.

# In both workgroups, the majority of examiners are White, followed by Asian, Black, and Hispanic examiners. Workgroup 162 has a larger number of examiners for each race compared to Workgroup 161.

# Tenure Distribution (grouped by years):
# Both workgroups show a similar trend in tenure distribution, with the number of examiners generally increasing as the tenure in years increases.
# For both workgroups, the largest number of examiners fall into the 17-year tenure category. There are 32,366 examiners in Workgroup 161 and 72,199 examiners in Workgroup 162 with a 17-year tenure.
# A considerable number of examiners in both workgroups have unknown tenure, 3,731 in Workgroup 161 and 4,389 in Workgroup 162.


# In summary, the two workgroups have similar trends in terms of examiners' demographics. Both workgroups have slightly more male than female examiners, a majority of White examiners followed by Asian, Black, and Hispanic examiners, and a similar distribution of tenure years with the largest number of examiners having a 17-year tenure. Workgroup 162 has a larger number of examiners for each demographic category compared to Workgroup 161.



### 3. Create advice networks from `edges_sample` and calculate centrality scores for examiners in selected workgroups

# For this exercise, let's use Degree Centrality and Betweenness Centrality. Degree Centrality measures the number of direct connections an examiner has, while Betweenness Centrality measures the extent to which an examiner lies on the shortest paths between other examiners in the network. Both measures can help us identify influential or well-connected examiners in the network.

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

# Calculate Degree Centrality and Betweenness Centrality
degree_centrality <- degree(g_filtered, mode = "all")
betweenness_centrality <- betweenness(g_filtered, directed = TRUE)

# Add the centrality scores to the vertex attributes
V(g_filtered)$degree_centrality <- degree_centrality
V(g_filtered)$betweenness_centrality <- betweenness_centrality

# Merge centrality scores with the examiners' characteristics
centrality_scores <- data.frame(
  examiner_id = V(g_filtered)$name,
  workgroup = V(g_filtered)$workgroup,
  degree_centrality = V(g_filtered)$degree_centrality,
  betweenness_centrality = V(g_filtered)$betweenness_centrality
)

applications_centrality <- applications %>%
  select(examiner_id, gender, race, tenure_days) %>%
  inner_join(centrality_scores, by = "examiner_id")

# Examine the results
print(applications_centrality)
