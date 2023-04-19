library(arrow)
library(dplyr)
library(tidygraph)
library(igraph)
library(visNetwork)
library(visNetwork)
library(tidyr)
library(readr)

library(tidyverse)
library(gender)
library(wru)
library(lubridate)
library(stargazer)

applications <- read_parquet("C:/Users/ulyan/OneDrive - McGill University/Documents/MMA/Winter II 2023/Org Network Analysis/Exercise 3/672_project_data/app_data_sample.parquet")
edges <- read_csv("C:/Users/ulyan/OneDrive - McGill University/Documents/MMA/Winter II 2023/Org Network Analysis/Exercise 3/672_project_data/edges_sample.csv")

## Preprocessing 

# Extract the first 3 digits of examiner_art_unit values and create 'workgroup' column
applications$workgroup <- substr(applications$examiner_art_unit, 1, 3)

#### Add gender variable
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


#### Add race variable
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

#### Add tenure variable
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



## Calculating Processing Time

applications$filing_date <- as.Date(applications$filing_date)
applications$patent_issue_date <- as.Date(applications$patent_issue_date)
applications$abandon_date <- as.Date(applications$abandon_date)

#Processing time in days
applications$app_proc_time_days <- ifelse(!is.na(applications$patent_issue_date), 
                                          difftime(applications$patent_issue_date, applications$filing_date, units = "days"), 
                                          difftime(applications$abandon_date, applications$filing_date, units = "days"))
#Processing time in weeks
applications$app_proc_time_weeks <- ifelse(!is.na(applications$patent_issue_date), 
                                           difftime(applications$patent_issue_date, applications$filing_date, units = "weeks"), 
                                           difftime(applications$abandon_date, applications$filing_date, units = "weeks"))
#Removing outliers
applications <- applications %>% 
  filter(app_proc_time_days >= 0)


## Centrality measures

# Edges dataset
edges_df <- edges
edges <- edges_df

# Filtering the data so that only the examiner_id still in the applications dataset stays in the edges dataset
edges <- edges %>%
  filter(ego_examiner_id %in% applications$examiner_id) %>%
  drop_na() %>%
  mutate(from = ego_examiner_id, to = alter_examiner_id) %>%
  select(from, to)

# Creating the nodes dataset with unique examiner_id from the applications dataset
nodes <- applications %>%
  dplyr::select(examiner_id) %>%
  dplyr::distinct() %>%
  dplyr::filter(examiner_id %in% edges$from | examiner_id %in% edges$to)

# Using graph to get the centrality scores
graph <- graph_from_data_frame(edges)

# Calculate centrality scores for degree, closeness, and betweenness centrality
deg <- degree(graph)
closeness<-closeness(graph, mode='out')
betweenness <- betweenness(graph, directed = TRUE)

deg_centrality <- data.frame(examiner_id = V(graph)$name, degree_centrality = deg)
close_centrality <- data.frame(examiner_id = V(graph)$name, closeness_centrality = closeness)
between_centrality <- data.frame(examiner_id = V(graph)$name, betweenness_centrality = betweenness)

# Merge new_dataset with degree centrality, closeness centrality, and betweenness centrality data frames
applications2 <- merge(applications, deg_centrality, by = "examiner_id")
applications2 <- merge(applications2, close_centrality, by = "examiner_id")
applications2 <- merge(applications2, between_centrality, by = "examiner_id")

# Remove rows with missing values in degree, betweenness, or closeness centrality.
applications2 <- applications2 %>%
  filter(!is.na(degree_centrality),
         !is.na(betweenness_centrality),
         !is.na(closeness_centrality))



## Linear Regression Models Estimating the Relationship between Centrality and Processing Time

model <- lm(app_proc_time_days ~ degree_centrality, data = applications2)
summary(model)

# Scatterplot of degree centrality vs. processing time (days)
ggplot(applications2, aes(x = degree_centrality, y = app_proc_time_days)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


model2 <- lm(app_proc_time_weeks ~ degree_centrality, data = applications2)
summary(model2)

# Scatterplot of degree centrality vs. processing time (weeks)
ggplot(applications2, aes(x = degree_centrality, y = app_proc_time_weeks)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


model3 <- lm(app_proc_time_days ~ betweenness_centrality, data = applications2)
summary(model3)

# Scatterplot of betweenness centrality vs. processing time (days)
ggplot(applications2, aes(x = betweenness_centrality, y = app_proc_time_days)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


model4 <- lm(app_proc_time_weeks ~ betweenness_centrality, data = applications2)
summary(model4)

# Scatterplot of betweenness centrality vs. processing time (weeks)
ggplot(applications2, aes(x = betweenness_centrality, y = app_proc_time_weeks)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


modell3 <- lm(app_proc_time_days ~ closeness_centrality, data = applications2)
summary(modell3)

# Scatterplot of closeness centrality vs. processing time (days)
ggplot(applications2, aes(x = closeness_centrality, y = app_proc_time_days)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)



modell4 <- lm(app_proc_time_weeks ~ closeness_centrality, data = applications2)
summary(modell4)

# Scatterplot of closeness centrality vs. processing time (weeks)
ggplot(applications2, aes(x = closeness_centrality, y = app_proc_time_weeks)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)



### Adding Race as a Predictor in Linear Regression Models

#### 'White' as refernce

#Use the race and set the reference on White
applications2$race<-factor(applications2$race)
applications2$race<-relevel(applications2$race, ref='white')
attach(applications2)


model5 <- lm(app_proc_time_days ~ degree_centrality+race, data = applications2)
#summary(model5)
coefplot(model5, intercept = FALSE)


model6 <- lm(app_proc_time_days ~ betweenness_centrality+race, data = applications2)
#summary(model6)
coefplot(model6, intercept = FALSE)


model7<- lm(app_proc_time_days ~ closeness_centrality+race, data = applications2)
#summary(model7)
coefplot(model7, intercept = FALSE)


#### 'Asian' as reference

# Use the race and set the reference on Asian
applications2$race<-relevel(applications2$race, ref='Asian')
attach(applications2)

model8<- lm(app_proc_time_days ~ degree_centrality+race, data = applications2)
summary(model8)
coefplot(model8, intercept = FALSE)


model9 <- lm(app_proc_time_days ~ betweenness_centrality+race, data = applications2)
summary(model9)
coefplot(model9, intercept = FALSE)


model10 <- lm(app_proc_time_days ~ closeness_centrality+race, data = applications2)
summary(model10)
coefplot(model10, intercept = FALSE)



### Regression models with gender, tenure days, and workgroups as additional predictors

model11 <- lm(app_proc_time_days ~ degree_centrality + gender, data = applications2)
#summary(model11)

model12 <- lm(app_proc_time_days ~ betweenness_centrality + gender, data = applications2)
#summary(model12)

model13 <- lm(app_proc_time_days ~ closeness_centrality + gender, data = applications2)
#summary(model13)

model14<- lm(app_proc_time_days ~ degree_centrality+tenure_days, data = applications2)
#summary(model14)

model15<- lm(app_proc_time_days ~ closeness_centrality+tenure_days, data = applications2)
#summary(model15)

model16<- lm(app_proc_time_days ~ betweenness_centrality+tenure_days, data = applications2)
#summary(model16)

# Create a list of the regression models
model_list <- list(model11, model12, model13, model14, model15, model16)

# Use stargazer to generate a table
stargazer(model_list, type = "html", title = "Regression Models Comparison",
          column.labels = c("Model 11", "Model 12", "Model 13", "Model 14", "Model 15", "Model 16"),
          covariate.labels = c("Degree Centrality + Gender", "Betweenness Centrality + Gender", 
                               "Closeness Centrality + Gender", "Degree Centrality + Tenure Days",
                               "Closeness Centrality + Tenure Days", "Betweenness Centrality + Tenure Days"))

# Workgroup
model17 <- lm(app_proc_time_days ~ betweenness_centrality + workgroup, data = applications2)
summary(model17)

#Gender
model18 <- lm(app_proc_time_days ~ degree_centrality + gender + degree_centrality*gender, data = applications2)
summary(model18)

model19 <- lm(app_proc_time_days ~ betweenness_centrality + gender + betweenness_centrality*gender, data = applications2)
summary(model19)

model20 <- lm(app_proc_time_days ~ betweenness_centrality + gender + closeness_centrality*gender, data = applications2)
summary(model20)



#Gender Interaction

model_degree_interaction <- lm(app_proc_time_days ~ degree_centrality * gender 
                               + race + tenure_days, data = applications2)
model_betweenness_interaction <- lm(app_proc_time_days ~ betweenness_centrality * gender 
                                    + race + tenure_days, data = applications2)
model_closeness_interaction <- lm(app_proc_time_days ~ closeness_centrality * gender 
                                  + race + tenure_days, data = applications2)


#Tenure Interaction

model_degree_interaction2 <- lm(app_proc_time_days ~ degree_centrality *tenure_days+ gender 
                                + race , data = applications2)
model_betweenness_interaction2 <- lm(app_proc_time_days ~ betweenness_centrality *tenure_days+ gender 
                                     + race , data = applications2)
model_closeness_interaction2 <- lm(app_proc_time_days ~ closeness_centrality * tenure_days+gender 
                                   + race, data = applications2)

summary(model_degree_interaction2)
summary(model_betweenness_interaction2)
summary(model_closeness_interaction2)


#Race Interaction

model_degree_interaction1 <- lm(app_proc_time_days ~ degree_centrality * race+ gender 
                                + tenure_days, data = applications2)
model_betweenness_interaction1 <- lm(app_proc_time_days ~ betweenness_centrality * race+gender 
                                     + tenure_days, data = applications2)
model_closeness_interaction1 <- lm(app_proc_time_days ~ closeness_centrality * race+ gender 
                                   + tenure_days, data = applications2)

summary(model_degree_interaction1)
summary(model_betweenness_interaction1)
summary(model_closeness_interaction1)