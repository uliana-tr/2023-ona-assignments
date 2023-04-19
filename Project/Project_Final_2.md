## Introduction

The United States Patent and Trademark Office, also known as the USPTO,
is responsible for issuing patents in the U.S. The process of patent
issuance starts with submitting patent applications that appeal for the
evaluation of a particular invention. The application is then assigned
to a patent examiner, who will assess the invention against a set of
criteria. Once the assessment is complete, the examiner releases a
verdict of approval or rejection of the application and grants or denies
the patent to the applicant accordingly. The status of examined
applications is not final; applicants can file for the re-evaluation of
their applications, usually after making necessary adjustments based on
the examiner’s feedback.

Even though a straightforward process, the examination of a patent
application is a lengthy one. Determining the novelty and
inconspicuousness of an application in light of previous inventions is
time-consuming. Considering this on the scale of the entire
organization, patent applications accumulate as they pend processing,
and application review times compound into months or even years. Time is
a crucial element for patent applicants, as the absence of a patent is
an obstacle to monetizing their inventions and increases the risk of
losing ownership to someone else in the market. The USPTO has faced some
legal pressure about the need to address this application backlog
problem, which has driven the organization to investigate the issue’s
root and attempt to solve it.

Previously, the effects of the characteristics of the examiners at USPTO
on the duration of the examination process were studied. The analysis of
the traits of examiners singularly yielded insightful findings about the
possible causes and solutions of the application backlog. Studying
examiners separately, however, is not enough. Humans are social beings;
we have an affinity to interact and exist in communities that impact us
and the events that unfold around us. In the professional context,
collaborative work cultures are heavily encouraged and widely
established. This, thus, raises the question about the implications of
the USPTO’s examiner network on the backlog of applications. This
project attempts to explain the topology’s effect of the examiner
network within the USPTO and the network characteristics of each
examiner on the length of the examination process of patent
applications. After that, the study results are translated into
actionable insights that may be implemented to expedite the processing
of applications across the USPTO.

## Data

Two datasets were used throughout this project: app\_data\_sample and
edges\_sample. App\_data\_sample contains detailed application
information for every application filed to the USPTO since 2000. The
other data set consists of inter-relational examiner information. Every
row in edges\_sample represents a connection between two examiners
within the USPTO, which indicates that these two examiners collaborate
or exchange advice on application processing.

    library(arrow)
    library(dplyr)
    library(igraph)
    library(visNetwork)
    library(readr)
    library(tidyverse)
    library(gender)
    library(wru)
    library(lubridate)
    library(ggplot2)
    library(coefplot)
    library(stargazer)
    library(broom)

    # Load data
    applications <- read_parquet("C:/Users/ulyan/OneDrive - McGill University/Documents/MMA/Winter II 2023/Org Network Analysis/Exercise 3/672_project_data/app_data_sample.parquet")
    edges <- read_csv("C:/Users/ulyan/OneDrive - McGill University/Documents/MMA/Winter II 2023/Org Network Analysis/Exercise 3/672_project_data/edges_sample.csv")

## Preprocessing and Generating Data

The initial app\_sample\_data dataset does not include demographic
information in the examiners. Some interesting demographic features to
study the application processing time were also missing. Our first step
was then to add those features to the dataset. To do so we used the R
libraries gender and wru to get the gender and the race of the
examiners, respectively. Finally, we added the tenure in days of each
examiner and the processing time of the applications. We calculated the
processing time, such as the difference between the patent issue date
and the filling date if the patent was granted or the difference between
the abandoned date and the filling date if the application was
abandoned.

The workgroup the examiners are in can also impact the application
processing time. We extracted that information as well and added it to
the primary dataset.

    applications$workgroup <- substr(applications$examiner_art_unit, 1, 3)

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

This section of the analysis involves computing the processing time for
each patent application in days and weeks. To achieve this, the
as.Date() function is applied to convert the filing date, patent issue
date, and abandon date variables to date format, which makes it possible
to calculate the processing time. The difftime() function is then
employed to compute the difference between the filing date and the
patent issue date or abandon date in days or weeks, depending on whether
the patent was granted or not. Subsequently, the calculated processing
times are appended to the original applications dataset as new variables
named app\_proc\_time\_days and app\_proc\_time\_weeks. These processing
time variables serve as the dependent variables in the linear regression
models aimed at exploring the relationship between centrality measures,
gender, race, and processing time. The computed processing time is a
significant metric to understand the efficiency of the USPTO patent
examination process.

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

To ensure the accuracy and validity of our data, we take extra
precautions to remove any potential outliers. We utilize the filter()
function to remove any rows in the applications dataset where the
processing time is less than 0, indicating a possible error in the data.
Removing these outliers helps to maintain a high level of data integrity
and increases the reliability of our results. By taking this step, we
can confidently analyze the relationship between centrality measures,
gender, race, and processing time without interference from inaccurate
data points.

    applications <- applications %>% 
      filter(app_proc_time_days >= 0)

## Centrality measures

### Defining the Centralities

To better understand the role of the network in the processing time and
overall on this subject, we calculated some network insights. Indeed, we
focused our study on three measures that can be used to determine the
relative importance or influence of individuals within a network: degree
centrality, closeness centrality, and betweenness centrality. The degree
of centrality is a measure of the importance of a node. The degree
represents how many relationships you have in the network. In our case,
a node is an examiner, and the relationship is an exchange of advice
from one examiner to another. The more relationship an examiner has, the
more advice that person has sought or given. Those examiners with a
higher degree centrality may have more opportunities to exchange
information or advice and know with whom they might find the answers to
their questions.

The betweenness centrality reflects the extent to which an examiner lies
on the shortest paths between other pairs of examiners who exchange
advice. Those examiners with a higher betweenness centrality could
frequently serve as intermediaries in advice exchange. Examiners with
low betweenness centrality could be more isolated from the advice
exchange.

Finally, the closeness centrality measures the average lengths of the
shortest paths between an examiner and all other examiners at the USPTO.
This measure can help identify which examiners are well-connected and
may have easier access to information and knowledge from the network.

To obtain these measures, we first created a network of examiners and
their relationships using the “edges” dataset and then generated a new
dataset for the individuals with examiner IDs. We then used R functions
‘degree()’, ‘closeness()’, and ‘betweenness()’ to calculate the
centrality measures. The resulting dataset was merged back with the
primary dataset and filtered for missing values. Our network analysis
provides valuable insights into the importance of social relationships
in patent processing time and helps us understand the impact of
individual examiners’ network positions on the patent approval process.

### Filtering the Data

In order to calculate the centrality measures, we needed to create a
network of examiners and their relationships. This was accomplished by
filtering the “edges” dataset so that only the examiner IDs present in
the “applications” dataset remained. The resulting dataset included
information on the relationships between examiners, and was used to
calculate degree, betweenness, and closeness centrality using R
functions. The resulting measures were then merged with the primary
dataset and any missing values were removed.

    # Edges dataset
    edges_df <- edges
    edges <- edges_df

    # Filtering the data so that only the examiner_id still in the applications dataset stays in the edges dataset
    edges <- edges %>%
      dplyr::filter(ego_examiner_id %in% applications$examiner_id) %>%
      drop_na() %>%
      mutate(from = ego_examiner_id, to = alter_examiner_id) %>%
      select(from, to)

### Creating the Nodes Dataset

In this portion of the analysis, we create a nodes dataset using the
select() function from the dplyr package to extract the unique examiner
IDs from the applications dataset. This nodes dataset is a necessary
component for creating the examiner network used to calculate the
centrality measures.

    # Creating the nodes dataset with unique examiner_id from the applications dataset
    nodes <- applications %>%
      dplyr::select(examiner_id) %>%
      dplyr::distinct() %>%
      dplyr::filter(examiner_id %in% edges$from | examiner_id %in% edges$to)

### Calculating Centrality and Merging with Applications Dataset

In this portion of the analysis, we use the edges dataset to calculate
degree centrality, closeness centrality, and betweenness centrality for
each examiner. The graph\_from\_data\_frame function is used to create a
graph object from the edges dataset, which is then used to calculate the
centrality scores for each examiner using the degree(), closeness(), and
betweenness() functions. These scores are then merged with the original
applications dataset to create a new dataset called applications2 that
includes the centrality measures. Finally, any rows with missing
centrality data are removed to ensure the validity of our results.

    # Using graph to get the centrality scores
    graph <- graph_from_data_frame(edges, directed = TRUE)

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

## Analysis

### Linear Regression Models Estimating the Relationship between Centrality and Processing Time

This project asks three questions: What are the organizational and
social factors associated with the length of patent application
prosecution? What is the role of network structure here? What is the
role of race and ethnicity in the processes described in the questions
above?

To be able to answer them, we chose to use linear regressions to
estimate the relationships between the different factors and the
processing time. To tackle this analysis we used the following
strategy: - First, test simple linear regressions of the processing time
against one or two factors. - Second, test complex linear regressions of
the processing time against multiple factors.

The single linear regressions of the processing time tested different
factors such as centralities, race, gender, and tenure. For the race, as
there are multiple, we used “White” as a reference in the first place
before using “Asian as a reference”. After running a dozen simple linear
regressions, here is what we learned.

    model <- lm(app_proc_time_days ~ degree_centrality, data = applications2)
    #summary(model)

    model3 <- lm(app_proc_time_days ~ betweenness_centrality, data = applications2)
    #summary(model3)

    modell3 <- lm(app_proc_time_days ~ closeness_centrality, data = applications2)
    #summary(modell3)

    # Create a list of the regression models
    model_list_basic <- list(model, model3, modell3)

    # Use stargazer to generate a LaTeX table
    stargazer_output_basic <- stargazer(model_list_basic, type = "latex", title = "Regression Models Comparison",
                                   column.labels = c("Model 1", "Model 2", "Model 3"),
                                   covariate.labels = c("Degree Centrality", "Betweenness Centrality", 
                                                        "Closeness Centrality"), 
                                   column.sep.width = "0.2pt", # add separation between columns
                                   style = "asr", # use American Sociological Review style
                                   font.size = "footnotesize")

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Tue, Apr
18, 2023 - 9:02:59 PM
    # Include the stargazer table in the document
    cat(stargazer_output_basic, sep = "\n")

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Tue, Apr
18, 2023 - 9:03:00 PM
### Regression models with gender and tenure days as additional predictors

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

The table below compares the results of different regression models. The
table provides a detailed summary of the coefficients and statistical
significance of each model’s variables.

    model_list_1 <- list(model11, model12, model13)

    stargazer_output1 <- stargazer(model_list_1, type = "latex", title = "Regression Models Comparison",
                                   column.labels = c("Model 11", "Model 12", "Model 13"),
                                   covariate.labels = c("Degree Centrality + Gender", "Betweenness Centrality + Gender", 
                                   "Closeness Centrality + Gender"),
                                   column.sep.width = "0.2pt", # add separation between columns
                                   style = "asr", # use American Sociological Review style
                                   font.size = "footnotesize")

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Tue, Apr
18, 2023 - 9:03:06 PM
    model_list_2 <- list(model14, model15, model16)

    stargazer_output2 <- stargazer(model_list_2, type = "latex", title = "Regression Models Comparison",
                                   column.labels = c("Model 14", "Model 15", "Model 16"),
                                   covariate.labels = c("Degree Centrality + Tenure Days",
                                   "Closeness Centrality + Tenure Days", "Betweenness Centrality + Tenure Days"), 
                                   column.sep.width = "0.2pt", # add separation between columns
                                   style = "asr", # use American Sociological Review style
                                   font.size = "footnotesize")

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Tue, Apr
18, 2023 - 9:03:08 PM
    cat(stargazer_output1, sep = "\n")

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Tue, Apr
18, 2023 - 9:03:06 PM
    cat(stargazer_output2, sep = "\n")

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Tue, Apr
18, 2023 - 9:03:08 PM
In Table 2, all three models demonstrate that degree centrality,
betweenness centrality, and closeness centrality significantly influence
processing time when accounting for gender differences. A higher degree
centrality and betweenness centrality are associated with increased
processing time, whereas a higher closeness centrality results in a
reduction of processing time. In addition, male examiners are found to
process applications more quickly, as indicated by the positive
coefficients for gender across all models.

Table 3 highlights the regression models accounting for tenure days as
an additional predictor. Similar to Table 2, the results indicate that
degree centrality and betweenness centrality are positively correlated
with processing time, while closeness centrality is negatively
correlated. Furthermore, the negative coefficients for tenure days
across all models suggest that as an examiner’s tenure increases, the
application processing time decreases. This finding underscores the
importance of experience in expediting the processing of patent
applications.

The reason behind the results that these linear regression models
produced can be attributed to the extents of obstruction in the
communication networks within the USPTO. In the light of that context,
increased processing times caused by unit increases in degree centrality
could be explained by the in friction. Application friction refers to
the slowing down of the examination process due to the transfer of the
application between examiners. Hence, a higher degree centrality
possibly means the application spends more time moving from one examiner
to the next, which may explain why examiners with higher degree
centralities take longer to reach a decision on patent applications. As
for the implication of betweenness centrality, the results could be
caused by the fact that an examiner with high betweenness centrality as
the only or one of the few channels of communication between the ends of
a network. This means that two assistant examiners that want to discuss
an application have to go through the assigned examiner due to the
absence of a shorter, direct path between each other. Finally, the two
models may be indicating a significant expedition of the examination
process attributed to the increase in closeness centrality because the
paths of communication in the examiner’s network are, generally,
shorter, meaning that less time is spent in the transfer of ideas or
information.

# Gender Interaction

The following linear regressions are the more complex ones using
multiple factors simultaneously.

1.  

<!-- -->

    model_degree_interaction <- lm(app_proc_time_days ~ degree_centrality * gender + race + tenure_days, data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_degree_interaction_summary <- tidy(model_degree_interaction)

    # Create a regression formula using the model summary data frame
    model_degree_interaction_formula <- paste0("app_proc_time_days = ", 
                                               round(model_degree_interaction_summary$estimate[1], 3), 
                                               " + ", 
                                               round(model_degree_interaction_summary$estimate[2], 3), "*degree_centrality",
                                               " + ", 
                                               round(model_degree_interaction_summary$estimate[3], 3), "*gender", 
                                               " + ", 
                                               round(model_degree_interaction_summary$estimate[4], 3), "*degree_centrality*gender", 
                                               " + ", 
                                               round(model_degree_interaction_summary$estimate[5], 3), "*race", 
                                               " + ", 
                                               round(model_degree_interaction_summary$estimate[6], 3), "*tenure_days")


    # Print the regression formula
    cat(model_degree_interaction_formula)

app\_proc\_time\_days = 1554.033 + 0.564*degree\_centrality +
36.731*gender + 2.98*degree\_centrality*gender + 8.625*race +
4.323*tenure\_days

This model uses degree centrality and interaction between gender and
degree centrality. The results show that the average processing time is
1486.3 days. It shows that degree centrality, gender, and race have a
statistically significant impact on processing time. Specifically, a
one-unit increase in degree centrality is associated with a 0.5637 day
increase in processing time. Being male is associated with a 36.73 day
increase in processing time compared to being female. Similarly, being a
member of each racial category (Asian, Black, Hispanic, and Other) is
associated with a statistically significant increase in processing time
compared to being White. For tenure days, it suggests that a one-day
increase in tenure is associated with a 0.04891 day decrease in
processing time. The interaction term between degree centrality and
gender indicates that the effect of degree centrality is stronger for
women than for men. This means that, for women, having a higher degree
centrality is associated with a larger increase in processing time than
it is for men. In other words, being well-connected may be more
beneficial for men in terms of reducing processing time for their
applications than it is for women.

1.  

<!-- -->

    model_betweenness_interaction <- lm(app_proc_time_days ~ betweenness_centrality * gender 
                                        + race + tenure_days, data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_betweenness_interaction_summary <- tidy(model_betweenness_interaction)

    # Create a regression formula using the model summary data frame
    model_betweenness_interaction_formula <- paste0("app_proc_time_days = ", 
                                                    round(model_betweenness_interaction_summary$estimate[1], 3), 
                                                    " + ", 
                                                    round(model_betweenness_interaction_summary$estimate[2], 3), "*betweenness_centrality",
                                                    " + ", 
                                                    round(model_betweenness_interaction_summary$estimate[3], 3), "*gender", 
                                                    " + ", 
                                                    round(model_betweenness_interaction_summary$estimate[4], 3), "*betweenness_centrality*gender", 
                                                    " + ", 
                                                    round(model_betweenness_interaction_summary$estimate[5], 3), "*race", 
                                                    " + ", 
                                                    round(model_betweenness_interaction_summary$estimate[6], 3), "*tenure_days")

    # Print the regression formula
    cat(model_betweenness_interaction_formula)

app\_proc\_time\_days = 1577.562 + 0.002*betweenness\_centrality +
20.783*gender + 2.591*betweenness\_centrality*gender + 8.842*race +
7.656*tenure\_days

The model is based on betweenness centrality and its interaction with
gender. It suggests that the average processing time is 1,511 days. It
shows that a one-unit increase in betweenness centrality is associated
with a 0.0015 day increase in processing time. Being male is associated
with a 20.78 day increase in processing time compared to being female.
Similarly, being a member of each racial category (Asian, Black,
Hispanic, and Other) is associated with a statistically significant
increase in processing time compared to being White. The coefficient for
tenure days suggests that a one-day increase in tenure is associated
with a 0.05127 day decrease in processing time. The interaction term
between betweenness centrality and gender indicates that for men, a
one-unit increase in betweenness centrality is associated with a
0.002489 day increase in processing time, holding all other variables
constant. The positiveness of the last factor also lead us to say that
the effect of betweenness centrality is stronger on male examiner than
on female examiner.

1.  

<!-- -->

    model_closeness_interaction <- lm(app_proc_time_days ~ closeness_centrality * gender 
                                      + race + tenure_days, data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_closeness_interaction_summary <- tidy(model_closeness_interaction)

    # Create a regression formula using the model summary data frame
    model_closeness_interaction_formula <- paste0("app_proc_time_days = ", 
                                               round(model_closeness_interaction_summary$estimate[1], 3), 
                                               " + ", 
                                               round(model_closeness_interaction_summary$estimate[2], 3), "*closeness_centrality",
                                               " + ", 
                                               round(model_closeness_interaction_summary$estimate[3], 3), "*gender", 
                                               " + ", 
                                               round(model_closeness_interaction_summary$estimate[4], 3), "*closeness_centrality*gender", 
                                               " + ", 
                                               round(model_closeness_interaction_summary$estimate[5], 3), "*race", 
                                               " + ", 
                                               round(model_closeness_interaction_summary$estimate[6], 3), "*tenure_days")

    # Print the regression formula
    cat(model_closeness_interaction_formula)

app\_proc\_time\_days = 1511.711 + -106.748*closeness\_centrality +
31.426*gender + 16.683*closeness\_centrality*gender + 3.906*race +
-14.474*tenure\_days

This model predicts application processing time in days based on all the
variables discussed earlier and closeness centrality. The model also
includes an interaction term between closeness centrality and gender.
The model indicates that the average processing time for an application
is 1,446 days. The negative coefficient for closeness centrality
suggests that as the closeness centrality of the examiner increases, the
processing time decreases. The model indicates that, on average, male
examiners have a longer processing time than female examiners, holding
all other predictor variables constant. The positive coefficients for
raceAsian, raceblack, and raceHispanic suggest that examinersfrom these
racial groups have a longer processing time compared to a white. The
model suggests that as the tenure of the examiners increases, the
processing time decreases. Finally, the negative coefficient for the
interaction term between closeness centrality and gender suggests that
for male examiners, as the closeness centrality increases, the
processing time increases at a slower rate than for female examiners.

### Tenure Interaction Models

Next, linear regression models were fitted to investigate the potential
interaction between degree centrality, betweenness centrality, and
closeness centrality with tenure days on processing time, while
controlling for gender and race.

1.  

<!-- -->

    model_degree_interaction2 <- lm(app_proc_time_days ~ degree_centrality *tenure_days+ gender 
                                    + race , data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_degree_interaction2_summary <- tidy(model_degree_interaction2)

    # Create a regression formula using the model summary data frame
    model_degree_interaction2_formula <- paste0("app_proc_time_days = ", 
                                               round(model_degree_interaction2_summary$estimate[1], 3), 
                                               " + ", 
                                               round(model_degree_interaction2_summary$estimate[2], 3), "*degree_centrality",
                                               " + ", 
                                               round(model_degree_interaction2_summary$estimate[3], 3), "*tenure_days", 
                                               " + ", 
                                               round(model_degree_interaction2_summary$estimate[4], 3), "*degree_centrality*tenure_days", 
                                               " + ", 
                                               round(model_degree_interaction2_summary$estimate[5], 3), "*gender", 
                                               " + ", 
                                               round(model_degree_interaction2_summary$estimate[6], 3), "*race")

    # Print the regression formula
    cat(model_degree_interaction2_formula)

app\_proc\_time\_days = 1584.422 + -0.86*degree\_centrality +
-0.053*tenure\_days + 24.5*degree\_centrality*tenure\_days +
2.008*gender + 5.39*race

This model aims to predict based on degree centrality and its
interaction with Tenure. The model indicates that, on average, the
processing time for job applications is 1,516 days. The negative
coefficient for degree centrality suggests that as the degree centrality
of an examiner increases, the processing time decreases, holding all
other predictor variables constant. The negative coefficient for
tenure\_days indicates that as the tenure of an examiner increases, the
processing time decreases. The positive coefficients for gendermale
(2.450e+01) and raceAsian (6.850e+01), raceblack (7.051e+01), and
raceHispanic (7.389e+01) indicate that, on average, male examiners and
examiners from these racial groups have longer processing times compared
to the reference group (i.e., female examiners and examiners from other
white groups). The positive coefficient for the interaction term between
degree centrality and tenure in days (1.687e-04) suggests that the
relationship between degree centrality and processing time varies by the
examiner’s tenure. Specifically, as the tenure of an examiner increases,
the effect of degree centrality on processing time becomes less
pronounced.\`

1.  

<!-- -->

    model_betweenness_interaction2 <- lm(app_proc_time_days ~ betweenness_centrality *tenure_days+ gender 
                                         + race , data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_betweenness_interaction2_summary <- tidy(model_betweenness_interaction2)

    # Create a regression formula using the model summary data frame
    model_betweenness_interaction2_formula <- paste0("app_proc_time_days = ", 
                                                   round(model_betweenness_interaction2_summary$estimate[1], 3), 
                                                   " + ", 
                                                   round(model_betweenness_interaction2_summary$estimate[2], 3), "*betweenness_centrality",
                                                   " + ", 
                                                   round(model_betweenness_interaction2_summary$estimate[3], 3), "*tenure_days", 
                                                   " + ", 
                                                   round(model_betweenness_interaction2_summary$estimate[4], 3), "*betweenness_centrality*tenure_days", 
                                                   " + ", 
                                                   round(model_betweenness_interaction2_summary$estimate[5], 3), "*gender", 
                                                   " + ", 
                                                   round(model_betweenness_interaction2_summary$estimate[6], 3), "*race")

    # Print the regression formula
    cat(model_betweenness_interaction2_formula)

app\_proc\_time\_days = 1575.151 + 0.008*betweenness\_centrality +
-0.051*tenure\_days + 22.278*betweenness\_centrality*tenure\_days +
3.107*gender + 8.943*race

This model is based on betweenness centrality and interaction terms
between betweenness centrality and tenure. The intercept coefficient
indicates that if all other predictor variables are held constant, the
average processing time for an application is 1,508 days. The positive
coefficient for betweenness centrality suggests that as the betweenness
centrality of the examiner increases, the processing time increases.
Further, as seen in all of the past models, as the tenure of the
examiner increases, the processing time decreases. On average, male
examiners have a longer processing time than female examiners, holding
all other predictor variables constant. Also, for Asians, Blacks, and
Hispanics, the model suggests that examiners from these racial groups
have a longer processing time than a reference group, holding all other
predictor variables constant. Finally, the negative coefficient for the
interaction term between betweenness centrality and tenure in days
suggests that the relationship between betweenness centrality and
processing time varies by tenure; as tenure increases, the effect of
betweenness centrality on processing time decreases.

1.  

<!-- -->

    model_closeness_interaction2 <- lm(app_proc_time_days ~ closeness_centrality * tenure_days+gender 
                                       + race, data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_closeness_interaction2_summary <- tidy(model_closeness_interaction2)

    # Create a regression formula using the model summary data frame
    model_closeness_interaction2_formula <- paste0("app_proc_time_days = ", 
                                                 round(model_closeness_interaction2_summary$estimate[1], 3), 
                                                 " + ", 
                                                 round(model_closeness_interaction2_summary$estimate[2], 3), "*closeness_centrality",
                                                 " + ", 
                                                 round(model_closeness_interaction2_summary$estimate[3], 3), "*tenure_days", 
                                                 " + ", 
                                                 round(model_closeness_interaction2_summary$estimate[4], 3), "*closeness_centrality*tenure_days", 
                                                 " + ", 
                                                 round(model_closeness_interaction2_summary$estimate[5], 3), "*gender", 
                                                 " + ", 
                                                 round(model_closeness_interaction2_summary$estimate[6], 3), "*race")

    # Print the regression formula
    cat(model_closeness_interaction2_formula)

app\_proc\_time\_days = 1425.895 + 296.072*closeness\_centrality +
-0.019*tenure\_days + 25.72*closeness\_centrality*tenure\_days +
21.487*gender + 4.698*race

This model uses closeness centrality and an interaction term between
closeness centrality and tenure. On average, the processing time for an
application is 1,362 days when all other predictor variables are held
constant. The coefficient for closeness centrality indicates that as the
closeness centrality of the examiner increases, the processing time
increases. Further, as the examiner’s tenure increases, the processing
time decreases. According to the model, on average, male examiners have
a longer processing time than female examiners. The positive
coefficients for raceAsian, race black, and race Hispanic indicate that
examiners from these racial groups have a longer processing time than a
white group, holding all other predictor variables constant. The
negative coefficient for the interaction term between closeness
centrality and tenure suggests that the relationship between closeness
centrality and processing time varies by tenure. Specifically, as tenure
increases, the effect of closeness centrality on processing time
decreases.

### Race Interaction Models

We examined the interaction effect of centrality measures and race on
application processing time. Three separate linear regression models
were fitted to the data, including the interaction terms between degree,
betweenness, and closeness centrality measures and race, while
controlling for gender and tenure days.

1.  

<!-- -->

    model_degree_interaction3 <- lm(app_proc_time_days ~ degree_centrality * race+ gender 
                                    + tenure_days, data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_degree_interaction3_summary <- tidy(model_degree_interaction3)

    # Create a regression formula using the model summary data frame
    model_degree_interaction3_formula <- paste0("app_proc_time_days = ", 
                                               round(model_degree_interaction3_summary$estimate[1], 3), 
                                               " + ", 
                                               round(model_degree_interaction3_summary$estimate[2], 3), "*degree_centrality",
                                               " + ", 
                                               round(model_degree_interaction3_summary$estimate[3], 3), "*race", 
                                               " + ", 
                                               round(model_degree_interaction3_summary$estimate[4], 3), "*degree_centrality*race", 
                                               " + ", 
                                               round(model_degree_interaction3_summary$estimate[5], 3), "*gender", 
                                               " + ", 
                                               round(model_degree_interaction3_summary$estimate[6], 3), "*tenure_days")

    # Print the regression formula
    cat(model_degree_interaction3_formula)

app\_proc\_time\_days = 1567.271 + 0.045*degree\_centrality +
-41.541*race + -25.756*degree\_centrality*race + 4.468*gender +
-69.632*tenure\_days

This model predicts using degree centrality along with all the variables
discussed earlier. The model also includes an interaction term between
degree centrality and race. The model shows that the average processing
time for an application is 1,498 days when all other predictor variables
are held constant. The positive coefficient for degree centrality
indicates that as the degree centrality of the examiner increases, the
processing time also increases. The positive coefficients for all races
except for white show that it takes longer to process applications. The
positive coefficient for male gender indicates that, on average, male
examiners have a longer processing time than female examiners. The
interaction term between degree centrality and race is significant for
raceblack and raceHispanic, but not for raceAsian and raceother. The
negative coefficient for degree\_centrality:raceAsian (-5.491e-02)
indicates that as the degree centrality of Asian examiners increases,
the processing time decreases, but the effect is not statistically
significant. The positive coefficient for degree\_centrality:raceblack
(3.260e+00) indicates that the effect of degree centrality on processing
time is stronger for Black examiners compared to the reference group.
The same holds for degree\_centrality:raceHispanic (1.742e+00).

1.  

<!-- -->

    model_betweenness_interaction3 <- lm(app_proc_time_days ~ betweenness_centrality * race+gender 
                                         + tenure_days, data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_betweenness_interaction3_summary <- tidy(model_betweenness_interaction3)

    # Create a regression formula using the model summary data frame
    model_betweenness_interaction3_formula <- paste0("app_proc_time_days = ", 
                                               round(model_betweenness_interaction3_summary$estimate[1], 3), 
                                               " + ", 
                                               round(model_betweenness_interaction3_summary$estimate[2], 3), "*betweenness_centrality",
                                               " + ", 
                                               round(model_betweenness_interaction3_summary$estimate[3], 3), "*race", 
                                               " + ", 
                                               round(model_betweenness_interaction3_summary$estimate[4], 3), "*betweenness_centrality*race", 
                                               " + ", 
                                               round(model_betweenness_interaction3_summary$estimate[5], 3), "*gender", 
                                               " + ", 
                                               round(model_betweenness_interaction3_summary$estimate[6], 3), "*tenure_days")

    # Print the regression formula
    cat(model_betweenness_interaction3_formula)

app\_proc\_time\_days = 1574.276 + 0.005*betweenness\_centrality +
0.879*race + 16.057*betweenness\_centrality*race + 8.54*gender +
-65.328*tenure\_days

This model analyzes the relationship between processing time of
applications and various predictor variables. The predictor variables
include betweenness centrality, race, gender, and tenure days. The model
shows that, on average, the processing time for an application is 1,509
days. The coefficient for betweenness centrality is positive, indicating
that as the betweenness centrality of an examiner increases, the
processing time also increases. The positive coefficients for all races,
except for raceother, indicate that processing applications for those
races takes longer. The positive coefficient for gendermale indicates
that, on average, male examiners have a longer processing time than
female examiners. The interaction term between betweenness centrality
and race is significant for raceblack and raceHispanic, but not for
raceAsian and raceother. The negative coefficient for
betweenness\_centrality:raceAsian indicates that as the betweenness
centrality of Asian examiners increases, the processing time decreases,
but the effect is not statistically significant. The positive
coefficients for betweenness\_centrality:raceblack and
betweenness\_centrality:raceHispanic indicate that the effect of
betweenness centrality on processing time is stronger for Black and
Hispanic examiners than the reference group.

1.  

<!-- -->

    model_closeness_interaction3 <- lm(app_proc_time_days ~ closeness_centrality * race+ gender 
                                       + tenure_days, data = applications2)

    # Obtain the model summary and convert it to a data frame
    model_closeness_interaction3_summary <- tidy(model_closeness_interaction3)

    # Create a regression formula using the model summary data frame
    model_closeness_interaction3_formula <- paste0("app_proc_time_days = ", 
                                               round(model_closeness_interaction3_summary$estimate[1], 3), 
                                               " + ", 
                                               round(model_closeness_interaction3_summary$estimate[2], 3), "*closeness_centrality",
                                               " + ", 
                                               round(model_closeness_interaction3_summary$estimate[3], 3), "*race", 
                                               " + ", 
                                               round(model_closeness_interaction3_summary$estimate[4], 3), "*closeness_centrality*race", 
                                               " + ", 
                                               round(model_closeness_interaction3_summary$estimate[5], 3), "*gender", 
                                               " + ", 
                                               round(model_closeness_interaction3_summary$estimate[6], 3), "*tenure_days")

    # Print the regression formula
    cat(model_closeness_interaction3_formula)

app\_proc\_time\_days = 1536.694 + -191.829*closeness\_centrality +
-11.04*race + -95.444*closeness\_centrality*race + -32.693*gender +
-91.381*tenure\_days

This model uses closeness\_centrality along with its interaction with
race. The model indicates that, on average, an application takes 1,445
days to process, as indicated by the intercept coefficient. Further, as
the closeness centrality of an examiner increases, the processing time
decreases. However, this coefficient is statistically significant at the
0.001 level, indicating that it strongly impacts processing time. The
coefficients for race indicate that Black, Asian, and other race
examiners have a longer processing time than the reference group
(White). The coefficient for gender indicates that, on average, male
examiners have a longer processing time than female examiners, and this
effect is statistically significant at the 0.001 level. The coefficient
for the interaction term between closeness centrality and race is
negative for raceAsian, indicating that as the closeness centrality of
Asian examiners increases, the processing time decreases. However, this
effect is not statistically significant. For raceblack and raceHispanic,
the coefficient for the interaction term is positive, indicating that
the effect of closeness centrality on processing time is stronger for
these groups compared to the reference group.

## Results

The analysis examines the relationship between several factors and the
processing time of job applications at the United States Patent and
Trademark Office. The factors examined include degree centrality,
betweenness centrality, and closeness centrality, which measures how
well-connected an examiner is within the USPTO’s network as well as
their race, tenure, and gender.

The first part of the analysis looks at the correlations between each of
these factors and the processing time of job applications. The results
suggest that degree centrality and betweenness centrality are positively
correlated with processing time, meaning that more well-connected
examiners within the USPTO network tend to have longer processing times.
In other words, an examiner that seeks or gives a lot of advice takes
more time to process an application. On the other hand, closeness
centrality is negatively correlated with processing time, indicating
that examiners closer to other examiners in the network tend to have
shorter processing times.

Additionally, the models suggest that the longer an examiner has worked
at the USPTO, the faster they tend to process applications. Finally, the
regression models show that examiner race significantly affects
processing time, with “White” examiners processing applications faster
than other racial groups. We can interpret the results as the more
experienced an examiner is, the faster he or she will process the
application. The race indicator can be biased, as we have seen in a
precedent study that a majority of the examiners at the USPTO are white.
Indeed, it can bias the results but also mean that maybe people in the
organization are majorly white, leading them to be more experienced.

Finally, gender also plays a significant role in the application
processing time. Indeed, our studies showed that male examiners are
taking more time to process the application.

One question is left hanging, what can the USPTO do to reduce the
overall processing time? An intelligent move could be to use the
principle of contagion and complex contagion. Contagion is the concept
that an idea or behavior can be spread through a network from one person
to another. In our case, this could be explained by an examiner copying
a behavior or being influenced by another examiner. Complex contagion is
the same principle but with a threshold: someone might need only one
person with good behavior to adopt it, while someone else could need two
or three people in his close circle to make it one of his behavior. The
USPTO could select a group of faster examiners and add some slower
examiners to that group, hoping that the slower ones would pick up the
pace influenced by the selected groups. With what we have learned, it
could be rearranging workgroups to have more women than men or putting
“Asian” or “Hispanic” examiners with more “White” or “Black” examiners.
Furthermore, putting new examiners with more experienced ones could help
the new members to perform faster.

USPTO is advised to create a structure of interdisciplinary groups the
members of which have the appropriate expertise to tackle most of the
applications assigned to them. This mitigates the need for communication
across work groups and art units, which would decrease the friction
throughout the evaluation process and expedite decision-making.
Furthermore, the organization could look to adopt cloud infrastructure
technologies that would allow it to establish a centralized repository
for its data. This would eliminate the data silos that may exist within
and between work groups, thus immensely reducing the informational
dependencies experienced throughout the examination process and the idle
time associated with them.

Overall, the analysis provides valuable insights into the factors that
affect the processing time of job applications at the USPTO.

## Conclusion

In conclusion, the analysis highlights several key factors that
influence the processing time of job applications at the USPTO. These
factors include the examiners’ degree centrality, betweenness
centrality, closeness centrality, tenure, race, and gender. The findings
suggest that more well-connected examiners tend to have longer
processing times, while examiners with shorter distances to other
examiners in the network tend to process applications faster.
Experienced examiners process applications faster than new ones, and
white examiners have shorter processing times than other racial groups.
Finally, male examiners take more time to process applications than
female examiners.

The study also proposes a potential solution to reduce processing time
through the principle of contagion and complex contagion. The USPTO
could select a group of faster examiners and add some slower examiners
to that group, hoping that the slower ones would pick up the pace
influenced by the selected group. The rearrangement of workgroups to
have more diversity could also help.

Overall, these insights can be used to optimize the USPTO’s patent
application processing time and increase efficiency.
