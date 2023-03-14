### STEP 2. Open the file in RStudio as a text file to clean up for import + import with `read_csv()`

# Import the file using read_csv() function
library(readr)
my_connections <- read_csv("Connections.csv")



### STEP 3. Get the count of your contacts by their current employer + total count

# Import library
library(dplyr)

# Count of contacts by current employer (excluding NA rows)
contacts_by_employer <- drop_na(my_connections, "Company")
contacts_by_employer_count <- nrow(contacts_by_employer)

# Print the contacts by employer
print(contacts_by_employer_count)


# Total count of contacts
total_count <- nrow(my_connections)

# Print the total count
print(total_count)



### STEP 4. Create nodes and edges dataframes to use with igraph

## Install packages
install.packages("tidyverse")
install.packages("tidygraph")
install.packages("igraph")

library(tidyverse)
library(tidygraph)
library(igraph)

## Pre-process data
# Drop the "Email Address" column
my_connections <- my_connections %>% select(-"Email Address")

# Drop any rows with missing values in the remaining columns
my_connections <- my_connections %>% drop_na()


## Create a tidygraph object from the connections data
my_connections_graph <- my_connections %>%
  as_tbl_graph(nodes = c("First Name", "Last Name", "Company", "Position"),
               edges = c("First Name", "Last Name"),
               node_key = "name")

## Extract the nodes and edges dataframes from the tidygraph object
nodes_df <- my_connections_graph %>%
  activate(nodes) %>%
  as_tibble()

edges_df <- my_connections_graph %>%
  activate(edges) %>%
  as_tibble()

# Create a new edges dataframe with name values in place of indexes
edges_df2 <- edges_df %>%
  mutate(from = nodes_df$name[from],
         to = nodes_df$name[to])



### STEP 5. Plot the resulting network

# Create an igraph object from the nodes and edges dataframes
my_igraph <- graph_from_data_frame(d = edges_df2, vertices = nodes_df)


# Plot the resulting network
plot(my_igraph)


    
