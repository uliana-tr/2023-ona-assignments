### 1. Create a dataset where edges are based on seat adjacency

# Load the igraph package
library(igraph)

# Create the nodes
seat_nodes <- c(1:6, LETTERS[1:4])

# Create an empty graph with the seat nodes
seat_adjacency_graph <- graph.empty(n = length(seat_nodes), directed = FALSE)
V(seat_adjacency_graph)$name <- seat_nodes

# Create the adjacency list
adj_list <- list(
  "1" = c("2"),
  "2" = c("1", "A"),
  "3" = c("4", "D", "C"),
  "4" = c("3", "C"),
  "5" = c("D", "6"),
  "6" = c("5", "D", "B"),
  "A" = c("B", "2"),
  "B" = c("6", "D", "C", "A"),
  "C" = c("3", "B"),
  "D" = c("3", "5", "6", "B")
)

# Add edges based on adjacency rules
for (seat in names(adj_list)) {
  for (neighbor in adj_list[[seat]]) {
    seat_adjacency_graph <- add.edges(seat_adjacency_graph, c(seat, neighbor))
  }
}

# Plot the seat adjacency graph
plot(seat_adjacency_graph, vertex.size = 30, vertex.label.cex = 1.2)



### 2. For each seat choice (A-D), assuming the other open seats are filled, calculate:
### Degree centrality, Closeness centrality, Betweenness centrality

# Define the seat choices
seat_choices <- LETTERS[1:4]

# Calculate centralities for the seat choices
degree_centralities <- degree(seat_adjacency_graph, v = seat_choices)
closeness_centralities <- closeness(seat_adjacency_graph, v = seat_choices)
betweenness_centralities <- betweenness(seat_adjacency_graph, v = seat_choices)

# Combine the centralities into a data frame
centralities <- data.frame(
  Seat = seat_choices,
  Degree = degree_centralities / 2,  # divide degree centralities by 2
  Closeness = closeness_centralities,
  Betweenness = betweenness_centralities
)

# Print the centralities data frame
print(centralities)

# Note: Degree centrality measures the number of connections a node has in a graph, counting both 
# incoming and outgoing connections. In an undirected graph like the "Fakebook bus", each seat has 
# connections to its adjacent seats, which results in degree centrality values that are twice the actual 
# number of adjacent seats. To correct for this, we divide the degree centrality values by 2.



### 4. Plot the network graph with labels and centrality values

# Generate a color palette with 10 colors
color_palette <- colorRampPalette(c("#B8E2FF", "#004D99"))(10)


# Add centrality values to the graph vertices
V(seat_adjacency_graph)$degree <- degree(seat_adjacency_graph)
V(seat_adjacency_graph)$closeness <- closeness(seat_adjacency_graph)
V(seat_adjacency_graph)$betweenness <- betweenness(seat_adjacency_graph)

# Customize vertex labels with centrality values
vertex_labels <- paste(V(seat_adjacency_graph)$name,
                       "\nDegree: ", V(seat_adjacency_graph)$degree,
                       "\nCloseness: ", round(V(seat_adjacency_graph)$closeness, 4),
                       "\nBetweenness: ", round(V(seat_adjacency_graph)$betweenness, 2))

# Normalize betweenness centrality values
scaled_betweenness <- scale(V(seat_adjacency_graph)$betweenness)

# Assign colors based on normalized betweenness centrality
V(seat_adjacency_graph)$color <- color_palette[rank(-scaled_betweenness)][V(seat_adjacency_graph)]

par(mar = c(5, 5, 1, 1)) # set plot margins

plot(seat_adjacency_graph, vertex.size = 30, vertex.label = vertex_labels, 
     vertex.label.cex = 1, vertex.label.color = "black", vertex.label.dist = 0.5,
     edge.arrow.size = 0.2, edge.curved = 0.3, layout = layout_with_kk,
     main = "Seat Adjacency Network Graph with Centrality Values")

legend("bottomleft", legend = c("Low centrality", "High centrality"), 
       fill = c(color_palette[1], color_palette[10]),
       x.intersp = 0.5, y.intersp = 0.5, inset = 0.01)
