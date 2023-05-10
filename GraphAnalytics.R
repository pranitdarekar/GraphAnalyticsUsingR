# PART 2

install.packages("igraph")
library(igraph)

setwd("C:/Masters/gwu/big data")
mydata <- read.table("soc-Epinions1_adj.tsv", header = FALSE, sep = "\t")
mydata <- mydata[, c("V1", "V2")]

mydatamatrix <- as.matrix(mydata)

# get all the rows of 1st and 2nd column as vectors
v1 <- mydatamatrix[, 1]
v2 <- mydatamatrix[, 2]

relations <- data.frame(from=v1, to=v2)
initial_g <- graph.data.frame(relations, directed = TRUE)
# plot(g)


# PART 3

# simplify graph
g <- graph.data.frame(relations, directed = FALSE)
simplified_g <- simplify(g)

# 1. Keep nodes whose degrees are above the graph's average degree
avg_degree <- mean(degree(simplified_g))
simplified_g <- delete.vertices(simplified_g, which(degree(simplified_g) < avg_degree))

# 2. Keep nodes above average centrality
avg_coreness <- mean(coreness(simplified_g))
simplified_g <- delete.vertices(simplified_g, which(coreness(simplified_g) < avg_coreness))


# 3. Find maximum betweenness centrality and maximum closeness centrality
betweenness_centrality <- betweenness(simplified_g)
top_betweenness <- head(sort(betweenness_centrality, decreasing = TRUE), 50)

closeness_centrality <- closeness(simplified_g)
top_closeness <- head(sort(closeness_centrality, decreasing = TRUE), 50)

simplified_g <- induced.subgraph(simplified_g, v = union(names(top_betweenness), names(top_closeness)))
# plot(simplified_g)

# rubric functions

# get vertices of the graph
V(simplified_g)

# get edges of the graph
E(simplified_g)


# get adjacency matrix of the graph
get.adjacency(simplified_g)

# get density of the graph
graph.density(simplified_g)

# get edge density
edge_density(simplified_g)

# get edge density with loops
edge_density(simplified_g, loops = T)

# get the degree of each node in the graph
igraph::degree(simplified_g)

# get betweenness centrality of the graph
igraph::centr_betw(simplified_g)

# get closeness centrality of the graph
igraph::centr_clo(simplified_g)

# get alpha centrality of the graph
igraph::alpha_centrality(simplified_g)

# get the shortest path between two nodes
igraph::shortest.paths(simplified_g)

# get actual shortest paths between two nodes
shortest_path <- shortest_paths(simplified_g, from = 1, to = 5)
shortest_path$vpath[[1]]

# get histogram of the degree of nodes in tehe graph
hist(igraph::degree(simplified_g))

# get edge density
edge_density(simplified_g)

# get edge density with loops
edge_density(simplified_g, loops = T)

# get diameter of the graph
igraph::diameter(simplified_g)

# get max cliques for vertex 1
max_cliques(simplified_g, min = NULL, max = NULL)[[1]]

# find the size of largest cliques in the graph
all_cliques <- cliques(simplified_g)
max_size <- max(sapply(all_cliques, length))
max_size


# PART 4

# determine central nodes
degree_centrality <- degree(simplified_g)
betweenness_centrality <- betweenness(simplified_g)
eigenvectore_centrality <- eigen_centrality(simplified_g)$vector
central_nodes <- which(degree_centrality == max(degree_centrality) & betweenness_centrality == max(betweenness_centrality) & eigenvectore_centrality == max(eigenvectore_centrality))
central_nodes

# determine longest path of the graph
diameter <- get_diameter(simplified_g)
diameter

# determine largest cliques of the graph
all_cliques <- cliques(simplified_g)
largest_size <- max(sapply(all_cliques, length))
largest_cliques <- all_cliques[sapply(all_cliques, length) == largest_size]
largest_cliques

# determine egos of the graph
ego(simplified_g)

# determine the power centrality of the graph
power_centrality(simplified_g)

