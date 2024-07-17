# set working directory and load packages
setwd("./DataSet")


install.packages("igraph")
library(igraph)

########################## T1 (done) ##########################
#(a) import csv files and check the content
nodes <- read.csv("StreetGangNodes.csv")
links <- read.csv("StreetGangLinks.csv")

head(nodes)
head(links)

#(b) convert into igraph object, set links to undirected
net <- graph_from_data_frame(d=links, vertices = nodes, directed = F)

#(c) Examine the nodes, edges, and their attributes
class(net)
net

E(net) #examine edges/links
V(net) #examine vertices/nodes
E(net)$name
edge_attr(net) #examine attributes
vertex_attr(net)

#(d) plot the network using the default settings
plot(net)

#(e) plot the network again with some required settings
V(net)$size <- V(net)$Age/2 #nodes
E(net)$width <- E(net)$weight #links
plot(net)


########################## T2 (done) ##########################
#(a) Explore measures of centrality within the network
#degree
centrality_degree <- degree(net)

#betweenness
centrality_betweenness <- betweenness(net, directed=T, weights=NA)
centrality_betweenness

#closeness
centrality_closeness <- closeness(net, mode="all", weights=NA)
centrality_closeness

#(b) descending order of each centrality measure
#degree
sorted_degree <- sort(centrality_degree, decreasing = TRUE)
top3_degree <- head(sorted_degree, 3)
top3_degree

#betweenness
sorted_betweenness <- sort(centrality_betweenness, decreasing = TRUE)
top3_betweenness <- head(sorted_betweenness, 3)
top3_betweenness

#closeness
sorted_closeness <- sort(centrality_closeness, decreasing = TRUE)
top3_closeness <- head(sorted_closeness, 3)
top3_closeness


########################## T3 (done) ##########################
#(a)simplify the network
#remove nodes with a degree less than 15
net_degree <- delete_vertices(net, degree(net)<=15)
plot(net_degree)

#remove edges whose weight attribute is less than 3
net_simplified <- delete_edges(net_degree, E(net_degree)[weight < 3])
plot(net_simplified)

#plot using layout option of 'layout_nicely'
plot(net_simplified, layout = layout_nicely(net_simplified), main="simplified network by removing some nodes and edges ")


########################## T4 (done) ##########################
#(a) set the color of node based on Ranking 1-5, where 1 is the highest ranking
colrs <- c("red", "orange", "yellow", "lightgreen", "lightblue")
V(net)$color <- colrs[V(net)$Ranking]
plot(net, layout = layout_nicely(net), main="Network with Node Colors Based on Ranking")
legend(x=1.1, y=1.1, c("1 (Highest)", "2", "3", "4", "5 (Lowest)"), 
       pch=21, col=colrs, pt.bg=colrs, pt.cex=3, cex=1, bty="n", ncol=1, title = "Ranking")

#(b) set the color of node based on Birthplace
colrs2 <- c("purple","cyan", "magenta", "brown")
V(net)$color <- colrs2[V(net)$Birthplace]

#simply the network to include only those who have served time in prison
prison_members <-V(net)[V(net)$Prison == 1]
net_prison <- induced_subgraph(net, prison_members)
plot(net_prison, layout = layout_nicely(net_prison), main="Members Who Have Served Time in Prison in Different Birthplaces")
legend(x=1.1, y=1.1, c("1 West Africa", "2 Caribbean", "3 UK", "4 East African"), 
       pch=21, col=colrs2, pt.bg=colrs2, pt.cex=3, cex=1, bty="n", ncol=1, title = "Birthplace")

#(c) delete nodes where a gang member's ranking is less than 3
net_prison_filter <- delete_vertices(net_prison, V(net_prison)$Ranking < 3)
plot(net_prison_filter) 

# create a two-panel plot (1 row, 2 columns)
hs <- hub_score(net_prison_filter)
par(mfrow=c(1,2))

# first panel, 15 times the value of the hub score
node_sizes <- 15 * hs$vector
plot(net_prison_filter, layout = layout_nicely(net_prison_filter), vertex.size = node_sizes,
     main = "Network with Node Sizes 15 Times Proportional to Hub Score")

# second panel
clusters <- cluster_optimal(net_prison_filter)
plot(clusters, net_prison_filter, main = "Communities within the Network")


########################## T5 (done) ##########################
net_ranking1 <- delete_vertices(net, V(net)$Ranking != 1)
plot(net_ranking1, layout = layout_nicely(net_ranking1), vertex.size = 15, vertex.label.cex=1.7, main="Members Whose Ranking is 1 (Highest)")

net_ranking2 <- delete_vertices(net, V(net)$Ranking != 2)
plot(net_ranking2, layout = layout_nicely(net_ranking2), vertex.size = 15, vertex.label.cex=1.7, main="Members Whose Ranking is 2")

net_ranking3 <- delete_vertices(net, V(net)$Ranking != 3)
plot(net_ranking3, layout = layout_nicely(net_ranking3), vertex.size = 15, vertex.label.cex=1.7, main="Members Whose Ranking is 3")


net_ranking4 <- delete_vertices(net, V(net)$Ranking != 4)
plot(net_ranking4, layout = layout_nicely(net_ranking4), vertex.size = 15, vertex.label.cex=1.4, main="Members Whose Ranking is 4")


net_ranking5 <- delete_vertices(net, V(net)$Ranking != 5)
plot(net_ranking5, layout = layout_nicely(net_ranking5), vertex.size = 15, vertex.label.cex=1.7, main="Members Whose Ranking is 5 (Lowest)")


########################## T6 (done) ##########################
#(a) three networks
# remove links with a weight value equal to 1
net_filter <- delete_edges(net, E(net)[weight == 1])

# West African (1) and UK (3), subset using induced_subgraph()
subnet_west <- induced_subgraph(net_filter, V(net_filter)[Birthplace %in% c(1,3)])

colrs2 <- c("purple","cyan", "magenta", "brown")
plot(subnet_west, layout = layout_nicely(subnet_west), main="West African (1) and UK (3) Network")
legend(x=1.5, y=1.1, c("1 West Africa", "2 Caribbean", "3 UK", "4 East African"), 
       pch=21, col=colrs2, pt.bg=colrs2, pt.cex=3, cex=1, bty="n", ncol=1, title = "Birthplace")

# Caribbean (2) and UK (3)
subnet_Caribbean <- induced_subgraph(net_filter, V(net_filter)[Birthplace %in% c(2,3)])
plot(subnet_Caribbean, layout = layout_nicely(subnet_Caribbean), main="Caribbean (2) and UK (3) Network")

# East African (4) and UK (3)
subnet_East <- induced_subgraph(net_filter, V(net_filter)[Birthplace %in% c(3,4)])
plot(subnet_East, layout = layout_nicely(subnet_East), main="East African (4) and UK (3) Network")


#(b) use UK and Caribbean network to calculate authority score
as <- authority_score(subnet_Caribbean, weights=NA)$vector

# Create two plots where the node size 10 times the value of authority score
par(mfrow=c(1,2))
plot(subnet_Caribbean,vertex.size=as*10, main="Network with Authorities")

communities <- cluster_optimal(subnet_Caribbean)
plot(communities, subnet_Caribbean, main="Network Communities")

colrs3 <- c("cyan", "magenta")
legend("bottomleft", c("2 Caribbean", "3 UK"), 
       pch=21, col=colrs3, pt.bg=colrs3, pt.cex=2, cex=1, bty="n", ncol=1, title = "Birthplace")

