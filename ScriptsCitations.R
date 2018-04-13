setwd("IIT/Junior/Second Semester/MATH380/Modeling-With-Networks/")

library('igraph')
library('zoom')

#Reading the citationNetwork.csv file
citation_edges <- read.csv("citationNetwork.csv")

#Getting all the unique vertices
citation_vertices <- data.frame(vertices=unique(citation_edges[,1]))
first_vertex <- data.frame(vertices=as.character(unlist(citation_edges[1,2])))
citation_vertices <- rbind(citation_vertices, first_vertex)

#Making the network
#
citation_network <- graph_from_data_frame(d=citation_edges[,1:2], vertices=citation_vertices, directed=T) 
citation_network


length(V(citation_network)) # Number of vertices in the graph
length(E(citation_network)) # Number of edges in the graph

#Plotting the network 
plot(citation_network, edge.arrow.size=0.3,vertex.size=1,margin=0, vertex.label=NA)
zm()

#Total degree
total_citation_degree <- degree(citation_network, v = V(citation_network), mode = c("all"), loops = TRUE, normalized = FALSE)
head(sort(total_citation_degree, decreasing=TRUE))

hist(total_citation_degree)

# Eigenvector Centrality
eigen_values <- eigen_centrality(citation_network)$vector
head(sort(eigen_values, decreasing=TRUE))

pal <- colorRampPalette(c("blue","green","yellow","orange","red"))
graphCol <- pal(50)[as.numeric(cut(eigen_values,breaks=50))]

plot(citation_network, vertex.color=graphCol, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=5,margin=0)
zm()

#Alpha centrality
citation_alpha <- alpha_centrality(citation_network, nodes = V(citation_network), alpha = 0.5, loops = FALSE)
head(sort(citation_alpha, decreasing=TRUE))

pal <- colorRampPalette(c("blue","green","yellow","orange","red"))
graphCol <- pal(5000)[as.numeric(cut(citation_alpha,breaks=5000))]

named <- subset(citation_alpha, citation_alpha>80)

plot(citation_network, vertex.color=graphCol, edge.arrow.size=1, edge.size=NA,vertex.label=NA,vertex.size=5,margin=0)
text(named, labels=names(named))
zm()

