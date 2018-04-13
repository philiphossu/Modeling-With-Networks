setwd("IIT/Junior/Second Semester/MATH380/Modeling-With-Networks/")

library('igraph')
library('zoom')

#Reading the citationNetwork.csv file
citationEdges <- read.csv("citationNetwork.csv")

#Getting all the unique vertices
citationVertices <- data.frame(vertices=unique(citationEdges[,1]))
firstVertex <- data.frame(vertices=as.character(unlist(citationEdges[1,2])))
citationVertices <- rbind(citationVertices, firstVertex)

#Making the network
citationNetwork <- graph_from_data_frame(d=citationEdges[,1:2], vertices=citationVertices, directed=T) 
citationNetwork

#Plotting the network 
plot(citationNetwork, edge.arrow.size=.01,vertex.size=1,margin=0, vertex.label=NA)
zm()

#Alpha centrality
citationAlpha <- alpha_centrality(citationNetwork, nodes = V(citationNetwork), alpha = 0.9, loops = FALSE, exo = 1)

sort(citationAlpha)

pal <- colorRampPalette(c("blue","green","yellow","orange","red"))
graphCol <- pal(5000)[as.numeric(cut(citationAlpha,breaks=5000))]

plot(citationNetwork, vertex.color=graphCol, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=1,margin=0)
zm()

