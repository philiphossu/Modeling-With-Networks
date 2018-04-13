# Problem 1
# Data Source: https://files.oakland.edu/users/grossman/enp/Erdos1.html

library('igraph')
library(zoom)

# 511 x 1 column of objects where each object has first attribute of name, second attribute as a list of collaborators
processFile = function(filepath) {
  # Current Main represents the Erdos1 collaborator
  currentMain <- ""
  df <- data.frame()
  con = file(filepath, "r")
  # Open file, read first main collaborator
  line = readLines(con, n = 1)
  words <- strsplit(line, " ")[[1]]
  temp <- gsub('[0-9:* \"]+', '', words)
  currentMain <- paste(words <- temp[which(temp != '')], collapse = ' ')
  
  while (TRUE) {
    line = readLines(con, n = 1)
    # print(line)
    # If EOF, break
    if (length(line) == 0) {
      break
    }
    # If empty line, read next main collaborator
    if (line == ''){
      line = readLines(con, n = 1)
      words <- strsplit(line, " ")[[1]]
      temp <- gsub('[0-9:* \"]+', '', words)
      currentMain <- paste(words <- temp[which(temp != '')], collapse = ' ')
    } else{
      # Record Erdos2 collaborators and their corresponding main collaborator
      words <- strsplit(line, " ")[[1]]
      temp <- gsub('[0-9:* \"]+', '', words)
      words <- paste(words <- temp[which(temp != '')], collapse = ' ')
      currentDF <- data.frame(currentMain,words)
      df <- rbind(df,currentDF)
    }
  }
  close(con)
  # df column 1 = main collaborators (from), column 2 = secondary collaborators (to)
  return(df)
}
# Result = data frame of all edges in the graph
#Phil's path
result <- processFile("~/Desktop/School/MATH380/Modeling-With-Networks/erdos1data.csv")
#Paolo's path
#result <- processFile("IIT/Junior/Second Semester/MATH380/Modeling-With-Networks/erdos1data.csv")
colnames(result) <- c("Main/From", "Secondary/To")
# Loners = Erdos1 collaborators who had no secondary collaborators
loners <- data.frame(c("RIEGER, GEORG JOHANN", "OBLATH, RICHARD", "FRIED, HANS", "FELDHEIM, ERVIN", "BUSOLINI, DONALD TERENCE", "ANNING, NORMAN H."))
colnames(loners) <- c("Main/From")

# Should have 511 Erdos1 collaborators
length(unique(result$`Main/From`))+length(loners$`Main/From`)
# Check!

uniqueE1People <- unique(result$`Main/From`)

# Now we need to go through our data frame and remove all the edges which are not between Erdos1 authors
a <- 1
indicesToRemove <- c()
for(a in 1:nrow(result)){
  #print(result[a])
  if(!(result[a,2] %in% uniqueE1People)){
    indicesToRemove <- c(indicesToRemove,a)
  }
}

# indicesToRemove contains the edges which we don't care about, Erdos2 authors, removing them here
result <- result[-indicesToRemove,]
# Obtaining all nodes for creation of the iGraph object
nodes <- unique(result$`Main/From`)

# Network is our iGraph object to contain the network of Erdos1 authors
network <- graph_from_data_frame(d=result, vertices=nodes, directed=T) 
network

# Messy plot
plot(network, edge.arrow.size=.1,vertex.size=1,margin=0,vertex.label=NA)
zm() # Allows us to zoom but still not really usable

plot(networkUnDirected, edge.arrow.size=.1,vertex.size=1,margin=0,vertex.label=NA)

# Most important authors with respect to the total degree of each node
totalDegree <- degree(network, v = V(network), mode = c("all"), loops = TRUE, normalized = FALSE)
head(sort(totalDegree, decreasing=TRUE))

hist(totalDegree)

outDegree <- degree(network, v = V(network), mode = c("out"), loops = TRUE, normalized = FALSE)
head(sort(outDegree, decreasing=TRUE))

# Detecting Cliques in the Network
networkUnDirected <- graph_from_data_frame(d=result, vertices=nodes, directed=F) # Making undirected network so this can be tested

cliquesFound <- cliques(networkUnDirected, min = 3, max = NULL) # Found lots of cliques!

biggestCliquesFound <- largest_cliques(networkUnDirected) # List of the biggest cliques in the network

# Visualizing one of the large cliques
clique1 <- biggestCliquesFound[[1]]
cliqueNetwork <- induced.subgraph(graph=networkUnDirected,vids=clique1)
plot(cliqueNetwork, edge.arrow.size=.01,vertex.size=1,margin=0,layout=layout_in_circle)

# Calculating Centrality Measures

# Betweenness Centrality
betweennessValues <- betweenness(network)
sort(betweennessValues)

pal <- colorRampPalette(c("blue","green","yellow","orange","red"))
graphCol <- pal(50)[as.numeric(cut(betweennessValues,breaks=50))]

plot(network, vertex.color=graphCol, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=1,margin=0,col=colorRange(50))
zm()

# Eigenvector Centrality
eigenValues <- eigen_centrality(network)$vector
sort(eigenValues)

pal <- colorRampPalette(c("blue","green","yellow","orange","red"))
graphCol <- pal(50)[as.numeric(cut(eigenValues,breaks=50))]

plot(network, vertex.color=graphCol, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=1,margin=0)
zm()

# Closeness Centrality
# Closeness calculation is not good because of the outliers, remove these and re-calculate
closenessValues <- closeness(network)
sort(closenessValues)

pal <- colorRampPalette(c("blue","red"))
graphCol <- pal(5)[as.numeric(cut(closenessValues,breaks=5))]

plot(network, vertex.color=graphCol, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=1,margin=0,col=colorRange(5))
zm()

# Eccentricity 
eccentricityValues <- eccentricity(network)
sort(eccentricityValues)

pal <- colorRampPalette(c("blue","green","yellow","orange","red"))
graphCol <- pal(500)[as.numeric(cut(eccentricityValues,breaks=500))]

plot(network, vertex.color=graphCol, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=1,margin=0,col=colorRange(500))
zm()

# Problem 2

# Problem 3

# Creating the citation igraph network object
library(readr)
citation_edges <- read_csv("~/Desktop/School/MATH380/Modeling-With-Networks/Citation.csv")
citation_vertices <- read_csv("~/Desktop/School/MATH380/Modeling-With-Networks/CitationVertices.csv")
# citation_vertices <- citation_vertices[,1]
cite_network <- graph_from_data_frame(d=citation_edges, vertices=citation_vertices, directed=T) 
cite_network

# Messy plot
plot(cite_network,vertex.label=NA,vertex.size=4,margin=0,edge.arrow.size=0.2)
zm() # Allows us to zoom but still not really usable

# Finding degree of each node
totalDegree <- degree(cite_network, v = V(cite_network), mode = c("all"), loops = TRUE, normalized = FALSE)
head(sort(totalDegree, decreasing=TRUE))

# Eigenvector Centrality
eigenValues <- eigen_centrality(cite_network)$vector
sort(eigenValues)

pal <- colorRampPalette(c("blue","green","yellow","orange","red"))
graphCol <- pal(50)[as.numeric(cut(eigenValues,breaks=50))]

plot(cite_network, vertex.color=graphCol, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=5,margin=0,col=colorRange(50))
zm()

hist(totalDegree)

# indicesToRemove contains the edges which we don't care about, Erdos2 authors, removing them here
result <- result[-indicesToRemove,]
# Obtaining all nodes for creation of the iGraph object
nodes <- unique(result$`Main/From`)

neighbors(networkUnDirected, "GRAHAM, RONALD LEWIS", mode = c("total"))$name

eigenValues <- eigen_centrality(networkUnDirected)$vector
test <- which(names(eigenValues) %in% neighbors(networkUnDirected, "GRAHAM, RONALD LEWIS", mode = c("total"))$name)
head(sort(eigenValues[test], decreasing=TRUE),10)
mean(eigenValues[test])

test2 <- which(names(eigenValues) %in% neighbors(networkUnDirected, "RODL, VOJTECH", mode = c("total"))$name)
head(sort(eigenValues[test2], decreasing=TRUE),10)
mean(eigenValues[test2])

