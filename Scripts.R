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
plot(network, edge.arrow.size=.01,vertex.label=NA,vertex.size=1,margin=0)
zm() # Allows us to zoom but still not really usable

# Most important authors with respect to the total degree of each node
totalDegree <- degree(network, v = V(network), mode = c("all"), loops = TRUE, normalized = FALSE)
head(sort(totalDegree, decreasing=TRUE))

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
colorRange <- colorRampPalette(c("blue","green","yellow","orange","red"))
betweennessValues <- betweenness(network)
sort(betweennessValues)

col <- colorRange(max(betweennessValues)+1)
col <- col[betweennessValues+1]

plot(network, vertex.color=col, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=1,margin=0,col=colorRange(50))
zm()

# Eigenvector Centrality
eigenValues <- eigen_centrality(network)$vector
sort(eigenValues)

col <- colorRange(max(eigenValues)+1)
col <- col[eigenValues+1]

plot(network, vertex.color=col, edge.arrow.size=.001, edge.size=NA,vertex.label=NA,vertex.size=1,margin=0,col=colorRange(50))
zm()

# Problem 2

# Problem 3