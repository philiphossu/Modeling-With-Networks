setwd("IIT/Junior/Second Semester/MATH380/Modeling-With-Networks/")

#Reading the movie_metadata.csv file
movieData <- read.csv("movie_metadata.csv")

movieData <- read.csv("~/Desktop/School/Math380/Modeling-With-Networks/movie_metadata.csv")

#Removing duplicate rows
movieData <- movieData[!duplicated(movieData),]

actorAList <- c()
actorBList <- c()

#For every row (movie) in the data
for (i in 1:nrow(movieData)){
  actors <- movieData[i,]
  #For every name (actor) in the row
  for (j in 1:ncol(movieData[i,])){
    #For every other name than j in the column 
    for (k in 1:ncol(movieData[i,])){
      if(as.character(unlist(movieData[i,j])) != as.character(unlist(movieData[i,k]))){
        #tempDF <- data.frame(actorA=as.character(unlist(movieData[i,j])),actorB=as.character(unlist(movieData[i,k])))
        #actorCollabDF <- rbind(actorCollabDF, tempDF)  
        actorAList <- c(actorAList, as.character(unlist(movieData[i,j])))
        actorBList <- c(actorBList, as.character(unlist(movieData[i,k])))
     }
    }
  }
}

#Creating the actorA - actorB dataframe
actorCollabDF <- data.frame(actorA=actorAList, actorB=actorBList)

#Igraph
#Getting the nodes
actorsNodes <- unique(actorCollabDF$actorA)

#igraph?
actorsNetwork <- graph_from_data_frame(d=actorCollabDF, vertices=actorsNodes, directed=T) 
actorsNetwork

#graph
plot(actorsNetwork, edge.arrow.size=.01, vertex.label=NA, vertex.size=1, margin=0)
zm() # Allows us to zoom but still not really usable
