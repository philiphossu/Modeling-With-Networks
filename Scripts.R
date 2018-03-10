# Problem 1
# Data Source: https://files.oakland.edu/users/grossman/enp/Erdos1.html

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
      temp <- gsub('[* \"]', '', words)
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
result <- processFile("~/Desktop/School/MATH380/Modeling-With-Networks/erdos1data.csv")
colnames(result) <- c("Main/From", "Secondary/To")
# Loners = Erdos1 collaborators who had no secondary collaborators
loners <- data.frame(c("RIEGER, GEORG JOHANN", "OBLATH, RICHARD", "FRIED, HANS", "FELDHEIM, ERVIN", "BUSOLINI, DONALD TERENCE", "ANNING, NORMAN H."))
colnames(loners) <- c("Main/From")

# Should have 511 Erdos1 collaborators
length(unique(result$`Main/From`))+length(loners$`Main/From`)
# Check!

# Problem 2

# Problem 3