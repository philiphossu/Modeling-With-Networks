# Problem 1
# Data Source: https://files.oakland.edu/users/grossman/enp/Erdos1.html

# 511 x 1 column of objects where each object has first attribute of name, second attribute as a list of collaborators

processFile = function(filepath) {
  currentMain <- ""
  df <- data.frame()
  con = file(filepath, "r")
  
  line = readLines(con, n = 1)
  words <- strsplit(line, " ")[[1]]
  temp <- gsub('[0-9:* \"]+', '', words)
  currentMain <- paste(words <- temp[which(temp != '')], collapse = ' ')
  
  while (TRUE) {
    line = readLines(con, n = 1)
    print(line)
    
    if (length(line) == 0) {
      break
    }
    if (line == ''){
      line = readLines(con, n = 1)
      words <- strsplit(line, " ")[[1]]
      temp <- gsub('[0-9:* \"]+', '', words)
      currentMain <- paste(words <- temp[which(temp != '')], collapse = ' ')
    } else{
      words <- strsplit(line, " ")[[1]]
      temp <- gsub('[* \"]', '', words)
      words <- paste(words <- temp[which(temp != '')], collapse = ' ')
      currentDF <- data.frame(currentMain,words)
      df <- rbind(df,currentDF)
    }
    #print(currentMain)
  }
  close(con)
  return(df)
}

result <- processFile("~/Desktop/School/MATH380/Modeling-With-Networks/erdos1_testdata.csv")

colnames(result) <- c("Main/From", "Secondary/To")


# Problem 2

# Problem 3