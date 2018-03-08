# Problem 1
# Data Source: https://files.oakland.edu/users/grossman/enp/Erdos1.html

# 511 x 1 column of objects where each object has first attribute of name, second attribute as a list of collaborators
df <- data.frame()
currentMain <- ""

processFile = function(filepath) {
  con = file(filepath, "r")
  
  line = readLines(con, n = 1)
  words <- strsplit(line, " ")[[1]]
  temp <- gsub('[0-9:*]+', '', words)
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
      temp <- gsub('[0-9:*]+', '', words)
      currentMain <- paste(words <- temp[which(temp != '')], collapse = ' ')
    }
    else{
      words <- strsplit(line, " ")[[1]]
      temp <- gsub('[*]', '', words)
      words <- paste(words <- temp[which(temp != '')], collapse = ' ')
      currentDF <- data.frame(currentMain,words)
      df <- rbind(df,currentDF)
    }
    if (line == "&"){
      break
    }
    #print(currentMain)

    # Removing spaces, year, extra symbols from main lines
  }
  close(con)
}

processFile("~/Desktop/School/MATH380/Modeling-With-Networks/erdos1_testdata.csv")

colnames(df) <- c("Main/From", "Secondary/To")


# Problem 2

# Problem 3