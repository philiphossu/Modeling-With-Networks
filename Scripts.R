# Problem 1
# Data Source: https://files.oakland.edu/users/grossman/enp/Erdos1.html

processFile = function(filepath) {
  con = file(filepath, "r")
  while (TRUE) {
    line = readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    # Our code goes here
    print(line)
  }
  close(con)
}

processFile("~/Desktop/School/MATH380/Modeling-With-Networks/erdos1_testdata.csv")

# Problem 2

# Problem 3