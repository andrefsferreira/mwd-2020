grades_mat <- read.csv('0-intro-to-R/data/student-mat.csv', header = TRUE, sep = ';')
grades_por <- read.csv('0-intro-to-R/data/student-por.csv', header = TRUE, sep = ';')

# Print the first 6 rows of data
head(grades_mat)

# Print the internal structure of the data
str(grades_mat)

# Get the summary of data
summary(grades_mat)

# Check if there is any missing value
sapply(grades_mat, function(x) sum(is.na(x)))
