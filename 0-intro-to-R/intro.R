1+2
1+2 #this is a comment and is ignored

### Assigning names 
x <- 1
x

# R quirkiness = and <-
matrix(1, nrow = 2)
matrix(1, nrow <- 2)

rm(nrow)


### Functions
sum(1, 1, 1, 1, 1) #function that sums 
c(1, 2, 3)         #function that concatenates numbers into a vector

### Extension Package
#install.packages()
library(ggplot2)

### Getting Help
help(c)
help(str)

### Creating vectors
# The created object will inherit the most permissive properties 
# when being created
vec <- c(1, 2, 3)
str(vec)

vec_alpha <- c(1, 2, 'mwd')
str(vec_alpha)  

### Data storage
# In R, data is stored in data frames or data tables. 
# The latter is an extension of the former. A data frame is a
# list of columns of equal length. For example, the following
# variable df is a data frame containing three lists n, s, b.
n  <- c(2, 3, 5) 
s  <- c("aa", "bb", "cc") 
b  <- c(TRUE, FALSE, TRUE) 
df <- data.frame(n, s, b, stringsAsFactors = FALSE)  # df is a data frame

df
str(df)

### Why factors are important?
# 1. Know all possible outcomes even if no observation exist
# 2. Being able to sort non numeric data
month.name
str(month.name)

order(month.name)
month.name[order(month.name)]

month_ordered <- factor(month_name, levels = month_name, ordered = T)
month_ordered[order(month_ordered)]

### Build-in Data Frame Examples
mtcars

#Here is the cell value from the first row, second column of mtcars.
mtcars[1, 2] 

#We can also access data introducing column names and row names
mtcars["Mazda RX4", "cyl"] 

#count observations in the dataset
nrow(mtcars)
#count number of variables in the dataset
ncol(mtcars)

#print first rows
head(mtcars) 

#In R you can control the execution flow of your code using if conditions:

# NOTE: 
# 1. Unlike python, R is not indentation dependent 
# 2. Despite not compulsory, curly braces are recommended to improve code readability

x <- TRUE

if (x == TRUE) {
  print('x has the value TRUE')
} else {
  print('x did not have the value TRUE')
}

# You can also use loops to perform repetive tasks 
# (the function paste concatenates all its parameters into a string).

for (i in 1:10) {
  print(paste(sep='','I am printing the number ', i))
}