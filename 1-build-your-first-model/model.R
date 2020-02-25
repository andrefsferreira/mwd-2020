# 0 - Pre setup -----------------------------------------------------------
# clean environment
rm(list=ls()); gc()

# load libraries
library(dplyr)
library(caret)
library(ROCR)

# 1 - Load data -----------------------------------------------------------
grades_raw <- read.csv('0-intro-to-R/data/student-mat.csv', header = TRUE, sep = ';')


# 2 - Data processing -----------------------------------------------------

# 2.1 - Create custom labels ----------------------------------------------
grades_raw$final <- factor(ifelse(grades_raw$G3 >= 10, 1, 0), labels = c("fail", "pass"))

# 2.2 - Normalize data ----------------------------------------------------
# The impact is that we end up with smaller standard deviations, which can suppress the 
# effect of outliers

### Approach 1
# create normalize function which takes a vector x of numeric values, and for each value in 
# x subtracts the min value in x and divides by the range of values in x. A vector will be returned.
normalize <- function(x){
  return ( (x- min(x)) / ( max(x) - min(x)))
}

# normalizing data
cols <- c('age','Medu','Fedu','traveltime','studytime','failures','famrel',
          'freetime','goout','Dalc','Walc','health','absences','G1','G2')

grades_normalized <- grades_raw
grades_normalized[cols] <- lapply(grades_raw[cols], normalize)

### Approach 2
# using caret library functions
preproc <- preProcess(grades_raw[,names(grades_raw) != 'G3'], method=c("range"))

grades_normalized <- predict(preproc, grades_raw[,names(grades_raw) != 'G3'])

summary(grades_normalized)


# 2.3 - Create dummy variables --------------------------------------------
dmy <- dummyVars("~.", data = grades_mat)

# 2.4 - Split data (training and test) ------------------------------------
set.seed(2020)
trainIndex <- createDataPartition(grades_normalized$final, 
                                  p=0.7, 
                                  list=FALSE)

# Create training and test set
data_train <- grades_normalized[ trainIndex,]
data_test <- grades_normalized[-trainIndex,]

# Check the dimension of both training and test dataset
dim(grades_normalized)
dim(data_train)


# 3 - Train model ---------------------------------------------------------
set.seed(2020)

# Set train_control to 10-fold cross validation
train_control <- trainControl(method = 'cv',        # cross validation
                              number  = 10,         # number of folds
                              verboseIter = TRUE,   # output model training details
                              allowParallel = TRUE) # parallelize if possible 

grid_gbm <- expand.grid(
  .shrinkage         = c(0.01, 0.05)
  , .interaction.depth = c(1)
  , .n.minobsinnode    = c(50)
  , .n.trees           = c(200, 400, 600, 1000, 1500, 2000, 3000, 5000, 10000)
)


# Train the model using glm 
model <- train(final ~ ., 
               data = data_train, 
               method = "gbm",        # rpart. rf, gbm, xgbTree
               tuneGrid  = grid_gbm,
               trControl = train_control)

# View the summary of the model
summary(model)
varImp(model) # only work for some models
plot(model)

# 4 - Evaluate model performance ------------------------------------------
# Do prediction using the model
model_probs <- predict(model, newdata = data_test, type = "raw")  

#create a confusion matrix table
confusionMatrix(table(model_probs, data_test$final), positive = "pass") 


model <- train(final ~ ., 
               method = "gbm", 
               data = data_train)

glm.probs <- predict(model, 
                     newdata = data_test, 
                     type="response") 

pr <- prediction(glm.probs, data_test$final)
prf <- performance(pr, 
                   measure = "tpr",
                   x.measure = "fpr")
plot(prf)
