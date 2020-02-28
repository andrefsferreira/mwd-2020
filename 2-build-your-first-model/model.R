# 0 - Pre setup -----------------------------------------------------------
# clean environment
rm(list=ls()); gc()

# load libraries
library(caret)
library(magick)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1 - Load data -----------------------------------------------------------
grades_raw <- read.csv('data/student-mat.csv', header = TRUE, sep = ';')

# 2 - Data processing -----------------------------------------------------
# 2.1 - Create custom labels ----------------------------------------------
grades_raw$final <- factor(ifelse(grades_raw$G3 >= 10,  0, 1), labels = c("pass", "fail"))
grades_raw$G3 <- NULL

# 2.2 - Normalize data ----------------------------------------------------
# The impact is that we end up with smaller standard deviations, which can suppress the 
# effect of outliers

### Approach 1
# create normalize function which takes a vector x of numeric values, and for each value in
# x subtracts the min value in x and divides by the range of values in x. A vector will be returned.
normalize <- function(x){
  return ((x - min(x)) / ( max(x) - min(x)))
}

# normalizing data
cols <- c('age','Medu','Fedu','traveltime','studytime','failures','famrel',
          'freetime','goout','Dalc','Walc','health','absences','G1','G2')

grades_normalized <- grades_raw
grades_normalized[cols] <- lapply(grades_raw[cols], normalize)

### Approach 2
# using caret library functions
preproc <- preProcess(grades_raw, method=c("range"))

grades_normalized <- predict(preproc, grades_raw)

summary(grades_normalized)

# 2.3 - Create dummy variables --------------------------------------------
# in situations where we have categorical variables (factors)
# but need to use them in analytical methods that require numbers
# (for example, K nearest neighbors (KNN), Linear Regression), we need to create dummy variables.
dmy <- dummyVars("~ .", data = grades_normalized[,names(grades_normalized) != 'final'])
grades_dummy <- data.frame(predict(dmy, newdata = grades_normalized[,names(grades_normalized) != 'final']))

# add final column
grades_dummy$final <- grades_raw$final

# 2.4 - Split data (training and test) ------------------------------------
set.seed(2020)
trainIndex <- createDataPartition(grades_dummy$final, 
                                  p=0.7, 
                                  list=FALSE)

# Create training and test set
data_train <- grades_dummy[ trainIndex,]
data_test <- grades_dummy[-trainIndex,]

# Check the dimension of both training and test dataset
dim(grades_dummy)
dim(data_train)
dim(data_test)

# 3 - Train model ---------------------------------------------------------
# image about cross validation
kfolds_pic <- image_read('../0-figures/kfolds.png')
kfolds_pic <- image_scale(kfolds_pic, "700")
print(kfolds_pic)
rm(kfolds_pic)

# Set train_control to 10-fold cross validation
train_control <- trainControl(method = 'cv',            # cross validation
                              number  = 10,              # number of folds
                              verboseIter = TRUE,       # output model training details
                              allowParallel = TRUE)     # parallelize if possible 

# Train the model using whatever model you want
# https://topepo.github.io/caret/train-models-by-tag.html
# https://topepo.github.io/caret/available-models.html
set.seed(2020)
model <- train(final ~ ., 
               data = data_train,
               # method examples -- knn, glm, ridge, rpart, rf, nnet, gbm, xgbTree
               method = "gbm",        
               # tuneGrid is used for grid search -- each model has its own variables
               # tuneGrid = expand.grid(k = seq(1, 30, 1)),
               trControl = train_control)

# View the summary of the model
summary(model)
plot(model)

# 4 - Evaluate model performance ------------------------------------------
# image about cross validation
cm_pic <- image_read('../0-figures/confusion-matrix.png')
cm_pic <- image_scale(cm_pic, "700")
print(cm_pic)
rm(cm_pic)

# prediction using the model classification for data_test
model_probs <- predict(model, 
                       newdata = data_test,
                       type = "raw")  # type = "prob"

# create a confusion matrix table by comparing actual final score for data_test and 
model_cm <- confusionMatrix(table(model_probs, data_test$final), 
                            positive = "pass") 

model_cm
print(model_cm$overall[1])

# 5 - Competition ---------------------------------------------------------

# 20-30 min competition to get best model 


# 6 - Save model ----------------------------------------------------------

# save the model to disk
saveRDS(model, "../3-deploy-your-model/data/final_model_normalized.rds")
saveRDS(model_cm, "../3-deploy-your-model/data/model_cm_normalized.rds")


# down sample could help improving results 
# i.e. number of pass == fail when training the model