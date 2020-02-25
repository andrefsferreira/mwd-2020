# 0 - Pre setup -----------------------------------------------------------
# clean environment
rm(list=ls()); gc()

# load libraries
library(caret)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1 - Load data -----------------------------------------------------------
grades_raw <- read.csv('data/student-mat.csv', header = TRUE, sep = ';')

# 2 - Data processing -----------------------------------------------------

# 2.1 - Create custom labels ----------------------------------------------
# lets consider that we have high standards and only pass if grade higher, or eq, than 15
grades_raw$final <- factor(ifelse(grades_raw$G3 >= 15, 0, 1), labels = c("pass", "fail"))
grades_raw$G3 <- NULL

# ignore G1 to simplify model
grades_raw$G1 <- NULL

# add some randomness to G2 -- THIS SHOULD NOT BE DONE!! 
# this is being done to reduce the correlation between G3 and G2
grades_raw$G2 <- grades_raw$G2*runif(n=length(grades_raw$G2), min=0.5, max=.9999999999)

# 2.4 - Split data (training and test) ------------------------------------
set.seed(2020)
trainIndex <- createDataPartition(grades_raw$final, 
                                  p=0.7, 
                                  list=FALSE)

# Create training and test set
data_train <- grades_raw[ trainIndex,]
data_test <- grades_raw[-trainIndex,]

# Check the dimension of both training and test dataset
dim(grades_raw)
dim(data_train)
dim(data_test)

# 3 - Train model ---------------------------------------------------------
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

# 20-30 min competition to get bes accuracy model


# 6 - Save model ----------------------------------------------------------

# save the model to disk
saveRDS(model, "data/final_model.rds")
saveRDS(model_cm, "data/model_cm.rds")


# downsample could help improving results 
# i.e. number of pass == fail when training the model