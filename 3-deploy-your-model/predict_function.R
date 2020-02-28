predict_function <- function(model, gender, G2, age, fjob, health, goout) {
  
  # recover orignal data set
  grades_raw <- read.csv('data/student-mat.csv', 
                         header = TRUE, 
                         sep = ';')
  # ignore final grade
  grades_raw$G3 <- NULL
  
  # initialize a new dataframe with just 1 row to inherit raw data properties
  grades_to_predict <- grades_raw[1,]
  
  # replace values with most frequent observation per attribute
  for (i in names(grades_raw)) {
    
    if(is.null(levels(grades_raw[,i]))){
      
      grades_to_predict[,i] <- as.numeric(names(sort(table(grades_raw[,i]), decreasing = TRUE))[1])
      
    } else {
      
      grades_to_predict[,i] <- factor(names(sort(table(grades_raw[,i]), decreasing = TRUE))[1],
                                      levels = levels(grades_raw[,i]))
    }
  }
  
  print(paste0('gender:', gender))
  print(paste0('G2:', G2))
  print(paste0('age:', age))
  print(paste0('fjob:', fjob))
  print(paste0('health:', health))
  print(paste0('goout:', goout))
  
  # populate dataframe with data from the UI
  grades_to_predict$sex <- factor(gender, levels(grades_raw$sex))
  grades_to_predict$G2 <- as.numeric(G2)
  grades_to_predict$age <- as.numeric(age)
  grades_to_predict$Fjob <- factor(fjob, levels(grades_raw$Fjob))
  grades_to_predict$health <- as.numeric(health)
  grades_to_predict$goout <- as.numeric(goout)
  
  
  # predict
  model_probs <- predict(model, 
                         newdata = grades_to_predict,
                         type = "prob")  
  print(model_probs)
  return(model_probs)
}  