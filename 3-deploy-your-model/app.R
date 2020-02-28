# 0 - Pre setup -----------------------------------------------------------
# load libraries
library(shiny)
library(shinyalert)
library(shinydashboard)
library(DT)
library(caret)
library(tidyr)
library(dplyr)

# source functions
source('draw_confusion_matrix.R')
source('predict_function.R')

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load model
model <- readRDS('data/final_model.rds')
model_cm <- readRDS('data/model_cm.rds')
descriptors <- read.csv('data/attribute_description.csv', sep = ';')


# 1 - UI ------------------------------------------------------------------
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = 'Mind Your Data 2020'),
  dashboardSidebar(
    selectInput("select_gender", 
                label =  ("Gender"), 
                choices = list("Female" = 'F', "Male" = 'M'), 
                selected = 'F'),
    sliderInput("select_G2", 
                ("Grade 2nd Semester"), 0, 20, 0),
    sliderInput("select_age", 
                ("Age"), 15, 22, 15),
    radioButtons("select_fjob", 
                 label = ("Father Job"),
                 choices = list("teacher" = "teacher",
                                "health care related" = "health", 
                                "civil services" = "services", 
                                "at home" = "at_home",
                                "other"  = "other"), 
                 selected = "teacher"),
    radioButtons("select_health", 
                 ("Health Status"), 
                 choices = list("very bad" = 1, 
                                "bad" = 2, 
                                "neutral" = 3,
                                "good" = 4,
                                "very good" = 5)),
    radioButtons("select_goout", 
                 ("Going Out"), 
                 choices = list("very low" = 1, 
                                "low" = 2, 
                                "neutral" = 3,
                                "high" = 4,
                                "very high" = 5)),
    actionButton("predict", "Predict!")
  ),
  dashboardBody(
    useShinyalert(),
    fluidRow(
      box(width = 12,
          height = 650,
          solidHeader = TRUE,
          status = "primary",
          title = "Data Set and Model Info",
          box(width = 5,
              title = 'Model Details',
              valueBox(width = 6, color = 'green',
                       paste(round(model_cm$overall[1]*100, 1), '%'), 
                       "Accuracy", icon = icon("bullseye")),
              valueBox(width = 6, color = 'yellow',
                       nrow(model$trainingData), 
                       "Training Data Observations", icon = icon("hashtag")),
              plotOutput('plot1')),
          box(width = 7,
              collapsed = FALSE,
              title = 'Data Set Attribute Description',
              dataTableOutput('table1'))
      )
    )
  )
)

# 2 - Server --------------------------------------------------------------
server <- function(input, output, session) {
  
  # Attribue table
  output$table1 <- renderDataTable(descriptors, rownames= FALSE)
  
  # Confusion matrix
  output$plot1 <- renderPlot(draw_confusion_matrix(model_cm))
  
  # Prediction
  observeEvent(input$predict,
               
               if (input$predict){
                 result <- predict_function(model,
                                            input$select_gender,
                                            input$select_G2,
                                            input$select_age,
                                            input$select_fjob,
                                            input$select_health,
                                            input$select_goout)
                 if (result$pass >= 0.5){
                   shinyalert("Congrats! You graduated!!! 🥳", 
                              paste0("Probabilities -- success: ",
                                     round(result$pass*100,1),"%; fail: ",
                                     round(result$fail*100,1),"%"), 
                              type = "success")
                 } else {
                   shinyalert("Sorry...try again next year ☹️", 
                              paste0("Probabilities -- success: ",
                                     round(result$pass*100,1),"%; fail: ",
                                     round(result$fail*100,1),"%"), 
                              type = "error")
                 }
               },
               ignoreInit = TRUE)
}

shinyApp(ui, server)