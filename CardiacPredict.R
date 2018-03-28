library(caret)
library(rpart)
library(rpart.plot)
library(shiny)
library(scales)
library(shinythemes)
library(shinyjs)
library(e1071)
library(plotly)

#==============================================================================
#
# # User interface
#
#==============================================================================

ui <- fluidPage(theme = shinytheme("paper"),
  useShinyjs(),
  
  titlePanel(title=div(img(src = "logo.png", height = 50, width = 50), "Heart disease predicton"), windowTitle = "Heart disease prediction"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("pID", "Patient personal number", value = "", width = NULL, placeholder = "9 digits"),
      actionButton("patientFetchButton", "Fetch patient data"),
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
      #br(),
      br(),
      br(),
      radioButtons("sex", "Sex", c("Man" = "1", "Woman" = "0"), selected = character(0)),
      radioButtons("restecg", "Resting ECG", c("Normal" = "0", "ST-T wave abnormality" = "1",
                     "Probable or definite left ventricular hypertrophy by Estes' criteria" = "2"), selected = character(0)),
      radioButtons("cp", "Chest pain type", c("Typical angina pectoris" = "1", "Atypical angina" = "1",
                                              "Non-anginal pain" = "1", "Asymptomatic" = "0"), selected = character(0)),
      numericInput("trestbps", "Resting blood pressure level (mm/Hg)", "", min = 60, max = 200),
      numericInput("age", "Age (years)", "", min = 7, max = 98),
      br(),
      actionButton("predButton", "Predict")
      ),
  
  mainPanel(strong(em("We act before you heart attack. ™")),
  tabsetPanel(
    
    tabPanel("Results",
            br(),
            "This application is a clinical decision support tool that aims to assist you, a cardiologist, 
            in making informed (treatment) decisions by predicting the probability of the presence of heart disease for a particular patient.",
            br(),
            br(),
            "Fetch your patient's data by entering his/her personal number or enter your patient's data manually in the side panel on the left
            to generate a heart disease probability prediction.",
            br(),
            br(),
            "After clicking 'Predict', a barplot will show the probability of the presence of heart disease
            for your patient below.",
            br(),
            br(),
            strong("Heart disease prediction:"),
            br(),
            plotlyOutput("predPlot"),             
            textOutput("reccText", container = div, inline = FALSE)),
    
    tabPanel("Definitions",
            br(),
            "The following attributes refer to demographics, presenting symptoms and results of
            basic lab tests and EKG of a patient.",
            br(),
            br(),
            strong("Sex:"),
            br(),
            "Man or woman.",
            br(),
            strong("Resting ECG:"),
            br(),
            "Normal, ST-T wave abnormality or probable or definite left ventricular hyperthrophy by Estes' criteria.",
            "The ST segments and T waves are parts of the signal on an EKG test of the heart electrical system. 
            These segments are what is affected in states of impaired blood and/or oxygen flow to the heart, such as a heart attack
            or a chronically narrowed artery. 
            May occur in both chronic and acute heart disease states.",
            br(),
            strong("Chest pain type:"),
            br(),
            "Typical angina pectoris, atypical angina, non-anginal pain or asymptomatic.",
            "Typical angina pectoris is defined as a type of pain suggesting it comes from the heart. 
            Usually angina is classified by location, quality (how does it feel), pattern (when does it occur) and duration (how long does it last).
            Atypical angina is defined as having the correct location for angina, but with an incorrect pattern.
            Non-anginal pain is defined as an incorrect location; or in case of a correct location, an incorrect pattern
            and unusual duration of pain (how long it lasts) of <10 seconds or >30 minutes.
            Asymptomatic is defined as the patient not experiencing any pain-like symptoms.",
            br(),
            strong("Resting blood pressure:"),
            br(),
            "Defined as the systolic blood pressure in mm/Hg upon admission to the hospital.",
            br(),
            strong("Age:"),
            br(),
            "As expressed in years.",
            br(),
            br(),
            em("© Nathan Brown")),
    
    tabPanel("Background", 
             tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                         src="HIPI-DataMiningProject2017.pdf")),
    
    tabPanel("About", 
            br(),
            strong("Developed and written by:"), 
            br(),
            "Loes Crielaard",
            br(),
            "Clàudia Figueras Julián",
            br(),
            "Nikola Mastilovic",
            br(),
            "Per Nyberg",
            br(),
            "Erik Scalfaro",
            br(),
            "Maria Tarla",
            br(),
            br(),
            img(src = "KI-logo.png", height = 74, width = 180),
            br(),
            br(),
            em("Health Informatics, 2017"),
            br(),
            br(),
            h6("Disclaimer: This tool for predicting heart disease should under 
            no circumstances be used in clinical practice and 
            the authors make no guarantee of the validity of the predictions or recommendations provided."))
  )
  )
)
)

#==============================================================================
#
# # Server logic
# # Contains:
# # - Accessing, retrieving and populating form with patient data
# # - Retrieving dataset
# # - Training a model using a decision tree algorithm
# # - Predicting presence of heart disease using aforementioned model
#
#==============================================================================

server <- function(input, output, session) {
  
  fetchPatient <- function(patientID){
    patients <- read.csv("patient_dataset.csv", header = TRUE)
    patient <- patients[which(patients$patientID == patientID), ]
    return(patient)
  }
  
  inputRangesCorrect <- function(){
    # Checking if fields are populated, otherwise a logical operation may cause server to crash below
    if(is.na(input$trestbps) || is.na(input$age)){
      return(FALSE)
    }
    # Checking if input adheres to predefined ranges
    if((input$trestbps > 59 & input$trestbps < 201) &
       (input$age > 6 & input$age < 99)){
      return(TRUE)
      # Input did not adhere to ranges, return false
    }else{
      return(FALSE) 
    }
  }
  
  # Observing user interaction with trestbps input field
  # Displaying error graphics if outside boundary
  observeEvent(input$trestbps, {
    if(!is.na(input$trestbps))
      if(input$trestbps < 60 || input$trestbps > 200){
        shinyjs::addClass("trestbps", "error")
      }else{
        shinyjs::removeClass("trestbps", "error")
      }
  })
  
  # Observing user interaction with age input field
  # Displaying error graphics if outside boundary
  observeEvent(input$age, {
    if(!is.na(input$age))
      if(input$age < 7 || input$age > 98){
        shinyjs::addClass("age", "error")
      }else{
        shinyjs::removeClass("age", "error")
      }
  })
  
  # Listening for user clicks on the patientFetchButton
  observeEvent(input$patientFetchButton, {
    # Fetching patient details using the ID that the user entered
    patient = fetchPatient(input$pID)
    # If patient with the user provided ID exists, populate form fields
    if(!is.null(input$pID) & nrow(patient) > 0){
      updateSelectInput(session, "sex", selected = patient[1,4])
      updateSelectInput(session, "restecg", selected = patient[1,8])
      updateSelectInput(session, "cp", selected = patient[1,5])
      updateNumericInput(session, "age", value = patient[1,3])
      updateNumericInput(session, "trestbps", value = patient[1,6])
      # If the user provided patient ID did not exist in the database, display alert containing error information
    }else{
      shinyjs::info("Patient ID not found.")
    }
  })
  
  
  prediction <- eventReactive(input$predButton, {
    # If user has provided input in every field, input is sent to getPrediction() which builds a model
    # and returns the result of the prediction as a vector, where vector[1] is the probability of no heart disease
    # and vector[2] is the probability for heart disease present.
    if(!is.null(input$sex) & !is.null(input$restecg) & 
       !is.null(input$cp) & !is.null(input$trestbps) & 
       !is.null(input$age)){
      if(inputRangesCorrect()){
        getPrediction(input$sex, input$restecg, input$cp, input$trestbps, input$age)
        # If user did not adhere to commmon ranges for the input values, error will be displayed
      }else{
        shinyjs::info("Input value in form is outside scope of model.")
      }
      # If user did not fill all fields in the form, an error message will be displayed.
    }else{
      shinyjs::info("Please fill all fields in the form.")
    }
  })
 
  # Used to render plot with info that is provided by the prediction() function
  output$predPlot <- renderPlotly({
    
    # Save the results of the prediction to pred
    pred <- prediction()
    
    # Prevent rendering of plot if predictions are null
    if(is.null(pred))
      return()
  
    y <- c('Probability')
    not_present <- c(round(pred[1]*100, digits = 2))
    present <- c(round(pred[2]*100, digits = 2))
    data <- data.frame(y, not_present, present)
    
    plot_ly(data, x = ~not_present, y = ~y, type = 'bar', orientation = 'h', name = 'Not present',
            marker = list(color = 'rgb(67, 205, 128, 0.6)',
                          line = list(color = 'rgb(46, 139, 87, 1.0)',
                                      width = 3))) %>%
      add_trace(x = ~present, name = 'Present',
                marker = list(color = 'rgb(255, 106, 106, 0.6',
                              line = list(color = 'rgb(255, 48, 48, 1.0)',
                                          width = 3))) %>%
      layout(barmode = 'stack',
             margin = list(l = 66, r = 66),
             xaxis = list(title = "%"),
             yaxis = list(title = ""))
  })
  
  # Used to render text of reccomendation
  output$reccText <- renderText({
    # Save the results of the prediction to pred
    pred <- prediction()
    cat("age is: ")
    cat(pred[4])
    
    if(is.null(pred))
      return()
    
    # Chest pain not present
    if(pred[3] == 0){
      "Recommendation: Cath"
    # Chest pain present
    }else if(pred[3] == 1){
      # Pred: not present
      if(pred[1] > pred[2]){
        if(pred[4] < 56){
          "Not present. Recommendation: CCTA. Please take into consideration that many false negatives are present in this patient group."
        }else{
          "Not present. Recommendation: Exe test."
        }
          
      # Pred: present
      }else if(pred[1] < pred[2]){
        "Heart disease present. Recommendation: CCTA."
      }
    }
  })

  getPrediction <- function(sex, restecg, cp, trestbps, age){
    # 1. Loads a dataset 
    # 2. Trains a decision tree using the aforementioned dataset
    # 3. Predicts a class label using 7 attributes
    # 4. Returns prediction as a vector.
    # 
    # Args:
    #   : 7 attributes used by the model to predict the presence of heart disease.
    #
    # Returns:
    # Results of prediction in a vector, where vector[1] is probability for 0
    # and vector[2] is the probability for 1
    
    # Reading dataset from csv file
    dataset <- read.csv("processed_dataset_5_attr.csv", header = TRUE)
    
    # Converts the attributes to the correct data types,
    #   e.g. many attributes should be nominal instead of integers
    dataset$sex <- as.factor(dataset$sex)
    dataset$cp <- as.factor(dataset$cp)
    dataset$trestbps <- as.integer(dataset$trestbps)
    dataset$restecg <- as.factor(dataset$restecg)
    dataset$heart_disease <- as.factor(dataset$heart_disease)
    
    # Utility function used to train model using train() function, returns the decision tree object
    treeModel <- function(seed, training, labels, ctrl) {
      set.seed(seed)
      treeModel <- train(x = training, y = labels, method = "rpart", tuneLength = 5, 
                         parms = list(split = "gini"), trControl = ctrl)
      return (treeModel)
    }
    
    # Setting new variable cv.parameters to an object containing parameters
    # for the cross validation
    cv.parameters <- trainControl(method = "cv", number = 10, repeats = 1)
    features <- c("age", "sex", "trestbps", "restecg", "cp")
    train.dataset <- dataset[, features]
    
    # Use the previously defined treeModel function to train and receive a decision tree
    tree <- treeModel(1337, train.dataset, dataset$heart_disease, cv.parameters)
    
    # Formatting user input to fit input types accepted by decision tree model
    age <- as.numeric(c(age))
    sex <- as.factor(c(sex))
    trestbps <- as.integer(c(trestbps))
    restecg <- as.factor(c(restecg))

    person.data <- data.frame(age, sex, trestbps, restecg, cp)
    
    # Use predict function, inputting the model (tree), person.data (the patients data), 
    # and setting the type of prediction to "prob"
    prediction <- predict(tree, person.data, type = "prob")
    resultVector <- c(prediction[1,1], prediction[1,2], as.numeric(cp), as.numeric(age))
    return(resultVector)
  }
}

# Run app
shinyApp(ui = ui, server = server)

