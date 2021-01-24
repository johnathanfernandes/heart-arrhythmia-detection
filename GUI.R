setwd("D:/RStudio/arrhythmia")
library(shiny) #Import shiny library to create GUI

# Define frontend UI design
ui <- fluidPage(
  headerPanel(
    "Final Year Major Project - Deep Learning Assisted Arrhythmia Detector"
  ),
  sidebarLayout(
    sidebarPanel(
      p("Final year Major Project"),
      p("Deep Learning Assisted Arrhythmia Detector"),
      p("Submitted by:"),
      tags$ul(
        tags$li("Aneesh Poduval, IC-A-06"),
        tags$li("Sarthak Chudgar, IC-A-15"),
        tags$li("Harshal Dharap, IC-A-22"),
        tags$li("Johnathan Fernandes, IC-A-35")
      ),
      p("Semester 7, August 2020 - December 2020")
    ),
    
    mainPanel(
      fileInput("file1", "Upload File: "),
      actionButton("calc", "Calculate"),
      hr(),
      textOutput("status"),
      mainPanel("Prediction case :"),
      tableOutput("optext"),
      mainPanel("Confidence :"),
      tableOutput("conf")
    )
  )
)
#Define backend server
server <- function(input, output)
{
  observeEvent(input$calc,
               {
                 demo <- read.csv(input$file1$datapath)
                 
                 progress <- shiny::Progress$new()
                 on.exit(progress$close())
                 
                 progress$set(message = "Calculating prediction", value = 0)
                 
                 progress$inc(1, detail = "Importing h2o lib")
                 # Import "h2o" library (v 3.30.0.1)
                 library(h2o)
                 
                 # Initialize h2o cluster with default parameters
                 progress$inc(2, detail = "Initializing h2o cluster")
                 h2o.init()
                 
                 # Load model from local disk
                 progress$inc(3, detail = "Importing prediction model")
                 bestmodel <-
                   h2o.loadModel("D:/RStudio/arrhythmia/model/grid_model_5")
                 
                 # Converting feature 14 to boolean indicating presence of J wave
                 progress$inc(4, detail = "Processing data")
                 demo$F14 <- !is.na(demo$F14)
                 
                 # Convert all columns to numeric, except columns 14
                 progress$inc(5, detail = "Processing data")
                 demo[-c(14)] <- lapply(demo[-c(14)], as.numeric)
                 
                 # Import list of features to use
                 progress$inc(6, detail = "Importing feature list")
                 features <- read.csv("featurelist.csv")
                 featurelist <- as.vector(features$feature)
                 
                 # Remove unused features
                 progress$inc(7, detail = "Processing data")
                 demo <-
                   demo[, which(colnames(demo) %in% featurelist)]
                 
                 # Copy data into h2o dataframe
                 progress$inc(8, detail = "Importing data to h2o")
                 demo_hf <-
                   as.h2o(demo, destination_frame = "demo_hf")
                 
                 # Sample prediction on demo data
                 progress$inc(9, detail = "Predicting results")
                 pred <-
                   (h2o.predict(object = bestmodel, newdata = demo_hf))
                 pred <- as.data.frame(pred)
                 
                 progress$inc(10, detail = "Done!")
                 output$optext <-
                   renderTable(as.data.frame(pred$predict),
                               colnames = FALSE,
                               bordered = TRUE)
                 
                 # Calculate confidence from predicted probabilities
                 prob_h <- as.data.frame(pred$healthy)
                 prob_a <- as.data.frame(pred$arrhythmia)
                 
                 library("dplyr")
                 confidence <- (max(prob_h, prob_a) * 100) %>%
                   round(2) %>%
                   as.character() %>%
                   paste("%")
                 
                 output$conf <-
                   renderTable(confidence,
                               colnames = FALSE,
                               bordered = TRUE)
               })
  
}
shinyApp(ui, server) #Run app using UI and Server
