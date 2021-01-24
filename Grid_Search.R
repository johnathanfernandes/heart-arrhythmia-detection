setwd("D:/RStudio/arrhythmia")
# Data preprocessing

# Import "RMySQL" library to operate on SQL server
library(RMySQL)

# Connect to SQL server
mydb = dbConnect(
  MySQL(),
  user = 'root',
  password = 'password',
  dbname = 'arrhythmia',
  host = 'localhost'
)

# Data import

# Define and execute SQL query
query <- dbSendQuery(mydb, "SELECT * FROM data")
arrhythmia.data <- dbFetch(query)

#Replace "?" with "NA"
arrhythmia.data[arrhythmia.data == "?"] <- NA

# Create dataframe with names of features containing NA values and number of values
natable <-
  data.frame(Feature = colnames(arrhythmia.data[colSums(is.na(arrhythmia.data)) > 0]),
             Count = colSums(is.na(arrhythmia.data[colSums(is.na(arrhythmia.data)) > 0])))

# Export "natable" dataframe for analytics and visualization
dbWriteTable(mydb,
             name = 'natable',
             value = natable,
             append = TRUE)

# Converting F14 to boolean indicating presence of J wave
arrhythmia.data$F14 <- !is.na(arrhythmia.data$F14)

# Convert all columns to numeric, except columns 14 and 280
arrhythmia.data[-c(14, 280)] <-
  lapply(arrhythmia.data[-c(14, 280)], as.numeric)

# Removing rows with NA values
arrhythmia.data <- na.omit(arrhythmia.data)

# Rename column 280 to "class"
colnames(arrhythmia.data)[280] <- "class"

# Convert "classes" to factors
arrhythmia.data$class <- as.factor(arrhythmia.data$class)

# Create "diagnosis" column, based on values of "class" column
arrhythmia.data$diagnosis <-
  ifelse(arrhythmia.data$class == 1, "healthy", "arrhythmia")

# Convert "diagnosis" to factors for classification
arrhythmia.data$diagnosis <- as.factor(arrhythmia.data$diagnosis)

# Import "matrixStats" library for "colvars" function, calculate variance of features
library(matrixStats)

# Feature selection

# Create "vartable" dataframe containing feature names and respective variances
vartable <-
  data.frame(feature = colnames(arrhythmia.data[-c(280, 281)]),
             variance = colVars(as.matrix(arrhythmia.data[-c(280, 281)])))


# Export "vartable" dataframe to database for visualization
dbWriteTable(mydb,
             name = 'vartable',
             value = vartable,
             append = TRUE)

# Create subset of data with features with variance greater than 50
arrhythmia.subset <-
  arrhythmia.data[, c(281, 280, 14, which(vartable$variance > 50))]

# Create list of features with variance > 50 for prediction
included_features <- vartable[c(14, which(vartable$variance > 50)), ]
write_csv(included_features, "featurelist.csv")

# Carry out principal component analysis, after scaling and centering data
pcaresults <-
  prcomp(arrhythmia.subset[-c(1, 2, 3)], scale = T, center = T)

# Create "pcatable" dataframe storing values of first 2 principle components and also diagnosis of each data point
pcatable <-
  data.frame(PC1 = pcaresults$x[, 1],
             PC2 = pcaresults$x[, 2],
             diagnosis = arrhythmia.subset$diagnosis)

# Export "pcatable" dataframe to database for visualization
dbWriteTable(mydb,
             name = 'pcatable',
             value = pcatable,
             append = TRUE)

# Create list of weights for each data point based on conditions determined after visualization
weights <- ifelse((pcatable$PC1 < (-7.5) |
                     pcatable$PC2 < (-5.8) |
                     pcatable$PC2 > 7.4),
                  2,
                  1)

# Attach weights to data subset
arrhythmia.subset <- cbind(weights, arrhythmia.subset)

# H2O

# Import "h2o" library
library(h2o)

# Initialize h2o cluster with default parameters
h2o.init(nthreads = 6,
         min_mem_size = "10G")

# Copy data subset into h2o dataframe
arrhythmia_hf <-
  as.h2o(arrhythmia.subset, destination_frame = "arrhythmia_hf")

# Convert "diagnosis" , "class" and "F14" columns of h2o dataframe into factors
arrhythmia_hf[, 2] <- h2o.asfactor(arrhythmia_hf[, 2])
arrhythmia_hf[, 3] <- h2o.asfactor(arrhythmia_hf[, 3])
arrhythmia_hf[, 4] <- h2o.asfactor(arrhythmia_hf[, 4])

# Create correlation matrix table
corr <- as.data.frame(h2o.cor(arrhythmia_hf[, -c(1, 3, 4)]))
temp <- colnames(corr)
rownames(corr) <- temp

#Create correlation matrix visual for at-a-glance review
image(
  z <- as.matrix(corr),
  col = hcl.colors(n = 100,
                   palette = "Green-Orange",
                   rev = TRUE),
  main = "Correlation Matrix",
  xlab = "Feature",
  ylab = "Feature"
)

# Export "corr" dataframe to database
dbWriteTable(mydb,
             name = 'correlation',
             value = corr,
             append = TRUE)

# Split dataframe into training, validation and testing sets
splits <- h2o.splitFrame(arrhythmia_hf, ratios = c(0.7, 0.15))
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# Random grid search

# Create list of hyperparameters for random grid search
hyper_params <- list(
  activation = c(
    "Maxout",
    "MaxoutWithDropout",
    "Rectifier",
    "RectifierWithDropout",
    "Tanh",
    "TanhWithDropout"
  ),
  epochs = c(50, 60, 80),
  epsilon = c(1e-7, 1e-8, 1e-6),
  hidden = list(c(64, 32, 16, 8, 4), c(10, 10, 10, 10), c(46, 23, 11, 5)),
  input_dropout_ratio = c(0.5, 0.1, 0.15),
  l1 = c(0, 1e-5, 1e-6),
  l2 = c(0, 1e-5, 1e-6),
  momentum_start = c(0.25, 0.5, 0.75),
  momentum_stable = c(0.25, 0.5, 0.75),
  max_w2 = c(1e4, 1e5, 3.4028235e+38),
  rate_annealing = c(1e-4, 1e-5, 1e-6),
  rate = c(0.0005, 0.001, 0.05),
  rho = c(0.8, 0.9, 1)
)

# Create list of stopping criteria for random grid search
search_criteria <- list(
  max_models = 50,
  max_runtime_secs = 600,
  stopping_tolerance = 0.001,
  stopping_rounds = 15,
  strategy = "RandomDiscrete"
)

# Execute random grid search to generate multiple models (23 in present execution)
grid <- h2o.grid(
  algorithm = "deeplearning",
  x = setdiff(
    colnames(train),
    c(response = "diagnosis", weights = "weights", "class")
  ),
  y = "diagnosis",
  weights_column = "weights",
  grid_id = "grid",
  training_frame = train,
  validation_frame = valid,
  nfolds = 25,
  fold_assignment = "Stratified",
  hyper_params = hyper_params,
  search_criteria = search_criteria
)

# Model selection

# Create "gridvalidation" dataframe to record performance metrics for all models
gridvalidation <-
  data.frame(
    id = NA,
    error = NA,
    logloss = NA,
    mse = NA,
    auc = NA,
    accuracy = NA,
    precision = NA,
    recall = NA,
    f1 = NA
  )

# Generate performance metrics for all models, excluding failed models (model ID 12 in present execution))
v <- (setdiff(1:length(grid@model_ids), 12))
for (x in v)
{
  temp <- h2o.getModel(grid@model_ids[[x]])
  pred <- data.frame(
    class = as.vector(test$class),
    actual = as.vector(test$diagnosis),
    as.data.frame(h2o.predict(object = temp, newdata = test))
  )
  perf <- h2o.performance(temp, test)
  pred$error <- ifelse(pred$actual == pred$predict, 0, 1)
  gridvalidation <- rbind(
    gridvalidation,
    c(
      x,
      sum(pred$error),
      h2o.logloss(perf),
      h2o.mse(perf),
      h2o.auc(perf),
      h2o.accuracy(perf, thresholds = "max"),
      h2o.precision(perf, thresholds = "max"),
      h2o.recall(perf, thresholds = "max"),
      h2o.F1(perf, thresholds = "max")
    )
  )
  print(x)
}
gridvalidation <- na.omit(gridvalidation)

# Export "gridvalidation" dataframe to database for visualization and model selection
dbWriteTable(mydb,
             name = 'gridvalidation',
             value = gridvalidation,
             append = TRUE)

# Save best model (5 in present execution) to further analysis
bestmodel <- h2o.getModel(grid@model_ids[[5]])

# Save model to local disk for future use
h2o.saveModel(bestmodel, path = "C:/Users/user/Documents/RStudio/arrhythmia/model")

# Load model from local disk
bestmodel <-
  h2o.loadModel("C:/Users/user/Documents/RStudio/arrhythmia/model/grid_model_5")

# Model performance evaluation

# Generate performance metrics and plots for reporting
plot(bestmodel, timestep = "epochs", metric = "classification_error")
plot(bestmodel, timestep = "epochs", metric = "logloss")
plot(bestmodel, timestep = "epochs", metric = "rmse")
perf <- h2o.performance(bestmodel, test)
perf
plot(perf)

# Create "predictions" dataframe, containing actual and predicted values for test set, for final prediction testing
predictions <- data.frame(
  class = as.vector(test$class),
  actual = as.vector(test$diagnosis),
  as.data.frame(h2o.predict(object = bestmodel, newdata = test))
)

# Create additional column to indicate if prediction was correct or incorrect
predictions$error <-
  ifelse(predictions$actual == predictions$predict, 0, 1)

# Export "predictions" dataframe to database for further visualization and insights
dbWriteTable(mydb,
             name = 'predictions',
             value = predictions,
             append = TRUE)

# Shut down h2o server
h2o.shutdown()
y
# Disconnect from SQL server
dbDisconnect(mydb)
