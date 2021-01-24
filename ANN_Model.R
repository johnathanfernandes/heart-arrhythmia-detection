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

# Removing rows with NA values
arrhythmia.data <- na.omit(arrhythmia.data)

# Convert all columns to numeric, except columns 14 and 280
arrhythmia.data[-c(14, 280)] <-
  lapply(arrhythmia.data[-c(14, 280)], as.numeric)

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
included_features <-
  vartable[c(14, which(vartable$variance > 50)),]
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
h2o.init()

# Copy data subset into h2o dataframe
arrhythmia_hf <-
  as.h2o(arrhythmia.subset, destination_frame = "arrhythmia_hf")

# Convert "diagnosis" , "class" and "F14" columns of h2o dataframe into factors
arrhythmia_hf[, 2] <- h2o.asfactor(arrhythmia_hf[, 2])
arrhythmia_hf[, 3] <- h2o.asfactor(arrhythmia_hf[, 3])
arrhythmia_hf[, 4] <- h2o.asfactor(arrhythmia_hf[, 4])

# Create correlation matrix table
corr <- as.data.frame(h2o.cor(arrhythmia_hf[,-c(1, 3, 4)]))
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

# Artificial Neural Network
ANN <-
  h2o.deeplearning(
    x = setdiff(colnames(train), c("diagnosis", "weights", "class")),
    y = "diagnosis",
    weights_column = "weights",
    model_id = "dl_model",
    training_frame = train,
    validation_frame = valid,
    nfolds = 15,
    keep_cross_validation_fold_assignment = TRUE,
    fold_assignment = "Stratified",
    activation = "RectifierWithDropout",
    score_each_iteration = TRUE,
    hidden = c(200, 200, 200, 200, 200),
    epochs = 100,
    variable_importances = TRUE,
    export_weights_and_biases = TRUE
  )

# Generate performance metrics and plots for reporting
plot(ANN, timestep = "epochs", metric = "classification_error")
plot(ANN, timestep = "epochs", metric = "logloss")
plot(ANN, timestep = "epochs", metric = "rmse")
perf <- h2o.performance(ANN, test)
perf
plot(perf)
h2o.logloss(perf)
h2o.mse(perf)
h2o.auc(perf)
h2o.accuracy(perf, thresholds = "max")
h2o.precision(perf, thresholds = "max")
h2o.recall(perf, thresholds = "max")
h2o.F1(perf, thresholds = "max")

# Shut down h2o server
h2o.shutdown()
y
# Disconnect from SQL server
dbDisconnect(mydb)
