setwd("D:/RStudio/arrhythmia")
# Import "h2o" library (v 3.30.0.1)
library(h2o)

# Initialize h2o cluster with default parameters
h2o.init()

# Load model from local disk
bestmodel <-
    h2o.loadModel("D:/RStudio/arrhythmia/model/grid_model_5")

# Import demo sample data
demo <- read.csv("test_RAW.csv")

# Converting feature 14 to boolean indicating presence of J wave
demo$F14 <- !is.na(demo$F14)

# Convert all columns to numeric, except columns 14
demo[-c(14)] <- lapply(demo[-c(14)], as.numeric)

# Import list of features to use
features <- read.csv("featurelist.csv")
featurelist <- as.vector(features$feature)

# Remove unused features
demo <- demo[, which(colnames(demo) %in% featurelist)]

# Copy data into h2o dataframe
demo_hf <- as.h2o(demo, destination_frame = "demo_hf")

# Sample prediction on demo data
pred <- (h2o.predict(object = bestmodel, newdata = demo_hf))
print(pred)

typeof# Shut down h2o server
h2o.shutdown()
y
