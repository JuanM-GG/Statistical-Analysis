# read the file with the yeast information
write.table(x = setYeast,file = "yeast.csv",row.names = FALSE, sep = ",")
data <- read.csv("yeast.csv")
View(data)

# To achieve reproducible model; set the random seed number
set.seed(100)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(data$yeast, p=0.8, list = FALSE)
TrainingSet <- data[TrainingIndex,] # Training Set
TestingSet <- data[-TrainingIndex,] # Test Set
View(TrainingIndex)

# SVM model (polynomial kernel)

# Build training model
Model <- train(yeast ~., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale","center"),
               trControl = trainControl(method = "none"),
               tuneGrid = data.frame(degree = 1, scale = 1, C = 1)
               )

# Build CV model
Model.cv <- train(yeast ~., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess = c("scale","center"),
                  trControl = trainControl(method = "cv", number = 10),
                  tuneGrid = data.frame(degree = 1, scale = 1, C = 1)
                  )


# Apply model for prediction
Model.training <- predict(Model, TrainingSet) # Apply model to make prediction on Training Set

Model.testing <- predict(Model, TestingSet) # Apply model to make prediction on Testing Set

Model.cv <- predict(Model.cv, TrainingSet) # Perform crossvalidation

# Model performance (Display confusion matrix and statistics)

Model.training.confusion <- confusionMatrix(Model.training, TrainingSet$yeast)
print(Model.training.confusion)
Model.testing.confusion <- confusionMatrix(Model.testing, TestingSet$yeast)
print(Model.testing.confusion)
Model.cv.confusion <- confusionMatrix(Model.cv, TrainingSet$yeast)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")
