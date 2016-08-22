library(dplyr)
library(caret)
library(rpart)
library(randomForest)

training <- read.csv("./Coursera/R/data/Course8/Activity-Monitoring-Prediction/data/pml-training.csv", 
                     na.strings=c("NA","#DIV/0!", ""))
testing <- read.csv("./Coursera/R/data/Course8/Activity-Monitoring-Prediction/data/pml-testing.csv", 
                     na.strings=c("NA","#DIV/0!", ""))

## Delete rows with NA

training <-training[,colSums(is.na(training)) == 0]
training <-training[,-c(1,3:7)]

testing <-testing[,colSums(is.na(testing)) == 0]
testing <-testing[,-c(1,3:7)]

set.seed(123)

subsamples <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
subTraining <- training[subsamples, ] 
subTesting <- training[-subsamples, ]

plot(subTraining$classe, col="blue", main="Bar Plot of levels of the variable classe within the subTraining 
     data set", xlab="classe levels", ylab="Frequency")


model1 <- rpart(classe ~ ., data=subTraining, method="class")

# Predicting:
prediction1 <- predict(model1, subTesting, type = "class")

# Plot of the Decision Tree
#rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

confusionMatrix(prediction1, subTesting$classe)


model2 <- randomForest(classe ~. , data=subTraining, method="class")

# Predicting:
prediction2 <- predict(model2, subTesting, type = "class")

# Test results on subTesting data set:
confusionMatrix(prediction2, subTesting$classe)


# Predicting:
prediction1 <- predict(model1, testing, type = "class")
prediction2 <- predict(model2, testing, type = "class")
