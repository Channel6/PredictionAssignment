trainingdatafile <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testdatafile  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("pml-testing.csv")){
	download.file(testdatafile, "pml-testing.csv", method="curl")
}
if(!file.exists("pml-training.csv")){
	download.file(trainingdatafile, "pml-training.csv", method="curl")
}


# Usuful libraries
suppressMessages(library(caret))
suppressMessages(library(randomForest))
suppressMessages(library(corrplot))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(RColorBrewer))
suppressMessages(library(rattle))
suppressMessages(library(e1071))

training  <- read.csv(trainingdatafile)
testing <- read.csv(testdatafile)

#initial cleaning
training[training == "#DIV/0!"]  <- NA
training[training == ""]  <- NA

#data exploration
dim(training)
dim(testing)
names(training)

## remove NA values
training <- training[, colSums(is.na(training)) == 0] 
testing <- testing[, colSums(is.na(testing)) == 0] 

training <- training[,-c(1,5)]
testing <- testing[,-c(1,5)]

testing  <- testing[, colSums(is.na(testing)) == 0]
training  <- training[, colSums(is.na(training)) == 0]

#set up x-validation, seed
set.seed(90011009)

inTrain <- createDataPartition(training$classe, p=3/4, list=FALSE)
trainingdata <- training[inTrain, ]
testingdata <- training[-inTrain, ]

# Model Specification and Cross Validation
rfcontrol <- trainControl(
	method="cv",
	5
)
rfmodel <- train(
	classe ~ .,
	data=trainingdata,
	method="rf",
	trControl=rfcontrol,
	ntree=250
)
rfmodel
# performance eval of RF
rfpredict <- predict(rfmodel, testingdata)
confusionMatrix(testingdata$classe, rfpredict)

accuracy <- postResample(rfpredict, testingdata$classe)
accuracy

oose <- 1 - as.numeric(confusionMatrix(testingdata$classe, rfpredict)$overall[1])
oose

# Prediction for the test set
final_result <- predict(rfmodel, testingdata)
final_result

# show outcomes
trainingdatanumber <- trainingdata[, sapply(trainingdata, is.numeric)]

corrPlot <- cor(trainingdatanumber[, -length(names(trainingdatanumber))])
corrplot(corrPlot, method="color")

tree_tree <- rpart(classe ~ ., data=trainingdata, method="class")
prp(tree_tree) # plotting using library(rpart.plot)

