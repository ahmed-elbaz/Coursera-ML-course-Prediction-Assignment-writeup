trainingUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(trainingUrl, destfile="./pml-training.csv", method="curl")
download.file(testingUrl, destfile="./pml-testing.csv", method="curl")

trainingInit <- read.csv("./pml-training.csv")
testingInit <- read.csv("./pml-testing.csv")

colRemove <- colSums(is.na(trainingInit))==0
trainingCleaned1 <- trainingInit[,colRemove]
testingCleaned1 <- testingInit[,colRemove]

trainingCleaned2 <- trainingCleaned1[,-c(3:5)]
testingCleaned2 <- testingCleaned1[,-c(3:5)]

nzv <- nearZeroVar(trainingCleaned2,saveMetrics = TRUE)
trainingCleaned <- trainingCleaned2[,nzv$nzv==FALSE]
testingCleaned <- testingCleaned2[,nzv$nzv==FALSE]

set.seed(12345)
inTrain <- createDataPartition(trainingCleaned$classe,p=0.7,list = FALSE)
training <- trainingCleaned[inTrain,]
trainingCV <- trainingCleaned[-inTrain,]


modFit <- train(classe ~ . ,data = training, method = "rf", trainControl(method = "cv", 4))
modFit

predictCV <- predict(modFit, trainingCV)
confusionMatrix(trainingCV$classe, predictCV)

predictTesting <- predict(modFit, testingCleaned)



