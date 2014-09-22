# Titanic: Machine Learning from Disaster
Jeferson Bisconde  
Friday, August 29, 2014  

### Introduction

In this challenge, I completed the analysis of what sorts of people were likely to survive. In particular, I applied the tools of machine learning to predict which passengers **survived** the tragedy. 

        VARIABLE DESCRIPTIONS:
        Survived        Survival
                        (0 = No; 1 = Yes)
        Pclass          Passenger Class
                        (1 = 1st; 2 = 2nd; 3 = 3rd)
        Name            Name
        Sex             Sex
        Age             Age
        SibSp           Number of Siblings/Spouses Aboard
        Parch           Number of Parents/Children Aboard
        Ticket          Ticket Number
        Fare            Passenger Fare
        Cabin           Cabin
        Embarked        Port of Embarkation
                        (C = Cherbourg; Q = Queenstown; S = Southampton)

For more details on this challenge, see <https://www.kaggle.com/c/titanic-gettingStarted>.

**--------------------------------------------------------------------------------------------------------------------------**

As for my overall analysis. I will be predicting which passengers survived based on the following:

>*Pclass, Sex, Age, SibSp, Parch and Fare*

### Functions

This function will clean my data to be able to use it for analysis.

```r
# This function will clean both the titanic train and test dataset
titanicClean = function(trainData, testData){
        # Change the Survived column to be a category
        trainData$Survived <- as.factor(trainData$Survived)
        # Get the mean Age of all passengers
        meanAge <- mean(trainData$Age, na.rm=TRUE)
        meanFare <- mean(trainData$Fare, na.rm=TRUE)
        # Input meanAge to the ones without it
        if (length(trainData[is.na(trainData$Age),]$Age)!=0){
                trainData[is.na(trainData$Age),]$Age <- meanAge
        }
        if (length(testData[is.na(testData$Age),]$Age)!=0){
                testData[is.na(testData$Age),]$Age <- meanAge
        }
        # Input meanAge to the ones without it
        if (length(trainData[is.na(trainData$Fare),]$Fare)!=0){
                trainData[is.na(trainData$Fare),]$Fare <- meanFare
        }
        if (length(testData[is.na(testData$Fare),]$Fare)!=0){
                testData[is.na(testData$Fare),]$Fare <- meanFare
        }
        # Subset the needed columns from the data
        trainData <- subset(trainData, select=-c(PassengerId, Name, Ticket, Cabin, Embarked))
        
        return(list(trainData=trainData, testData=testData))
}
```

This function will *split 1 dataset* into both training and testing (validation):

```r
# This function will split the dataset to a default of 60% training and 40% testing.
dataSplit = function(dataset, p=0.6){
        inBuild <- createDataPartition(dataset$Survived, p=p, list=FALSE)
        myTrain <- dataset[inBuild,]
        myTest <- dataset[-inBuild,]
        # Return both training and testing dataset
        return(list(training=myTrain, testing=myTest))
}
```

This is the main function for analysis:

        (default) - Random Forest
        type=1 - Regression Trees

        PARAMETERS:
        trainData - Data to be trained to produce the prediction model
        
        testData - Data to be tested on to produce the confusion matrix or the predictions
        
        type - what model the function will use
        
        split - 1 to only use the trainData and produce the confusion matrix (initial analysis)
                2 to use both trainData and testData to produce the predictions


```r
# This function will return the prediction on the test dataset based on the train dataset and model type
titanicTest = function(trainData, testData, type=0, split=0){
        # All the necessary libraries
        library(ggplot2); library(caret); library(gbm); library(randomForest); library(rpart)
        
        if (split){
                splitData <- dataSplit(trainData)
                trainData <- splitData$training
                testData <- splitData$testing
        }
        # Clean the Data
        cleanData <- titanicClean(trainData, testData)
        trainData <- cleanData$trainData
        testData <- cleanData$testData
        
        # Depending on what type of model, do the following
        if (type==1){
                modFit <- rpart(Survived ~., data=trainData)
                myPred <- predict(modFit, testData, type="class")
        }     
        else {
                modFit <- randomForest(Survived ~., data=trainData, verbose=FALSE,
                                           trControl=trainControl(allowParallel=TRUE))
                myPred <- predict(modFit, testData)
        }
        if (split){
                return(confusionMatrix(myPred, testData$Survived))
        }
        else {
                return(myPred)
        }
}
```

**--------------------------------------------------------------------------------------------------------------------------**

### Analysis

This is where I start my analysis.


```r
# Load all the necessary files
library(ggplot2); library(caret)
```

```
## Loading required package: lattice
```

```r
titanic <- read.csv(file="train.csv", header=TRUE)
testData <- read.csv(file="test.csv", header=TRUE)
```

My *initial prediction* for Regression Trees.

```r
# Check my initial predictions for rpart -- split=1 will show the confusion Matrix
initPred <- titanicTest(titanic, type=1, split=1)
```

```
## Loading required package: survival
## Loading required package: splines
## 
## Attaching package: 'survival'
## 
## The following object is masked from 'package:caret':
## 
##     cluster
## 
## Loading required package: parallel
## Loaded gbm 2.1
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

* **Confusion Matrix** - Regression Trees

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   0   1
##          0 194  43
##          1  42  77
##                                         
##                Accuracy : 0.761         
##                  95% CI : (0.713, 0.805)
##     No Information Rate : 0.663         
##     P-Value [Acc > NIR] : 3.54e-05      
##                                         
##                   Kappa : 0.465         
##  Mcnemar's Test P-Value : 1             
##                                         
##             Sensitivity : 0.822         
##             Specificity : 0.642         
##          Pos Pred Value : 0.819         
##          Neg Pred Value : 0.647         
##              Prevalence : 0.663         
##          Detection Rate : 0.545         
##    Detection Prevalence : 0.666         
##       Balanced Accuracy : 0.732         
##                                         
##        'Positive' Class : 0             
## 
```

My *initial prediction* for Random Forest.

```r
# Check my initial predictions for random forest
initPred <- titanicTest(titanic, split=1)
```

* **Confusion Matrix** - Random Forest

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   0   1
##          0 183  39
##          1  31 103
##                                         
##                Accuracy : 0.803         
##                  95% CI : (0.758, 0.843)
##     No Information Rate : 0.601         
##     P-Value [Acc > NIR] : 2.53e-16      
##                                         
##                   Kappa : 0.586         
##  Mcnemar's Test P-Value : 0.403         
##                                         
##             Sensitivity : 0.855         
##             Specificity : 0.725         
##          Pos Pred Value : 0.824         
##          Neg Pred Value : 0.769         
##              Prevalence : 0.601         
##          Detection Rate : 0.514         
##    Detection Prevalence : 0.624         
##       Balanced Accuracy : 0.790         
##                                         
##        'Positive' Class : 0             
## 
```

Based on both initial prediction, the `Random Forest model` is good enough for submission to Kaggle.

```r
## - Looks pretty good so will create a model based on titanic dataset to predict the testData
myPred <- titanicTest(titanic, testData)
```

This will create my submission file to Kaggle.

```r
# Write the prediction to file
answers <- data.frame(PassengerId=testData$PassengerId,Survived=myPred)
write.table(answers, file="titanicPrediction.txt",
            sep=",", quote=FALSE, row.names=FALSE)
# To look at the predictions
head(answers)
```

```
##   PassengerId Survived
## 1         892        0
## 2         893        0
## 3         894        0
## 4         895        0
## 5         896        0
## 6         897        0
```

Note that the `answers` may not be the best prediction you can make. This is only my initial submission to check how good a regular Random Forest will do on the public leaderboard.
