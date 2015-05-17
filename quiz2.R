## ------------------------------------------
## Coursera - Data Science Specialization
## Machine Learning - Quiz 2
##
## Date: May 2015
## ------------------------------------------

pkgTest <- function(x) {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}

q01 <- function(resp=1) {
	#	Load the Alzheimer's disease data using the commands:

	pkgTest("AppliedPredictiveModeling")

	library(AppliedPredictiveModeling)
	library(caret)
	data(AlzheimerDisease)

	if(resp==1) {
		adData = data.frame(diagnosis,predictors)
		testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
		training = adData[-testIndex,]
		testing = adData[testIndex,]
		dim(training); dim(testing)
	}
	else
	if(resp==2) {
		adData = data.frame(diagnosis,predictors)
		trainIndex = createDataPartition(diagnosis, p = 0.50, list=TRUE)
		training = adData[trainIndex,]
		testing = adData[-trainIndex,]
		dim(training); dim(testing)
	}
	else
	if(resp==3) {
		adData = data.frame(diagnosis,predictors)
		trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
		training = adData[trainIndex,]
		testing = adData[trainIndex,]
		dim(training); dim(testing)
	}
	else
	if(resp==4) {
		adData = data.frame(predictors)
		trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
		training = adData[trainIndex,]
		testing = adData[-trainIndex,]
		dim(training); dim(testing)
	}
	#	Make a variation of parameter function. You'll see that only "1" is right answer.
}

#	......................................

# ref.: http://www.r-statistics.com/2013/05/log-transformations-for-skewed-and-wide-distributions-from-practical-data-science-with-r/
signedlog10 <- function(x) {
	ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

q02 <- function() {
	#	Load the cement data using the commands:
	library(AppliedPredictiveModeling)
	data(concrete)
	library(caret)
	set.seed(975)
	inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
	training = mixtures[inTrain,]
	testing = mixtures[-inTrain,]
	colnames(concrete)
	#	Make a histogram and confirm the SuperPlasticizer variable is skewed
	hist(concrete$Superplasticizer, main="Histogram of Concrete/Superplasticizer")

	# 	calculate log of "Superplasticizer"
	concrete$log_Superplasticizer <- signedlog10(concrete$Superplasticizer + 1)
	hist(concrete$log_Superplasticizer, main="Histogram of Log-Concrete/Superplasticizer")
	
	#	Why would that be a poor choice for this variable?
	print("It's easy to see there are several zeros in this parameter, so taking the log base 10 would yield infinities.")
}	
	
#	......................................
	
q03	<- function(variance=80) {
	#	Load the Alzheimer's disease data using the commands
	library(caret)
	library(AppliedPredictiveModeling)
	set.seed(3433)
	data(AlzheimerDisease)
	adData = data.frame(diagnosis,predictors)
	inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
	training = adData[ inTrain,]
	testing = adData[-inTrain,]
	#	Find all the predictor variables in the training set that begin with IL
	col_vars <- colnames(training)
	IL_vars <- grep("^IL", col_vars, value=TRUE)
	head(IL_vars,5)
	# 	Perform principal components on these variables with the preProcess() 
	#	function from the caret package
	#	Calculate the number of principal components needed to capture x% of the variance
	preProc <- preProcess(training[, IL_vars], method = "pca", thresh = variance/100)
	preProc$rotation
	pcs <- colnames(preProc$rotation)
	print(pcs)
	answer <- paste("We could see there are", length(pcs), 
			  " principal components required to achieve", variance ,"% of the variance.")
	print(answer)
}	

#	......................................

q04 <- function() {
	#	Load the Alzheimer's disease data using the commands:
	library(caret)
	library(AppliedPredictiveModeling)
	set.seed(3433)
	data(AlzheimerDisease)
	adData = data.frame(diagnosis,predictors)
	inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
	training = adData[ inTrain,]
	testing = adData[-inTrain,]
	
	#	Create a training data set consisting of only the predictors with variable names
	#	beginning with IL and the diagnosis	
	
	set.seed(3433)
	col_vars <- colnames(training)
	IL_vars <- grep("^IL", col_vars, value=TRUE)
	head(IL_vars,15)

	#	Build two predictive models, one using the predictors as they are and one 
	#	using PCA with principal components explaining 80% of the variance in the predictors
	#	(Use method="glm" in the train function.)
	predictors_IL <- predictors[, IL_vars]
	
	#	What is the accuracy of each method in the test set? Which is more accurate?
	df <- data.frame(diagnosis, predictors_IL)
	inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
	training = df[inTrain, ]
	testing = df[-inTrain, ]

	# 	train the data using the first method
	modelFit <- train(diagnosis ~ ., method = "glm", data = training)

	pkgTest("e1071")
	library(e1071)
	predictions <- predict(modelFit, newdata = testing)
	## get the confustion matrix for the first method
	C1 <- confusionMatrix(predictions, testing$diagnosis)
	print(C1)

	A1 <- C1$overall[1]

	## do similar steps with the caret package
	modelFit <- train(training$diagnosis ~ ., method = "glm", preProcess = "pca", 
    	data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
	C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
	print(C2)	
	
	A2 <- C2$overall[1]
	
	print(A1)
	print(A2)
}
	
	
