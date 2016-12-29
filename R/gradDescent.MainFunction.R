#############################################################################
#
#  This file is a part of the R package "gradDescent".
#
#  Author: Dendi Handian 
#  Co-author: Imam Fachmi Nasrulloh
#  Supervisors: Lala Septem Riza, Rani Megasari
#  Copyright (c) Department of Computer Science Education, Indonesia University of Education.
#
#  This package is free software: you can redistribute it and/or modify it under
#  the terms of the GNU General Public License as published by the Free Software
#  Foundation, either version 2 of the License, or (at your option) any later version.
#
#  This package is distributed in the hope that it will be useful, but WITHOUT
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
#  A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
#############################################################################
#' This is one of the main function of the package This function is used to 
#' create a preprocessed data that can be use in the further main function.
#' 
#' This function makes the input data to be split into data train and data test
#' with user input of data train rate composition. After split up, both data train
#' and test will be normalized (if chosen) with variance or minmax scaling method.
#' Both data train and test will be transformed into matrix format for quick learn
#' process later. Finally, those data will be wrapped into a class along with 
#' normalization method and parameters.
#'
#' @title The gradDescent data preprocessing function
#'
#' @param dataSet a data frame of data that will be processed in this function.
#'        dataSet must have at least two column (variable) and 10 row data that
#'        contain only number (integer or float). The last column will be defined
#'        as output variable.
#' @param trainRate a float value between 0 and 1 that will decide the data train
#'        rate composition (data_length * trainRate), and the remaining rate
#'        will set the data test rate. The default trainRate is 0.5
#' @param normalizationMethod this parameter determines the normalization method 
#'        to be used. The default normalizationMethod is NULL, which means the
#'        data will not be normalized.
#'        The method parameters that can be use:
#' \itemize{
#' \item \code{"variance"}: variance scaling method to normalize/scaling data 
#'                 into standard score.;
#' \item \code{"minmax"}: minmax scaling method to scaling data into range of
#'                 0 to 1.
#' }
#' @param seed a integer value for static random. Default value is NULL, which means 
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## I. Preprocess data with normalization
#' ## Suppose the data have two column (input and output variable) data and  
#' ## 10 row data that contain only number values.  
#' ##################################   
#' ## Data must be in data.frame form and the last column is the output
#' ## variable. Then, set normalizationMethod value with "variance".
#' square_feet <- c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700)
#'
#' price <- c(245,312,279,308,199,219,405,324,319,255)
#'
#' house_price <- data.frame(square_feet, price)
#' 
#' house_price.data <- gradDescent.preprocessing(
#'       house_price,
#'       normalizationMethod="variance"
#'    )
#'
#' ##################################
#' ## II. Preprocess data without normalization
#' ## Suppose the data have two column (input and output variable) data and  
#' ## 10 row data that contain only number values.  
#' ##################################   
#' ## Data must be in data.frame form and the last column is the output
#' ## variable.
#' square_feet <- c(-0.035,0.382,0.753,1.519,-1.471,-0.275,-0.394,-0.694,-0.035,1.758)
#'
#' price <- c(-0.523,0.357,-0.689,1.968,-1.453,0.423,-1.121,0.539,-0.124,0.623)
#'
#' house_price <- data.frame(square_feet, price)
#' 
#' house_price.data <- gradDescent.preprocessing(
#'       house_price
#'    )
#'
#' ##################################
#' ## III. Preprocess data with custom train rate
#' ## Suppose the data have two column (input and output variable) data and  
#' ## 10 row data that contain only number values.  
#' ##################################   
#' ## Data must be in data.frame form and the last column is the output
#' ## variable.
#' square_feet <- c(-0.035,0.382,0.753,1.519,-1.471,-0.275,-0.394,-0.694,-0.035,1.758)
#'
#' price <- c(-0.523,0.357,-0.689,1.968,-1.453,0.423,-1.121,0.539,-0.124,0.623)
#'
#' house_price <- data.frame(square_feet, price)
#' 
#' house_price.data <- gradDescent.preprocessing(
#'       house_price,
#'       trainRate=0.5
#'    )
#' @export

gradDescent.preprocessing <- function(
	dataSet, 
	trainRate=0.5, 
	normalizationMethod=NULL,
	seed=NULL)
{

	#validate row length
	if(nrow(dataSet) < 10){
		stop("at least the dataSet column must be 2 or more")
	}

	#validate column length
	if(ncol(dataSet) < 2){
		stop("at least the dataSet column must be 2 or more")
	}

	#validate trainRate input
	if (trainRate <= 0 | trainRate >= 1){
		stop("trainRate must be between 0 and 1")
	}

	if(!is.null(normalizationMethod)){
		if(normalizationMethod != "variance" & normalizationMethod != "minmax"){
			stop("unknown normalizationMethod")
		}
	}

	#get all index in dataSet
	allDataIndex <- 1:nrow(dataSet)

	#set static random
	set.seed(seed)

	#randomize and set dataTrain index
	dataTrainIndex <- sample(allDataIndex, trunc(length(allDataIndex) * trainRate))

	#unset static random
	set.seed(NULL)

	#set dataTrain and dataTest
	dataTrain <- dataSet[dataTrainIndex, ]
	dataTest <- dataSet[-dataTrainIndex, ]

	#set normalizationParameter default to NULL
	normalizationParameter <- "NULL"
	
	#normalize data
	if(!is.null(normalizationMethod)){
		if(normalizationMethod == "variance"){
			#get mean and standard deviation
			varianceParameter <- getMeanStd(dataSet)
			#normalization data with variance method
			dataTrain <- varianceScaling(dataTrain, varianceParameter)
			dataTest <- varianceScaling(dataTest, varianceParameter)
			#store the used normalization parameter
			normalizationParameter <- varianceParameter
		}else if(normalizationMethod == "minmax"){
			#get min and max
			minmaxParameter <- getMinMax(dataSet)
			#normalization data with variance method
			dataTrain <- minmaxScaling(dataTrain, minmaxParameter)
			dataTest <- minmaxScaling(dataTest, minmaxParameter)
			#store the used normalization parameter
			normalizationParameter <- minmaxParameter
		}
	}

	#transform data into matrix format
	dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
	dataTest <- matrix(unlist(dataTest), ncol=ncol(dataTest), byrow=FALSE)

	#create and set a result list
	result <- list()
	result$dataTrain <- dataTrain	
	result$dataTest <- dataTest
	result$normalizationMethod <- 	normalizationMethod
	result$normalizationParameter <- normalizationParameter

	#wrap result into class (object)
	class(result) <- "gradDescent.data.object"
	
	return(result)
}

#' This is the second and central main function to build a prediction
#' model with various gradient descent algorithms using data object
#' that created from gradDescent.preprocessing function. The model and
#' used parameter will be wrapped into a class.
#'
#' @title the gradDescent model building function
#'
#' @param gradDescent.data a object that created exclusively from 
#'        gradDescent.preprocessing function.
#' @param methodType this parameter determine the learning algorithm
#'        to be used. The default method is "GD".
#'        The Following methods are implemented:
#' \itemize{
#' \item \code{"GD"}: Gradient descent method to calculate gradient.
#' \item \code{"SAG"}: Stochastic Average gradient descent method to calculate gradient.
#' \item \code{"ADAGRAD"} : Adaptive Subgradient method to calculate gradient.
#' \item \code{"ADADELTA"} : Adadelta method to calculate gradient.
#' \item \code{"RMSPROP"} : RMSprop method to calculate gradient.
#' \item \code{"ADAM"} : Adaptive Moment Estimation method to calculate gradient.
#' }
#' @param alpha a float value for learning rate. Default value is 0.1
#' @param maxIter a integer value for maximum iteration for learning. Default 
#'        value is 10. 
#' @param shuffle a boolean value to shuffle data. Default value is TRUE, which means
#'        the data will be shuffled before the learning.
#' @param batchRate a float value for batch size of data that will used for learning.
#'        batchRate must be more or equal to 0.1 and least or equal to 1. Default value
#'        is 1, which means will process all data train. 
#' @param momentum a float value to give a constant speed to learning process.
#'        Default value is 0, which means learning has no momentum.
#' @param accelerate  a boolean value to enable accelerate in the learning with momentum.
#'        If the the momentum set to 0, there will be no accelerate. Default value is TRUE,
#'        which means the learning process do the accelerate.
#' @param stochastic a boolean value to enable stochastic, which mean to select one random
#'        value in data train, instead process all data train. Default value is TRUE,  
#'        which means the learning will do the stochastic.
#' @param smooth a float value to handle zero division issue in certain learning method.
#'         Default value is 0.0000001.
#' @param delay a float value represent time second to give extra-time in iterations.
#' @param seed a integer value for static random. Default value is NULL, which means 
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## I. Build model with Gradient Descent Method
#' square_feet <- c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700)
#'
#' price <- c(245,312,279,308,199,219,405,324,319,255)
#'
#' house_price <- data.frame(square_feet, price)
#' 
#' ## do the preprocessing stage
#' house_price.data <- gradDescent.preprocessing(
#'       house_price,
#'       normalizationMethod="variance"
#'    )
#'
#' ## do the model building stage
#' GD.model <- gradDescent.learn(
#'       house_price.data,
#'       methodType="GD" 
#'    )
#'
#'
#' ##################################
#' ## II. Build model with Accelerated-Momentum Gradient Descent Method
#' square_feet <- c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700)
#'
#' price <- c(245,312,279,308,199,219,405,324,319,255)
#'
#' house_price <- data.frame(square_feet, price)
#' 
#' ## do the preprocessing stage
#' house_price.data <- gradDescent.preprocessing(
#'       house_price,
#'       normalizationMethod="variance"
#'    )
#'
#' ## do the model building stage
#' GD.model <- gradDescent.learn(
#'       house_price.data,
#'       momentum=0.9,
#'       accelerate=TRUE
#'    )
#'
#' @export

gradDescent.learn <- function(
	gradDescent.data, 
	methodType="GD", 
	alpha=0.1, 
	maxIter=10, 
	shuffle=FALSE, 
	batchRate=1, 
	momentum=0, 
	accelerate=FALSE, 
	stochastic=FALSE, 
	smooth=0.0000001,
	delay=0, 
	seed=NULL)
{
	#validate gradDescent objects
	if(!inherits(gradDescent.data, "gradDescent.data.object")){
		stop("not a legitimate gradDescent data object")
	}

	#validate batchRate
	if(batchRate < 0.1 | batchRate > 1){
		stop("batchRate must be more or equal to 0.1 and least or equal to 1")
	}

	#get dataTrain
	dataTrain <- gradDescent.data$dataTrain

	#shuffle data if asked	
	if(shuffle){
		set.seed(seed)
		dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
		set.seed(NULL)
	}
	

	#mini-batch processing
	batch.size <- nrow(dataTrain) * batchRate 
	dataTrain <- dataTrain[1:batch.size,]

	#bind 1 column to dataTrain
	dataTrain <- cbind(1, dataTrain)

	#parse input and output
	inputData <- dataTrain[,1:ncol(dataTrain)-1]
	outputData <- dataTrain[,ncol(dataTrain)]

	#initialize theta
	theta <- getTheta(ncol(dataTrain)-1, seed=seed)

	#wrap parameters into a list
	list <- list()
	list$rowLength <- nrow(dataTrain)
	list$theta <- theta
	list$alpha <- alpha		
	list$momentum <- momentum
	list$smooth <- smooth
	list$stochastic <- stochastic
	list$accelerate <- accelerate
	list$maxIter <- maxIter
	list$delay <- delay
	list$seed <- seed

	#process the chosen methodType with running time tracking background
	rt <- system.time(
		if(methodType == "GD"){
			theta <- gradientDescent(inputData, outputData, list)
		}else if(methodType == "ADAGRAD"){
			theta <- adagrad(inputData, outputData, list)
		}else if(methodType == "ADADELTA"){
			theta <- adadelta(inputData, outputData, list)
		}else if(methodType == "SAG"){
			theta <- stochasticAverage(inputData, outputData, list)
		}else if(methodType == "RMSPROP"){
			theta <- rmsprop(inputData, outputData, list)
		}else if(methodType == "ADAM"){
			theta <- adam(inputData, outputData, list)
		}
		# , gcFirst=FALSE
	)

	#create and set result list
	result <- list
	result$theta <- theta
	result$batchRate <- batchRate
	result$shuffle <- shuffle
	result$methodType <- methodType
	result$time <- rt[[3]]

	#wrap result into class (object)
	class(result) <- "gradDescent.model.object"

	return(result)
}

#' This is the last of main function to predict the data test in gradDescent.data.
#' Input data in data test will be predicted using user-given input of model that
#' created exclusively from gradDescent.learn function. The predicted value will be
#' merged with the input data and denormalized to reveal the original value. This
#' function also calculate the error rate of the predicted data using Mean-Square-
#' Error and Root-Mean-Square-Error. Finaly, predicted data and error rate will be 
#' wrapped into a class.
#'
#' @title the gradDescent prediction stage
#'
#' @param gradDescent.model a object that created exclusively from
#'        gradDescent.learn function
#' @param gradDescent.data a object that created exclusively from 
#'        gradDescent.preprocessing function.
#' 
#' @examples
#' ##################################
#' ## I. Predict using gradient descent model
#' square_feet <- c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700)
#'
#' price <- c(245,312,279,308,199,219,405,324,319,255)
#'
#' house_price <- data.frame(square_feet, price)
#' 
#' ## do the preprocessing stage
#' house_price.data <- gradDescent.preprocessing(
#'       house_price,
#'       normalizationMethod="variance"
#'    )
#'
#' ## do the model building stage
#' GD.model <- gradDescent.learn(
#'       house_price.data,
#'       methodType="GD" 
#'    )
#' ## predict data
#' GD.predict <- gradDescent.predict(
#'       GD.model,
#'       house_price.data
#'    )
#' ## view predicted value
#' print(GD.predict$predictionData)
#'
#' ## view mse and rmse error
#' print(GD.predict$mse)
#' print(GD.predict$rmse)
#'
#' @export

gradDescent.predict <- function(gradDescent.model, gradDescent.data){
	#validate gradDescent objects
	if(!inherits(gradDescent.model, "gradDescent.model.object")){
		stop("not a legitimate gradDescent model object")
	}
	if(!inherits(gradDescent.data, "gradDescent.data.object")){
		stop("not a legitimate gradDescent data object")
	}

	#get dataTest
	dataTest <- gradDescent.data$dataTest

	#bind 1 column to dataTrain
	dataTest <- cbind(1,dataTest)

	#parse input and output
	inputData <- dataTest[,1:ncol(dataTest)-1]
	outputData <- dataTest[,ncol(dataTest)]
	
	#get theta
	theta <- gradDescent.model$theta

	#predict 
	prediction <- inputData %*% t(theta)

	#merge inputData and prediction
	predictionData <- cbind(inputData[,2:(ncol(inputData))],prediction)

	#transform into data.frame
	predictionData <- as.data.frame(predictionData)

	#remove 1-value column in data test and change to data.frame
	dataTest <- as.data.frame(dataTest[,2:ncol(dataTest)])

	#denormalization
	normalizationMethod <- gradDescent.data$normalizationMethod
	if(!is.null(normalizationMethod)){
		param <- gradDescent.data$normalizationParameter
		if(gradDescent.data$normalizationMethod == "variance"){
			predictionData <- varianceDescaling(predictionData, param)
			dataTest <- varianceDescaling(dataTest, param)
		}else if(gradDescent.data$normalizationMethod == "minmax"){
			predictionData <- minmaxDescaling(predictionData, param)
			dataTest <- minmaxDescaling(dataTest, param)
		}
	}

	#calculate error
	mse <- mseCalculation(outputData, prediction)
	rmse <- rmseCalculation(outputData, prediction)

	#create and set result list
	result <- list()
	result$predictionData <- predictionData
	result$dataTest <- dataTest
	result$mse <- mse
	result$rmse <- rmse

	#wrap result into class (object)
	class(result) <- "gradDescent.prediction.object"

	return(result)
}

#' This is the another last of main function to predict the user-given input data.
#' Instead of predict the data test in gradDescent.data object, this function will
#' predict user-given input data. Same as previous predict function, the output 
#' will be prediction data that wrapped into a class without error rate value. 
#'
#' @title the gradDescent input data prediction stage
#'
#' @param gradDescent.model a object that created exclusively from
#'        gradDescent.learn function
#' @param gradDescent.data a object that created exclusively from 
#'        gradDescent.preprocessing function.
#' @param inputData a data.frame of input data to be predicted.
#'        input variable must be match with the dataTrain (excluded 
#'        outputData).
#' @param normalize a boolean value for normalize the data using 
#'        normalization parameter in gradDescent.data. Default value
#'        is TRUE, which means this function will normalize the input
#'        input data before predict calculation.
#'
#' @examples
#' ##################################
#' ## I. Predict Input data using gradient descent model
#' square_feet <- c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700)
#'
#' price <- c(245,312,279,308,199,219,405,324,319,255)
#'
#' house_price <- data.frame(square_feet, price)
#' 
#' ## do the preprocessing stage
#' house_price.data <- gradDescent.preprocessing(
#'       house_price,
#'       normalizationMethod="variance"
#'    )
#'
#' ## do the model building stage
#' GD.model <- gradDescent.learn(
#'       house_price.data,
#'       methodType="GD" 
#'    )
#'
#' ## create input data
#' input_square <- c(1500, 999, 1111)
#' inputData <- data.frame(input_square)
#'
#' ## predict input data
#' GD.predict <- gradDescent.predictInput(
#'       GD.model,
#'       house_price.data,
#'       inputData,
#'       normalize=TRUE
#'    )
#'
#' ##view predicted value
#' print(GD.predict$predictionData)
#'
#' @export

gradDescent.predictInput <- function(
	gradDescent.model, 
	gradDescent.data, 
	inputData,
	normalize=TRUE)
{
	#validate gradDescent objects
	if(!inherits(gradDescent.model, "gradDescent.model.object")){
		stop("not a legitimate gradDescent model object")
	}
	if(!inherits(gradDescent.data, "gradDescent.data.object")){
		stop("not a legitimate gradDescent data object")
	}

	#get theta
	theta <- gradDescent.model$theta

	#validate column length
	if (length(theta) != ncol(inputData)+1){
		stop("inputData column didn't match")
	}

	#bind inputData with output dummy (1)
	inputData <- cbind(inputData,1)

	#get normalizationParameter and normalizationMethod
	param <- gradDescent.data$normalizationParameter
	normalizationMethod <-  gradDescent.data$normalizationMethod

	#normalization inputData
	if(normalize){
		if(!is.null(normalizationMethod)){
			if(normalizationMethod == "variance"){
				# print("variance")
				inputData <- varianceScaling(inputData, param)
			}else if(normalizationMethod == "minmax"){
				# print("minmax")
				inputData <- minmaxScaling(inputData, param)
			}
		}
	}

	#clear output dummy
	inputData <- inputData[,1:(ncol(inputData)-1)]

	#add dummy input in matrix
	inputData <- cbind(1,inputData)
	inputData <- rbind(inputData,1) #dummy row for matrix issue purpose

	#transform inputData into matrix
	inputData <- matrix(unlist(inputData), ncol=ncol(inputData), byrow=FALSE)

	#predict inputData
	prediction <- inputData %*% t(theta)

	#merge inputData and prediction
	predictionData <- cbind(inputData[,2:ncol(inputData)], prediction)

	#transform into data.frame
	predictionData <- as.data.frame(predictionData)

	#remove predictionData dummy row
	predictionData <- predictionData[1:nrow(predictionData)-1,]

	#denormalization
	if(normalize){
		param <- gradDescent.data$normalizationParameter
		if(gradDescent.data$normalizationMethod == "variance"){
			predictionData <- varianceDescaling(predictionData, param)
		}else if(gradDescent.data$normalizationMethod == "minmax"){
			predictionData <- minmaxDescaling(predictionData, param)
		}
	}

	#create and set result list
	result <- list()
	result$predictionData <- predictionData

	#wrap result into class (object)
	class(result) <- "gradDescent.input.prediction.object"

	return(result)
}