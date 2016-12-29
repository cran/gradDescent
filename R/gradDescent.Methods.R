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
#' This is internal function of learning stage that implement batch Gradient 
#' Descent method to create model.
#'
#' @title Gradient Descent learn
#'
#' @param inputData a matrix of input data that created inside gradDescent.learn
#'        function.
#' @param outputData a matrix of output data that created inside gradDescent.learn
#'        function.
#' @param list a list of parameter that customize the learn.
#'        
#' \itemize{
#' \item \code{rowLength}: a integer of data length (row).
#' \item \code{theta}: a matrix of float number of current model value.
#' \item \code{alpha} : a float value for learning rate.
#' \item \code{momentum} : a float value to give a constant speed to learning process..
#' \item \code{smooth} : a float value to handle zero division issue in certain learning method.
#' \item \code{stochastic} : a boolean value to enable stochastic, which mean to select one random
#'                    value in data train, instead process all data train. 
#' \item \code{accelerate} : a boolean value to enable accelerate in the learning with momentum.
#' \item \code{maxIter} : Adaptive Moment Estimation method to calculate gradient.
#' }
#' @return a matrix of theta
#' @references
#' A. Cotter, O. Shamir, N. Srebro, K. Sridharan. (2011). Better Mini-Batch Algorithm via
#'     Accelerated Gradien Methods. NIPS.
#' L. Buttou. (2016). Large-Scale Machine Learning with Stochastic Gradient Descent.
#'     COMPSTAT'2010 (pp.17-186). NEC Labs America, Princeton NJ 088542, USA.
#' N. Qian. (1999). On the momentum term in gradient descent learning algorithms.
#'     Neural networks: the official journal of the International Neural Network Society,
#'     145-151.
#' Y. Nesterov. (1983). A method for unconstrained convex minimization problem with the rate of
#'     convergence O (1/k2). Soviet Mathematic Doklady 27 (2), 543-547.
#' @export

gradientDescent <- function(
	inputData,
	outputData,
	list)
{

	rowLength  <- list$rowLength 	
	theta <- list$theta	
	alpha <- list$alpha
	momentum <- list$momentum
	smooth <- list$smooth
	stochastic <- list$stochastic 
	accelerate <- list$accelerate
	maxIter <- list$maxIter
	delay <- list$delay
	seed <- list$seed

	#temporary variables
	temporaryTheta <- matrix(ncol=length(theta), nrow=1)
	updateRule <- matrix(0, ncol=length(theta), nrow=1)

	#loop the gradient descent
	for(iteration in 1:maxIter){

		#create random integer for stochastic
		randomRow <- sample(1:rowLength , 1)

		#check accelerate
		if(accelerate){
			theta <- theta - (updateRule * momentum)
		}

		#check stochastic
		if(stochastic){
			#stochastic compute error
			error <- (inputData[randomRow,] %*% t(theta)) - outputData[randomRow]
		}else{
			#compute error
			error <- (inputData %*% t(theta)) - outputData
		}

		#loop the theta/column
		for(column in 1:length(theta)){

			#check stochastic
			if(stochastic){
				term <- error * inputData[randomRow, column]
				rowLength  <- 1
			}else{
				term <- error * inputData[,column]
			}

			#calculate gradient
			gradient <- sum(term) / rowLength 

			updateRule[1,column] <- (momentum*updateRule[1,column]) + (alpha*gradient)
			#update single theta
			temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
		}

		#update all theta in the current iteration
		theta <- temporaryTheta
		Sys.sleep(delay)
	}

	result <- theta
	return(result)
}

#' This is internal function of learning stage that implement Stochastic Average Gradient 
#' Descent method to create model.
#'
#' @title Stochastic Average Gradient Descent learn
#'
#' @param inputData a matrix of input data that created inside gradDescent.learn
#'        function.
#' @param outputData a matrix of output data that created inside gradDescent.learn
#'        function.
#' @param list a list of parameter that customize the learn.
#'        
#' \itemize{
#' \item \code{rowLength}: a integer of data length (row).
#' \item \code{theta}: a matrix of float number of current model value.
#' \item \code{alpha} : a float value for learning rate.
#' \item \code{momentum} : a float value to give a constant speed to learning process..
#' \item \code{smooth} : a float value to handle zero division issue in certain learning method.
#' \item \code{stochastic} : a boolean value to enable stochastic, which mean to select one random
#'                    value in data train, instead process all data train. 
#' \item \code{accelerate} : a boolean value to enable accelerate in the learning with momentum.
#' \item \code{maxIter} : Adaptive Moment Estimation method to calculate gradient.
#' }
#' @return a matrix of theta
#' @references
#' M. Schmidt, N. Le Roux, F. Bach. (2013). Minimizing Finite Sums with the Stochastic Average Gradient
#' Paris, France: INRIA-SIERRA Project - Team Departement d'informatique de l'Ecole Normale Superieure
#' 
#' @export

stochasticAverage <- function(
	inputData,
	outputData,
	list)
{

	rowLength  <- list$rowLength 	
	theta <- list$theta	
	alpha <- list$alpha
	momentum <- list$momentum
	smooth <- list$smooth
	stochastic <- list$stochastic 
	accelerate <- list$accelerate
	maxIter <- list$maxIter
	delay <- list$delay

	#temporary variables
	temporaryTheta <- matrix(ncol=length(theta), nrow=1)
	updateRule <- matrix(0, ncol=length(theta), nrow=1)

	#loop the gradient descent
	for(iteration in 1:maxIter){

		#create random integer for stochastic
		randomRow <- sample(1:rowLength , 1)

		#check accelerate
		if(accelerate){
			theta <- theta - (updateRule * momentum)
		}

		#check stochastic
		if(stochastic){
			#stochastic compute error
			error <- (inputData[randomRow,] %*% t(theta)) - outputData[randomRow]
		}else{
			#compute error
			error <- (inputData %*% t(theta)) - outputData
		}

		#loop the theta/column
		for(column in 1:length(theta)){

			#check stochastic
			if(stochastic){
				term <- error * inputData[randomRow, column]
				rowLength  <- 1
			}else{
				term <- error * inputData[,column]
			}

			#calculate gradient
			gradient <- sum(term) / rowLength 

			updateRule[1,column] <- (momentum*updateRule[1,column]) + (alpha*gradient)
			#update single theta
			temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
		}

		#stochastic average randomization
		if(sample(0:1,1) == 1){
			#update all theta in the current iteration
			theta <- temporaryTheta
		}

		Sys.sleep(delay)
	}

	result <- theta
	return(result)
}

#' This is internal function of learning stage that implement Adaptive Subgradient 
#' method to create model.
#'
#' @title Adaptive Subgradient learn
#'
#' @param inputData a matrix of input data that created inside gradDescent.learn
#'        function.
#' @param outputData a matrix of output data that created inside gradDescent.learn
#'        function.
#' @param list a list of parameter that customize the learn.
#'        
#' \itemize{
#' \item \code{rowLength}: a integer of data length (row).
#' \item \code{theta}: a matrix of float number of current model value.
#' \item \code{alpha} : a float value for learning rate.
#' \item \code{momentum} : a float value to give a constant speed to learning process..
#' \item \code{smooth} : a float value to handle zero division issue in certain learning method.
#' \item \code{stochastic} : a boolean value to enable stochastic, which mean to select one random
#'                    value in data train, instead process all data train. 
#' \item \code{accelerate} : a boolean value to enable accelerate in the learning with momentum.
#' \item \code{maxIter} : Adaptive Moment Estimation method to calculate gradient.
#' }
#' @return a matrix of theta
#' @references
#' J. Duchi, E. Hazan, Y. Singer. (2011). Adaptive Subgradient Methods for Online Learning and Stochastic Optimization
#' Journal of Machine Learning Research 12, 2121-2159.
#' 
#' @export

adagrad <- function(
	inputData,
	outputData,
	list)
{
	rowLength  <- list$rowLength 	
	theta <- list$theta	
	alpha <- list$alpha
	momentum <- list$momentum
	smooth <- list$smooth
	stochastic <- list$stochastic 
	accelerate <- list$accelerate
	maxIter <- list$maxIter
	delay <- list$delay

	#temporary variables
	temporaryTheta <- matrix(ncol=length(theta), nrow=1)
	updateRule <- matrix(0, ncol=length(theta), nrow=1)

	#adagrad temporary variables
	gradientList <- matrix(nrow=1, ncol=0)

	#loop the adadelta gradient descent
	for(iteration in 1:maxIter){
		#create random integer for stochastic
		randomRow <- sample(1:rowLength, 1)

		#check accelerate
		if(accelerate){
			theta <- theta - (updateRule * momentum)
		}

		#check stochastic
		if(stochastic){
			#stochastic compute error
			error <- (inputData[randomRow,] %*% t(theta)) - outputData[randomRow]
		}else{
			#compute error
			error <- (inputData %*% t(theta)) - outputData
		}

		#loop the theta/column
		for(column in 1:length(theta)){
			#check stochastic
			if(stochastic){
				term <- error * inputData[randomRow, column]
				rowLength <- 1
			}else{
				term <- error * inputData[,column]
			}

			#calculate gradient
			gradient <- sum(term) / rowLength

			#adagrad update rule calculation
			gradientList <- cbind(gradientList, gradient)
			gradientSum <- sqrt(gradientList %*% t(gradientList))
			updateRule[1,column] <- (alpha / gradientSum) * gradient

			temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
		}

		#update all theta in the current iteration
		theta <- temporaryTheta
		Sys.sleep(delay)
	}


	result <- theta
	return(result)
}

#' This is internal function of learning stage that implement Adadelta 
#' method to create model.
#'
#' @title Adadelta learn
#'
#' @param inputData a matrix of input data that created inside gradDescent.learn
#'        function.
#' @param outputData a matrix of output data that created inside gradDescent.learn
#'        function.
#' @param list a list of parameter that customize the learn.
#'        
#' \itemize{
#' \item \code{rowLength}: a integer of data length (row).
#' \item \code{theta}: a matrix of float number of current model value.
#' \item \code{alpha} : a float value for learning rate.
#' \item \code{momentum} : a float value to give a constant speed to learning process..
#' \item \code{smooth} : a float value to handle zero division issue in certain learning method.
#' \item \code{stochastic} : a boolean value to enable stochastic, which mean to select one random
#'                    value in data train, instead process all data train. 
#' \item \code{accelerate} : a boolean value to enable accelerate in the learning with momentum.
#' \item \code{maxIter} : Adaptive Moment Estimation method to calculate gradient.
#' }
#' @return a matrix of theta
#' @references
#' M. Zeiler. (2012). Adadelta: An Adaptive Learning Rate Method
#' arXiv: 1212.5701v1, 1-6.
#' @export

adadelta <- function(
	inputData,
	outputData,
	list)
{

	rowLength  <- list$rowLength 	
	theta <- list$theta	
	alpha <- list$alpha
	momentum <- list$momentum
	smooth <- list$smooth
	stochastic <- list$stochastic 
	accelerate <- list$accelerate
	maxIter <- list$maxIter
	delay <- list$delay

	#temporary variables
	temporaryTheta <- matrix(ncol=length(theta), nrow=1)
	updateRule <- matrix(0, ncol=length(theta), nrow=1)

	#set adadelta temporary variables
	ESG <- 0
	ESR <- 0
	RMSUpdate <- 0
	
	#loop the adadelta gradient descent
	for(iteration in 1:maxIter){
		#create random integer for stochastic
		randomRow <- sample(1:rowLength, 1)

		#check accelerate
		if(accelerate){
			theta <- theta - (updateRule * momentum)
		}

		#check stochastic
		if(stochastic){
			#stochastic compute error
			error <- (inputData[randomRow,] %*% t(theta)) - outputData[randomRow]
		}else{
			#compute error
			error <- (inputData %*% t(theta)) - outputData
		}

		#loop the theta/column
		for(column in 1:length(theta)){
			#check stochastic
			if(stochastic){
				term <- error * inputData[randomRow, column]
				rowLength <- 1
			}else{
				term <- error * inputData[,column]
			}

			#calculate gradient
			gradient <- sum(term) / rowLength

			#adadelta update rule calculation
			ESG <- (momentum*ESG) + (1-momentum)*gradient^2
			RMSGradient <- sqrt(ESG + smooth)
			ESR <- (momentum*ESR) + (1-momentum)*updateRule[1,column]^2
			updateRule[1,column] <- (RMSUpdate / RMSGradient) * gradient

			#temporary change
			temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]

			#adadelta temporary change
			RMSUpdate <- sqrt(ESR + smooth)
		}

		#update all theta in the current iteration
		theta <- temporaryTheta
		Sys.sleep(delay)
	}

	result <- theta
	return(result)
}

#' This is internal function of learning stage that implement RMSprop 
#' method to create model.
#'
#' @title RMSprop learn
#'
#' @param inputData a matrix of input data that created inside gradDescent.learn
#'        function.
#' @param outputData a matrix of output data that created inside gradDescent.learn
#'        function.
#' @param list a list of parameter that customize the learn.
#'        
#' \itemize{
#' \item \code{rowLength}: a integer of data length (row).
#' \item \code{theta}: a matrix of float number of current model value.
#' \item \code{alpha} : a float value for learning rate.
#' \item \code{momentum} : a float value to give a constant speed to learning process..
#' \item \code{smooth} : a float value to handle zero division issue in certain learning method.
#' \item \code{stochastic} : a boolean value to enable stochastic, which mean to select one random
#'                    value in data train, instead process all data train. 
#' \item \code{accelerate} : a boolean value to enable accelerate in the learning with momentum.
#' \item \code{maxIter} : Adaptive Moment Estimation method to calculate gradient.
#' }
#' @return a matrix of theta
#' @references
#' M. Zeiler. (2012). Adadelta: An Adaptive Learning Rate Method
#' arXiv: 1212.5701v1, 1-6.
#'
#' @export

rmsprop <- function(
	inputData,
	outputData,
	list)
{

	rowLength  <- list$rowLength 	
	theta <- list$theta	
	alpha <- list$alpha
	momentum <- list$momentum
	smooth <- list$smooth
	stochastic <- list$stochastic 
	accelerate <- list$accelerate
	maxIter <- list$maxIter
	delay <- list$delay

	#temporary variables
	temporaryTheta <- matrix(ncol=length(theta), nrow=1)
	updateRule <- matrix(0, ncol=length(theta), nrow=1)

	#set rmsprop temporary variables
	ESG <- 0
	
	#loop the adadelta gradient descent
	for(iteration in 1:maxIter){
		#create random integer for stochastic
		randomRow <- sample(1:rowLength, 1)

		#check accelerate
		if(accelerate){
			theta <- theta - (updateRule * momentum)
		}

		#check stochastic
		if(stochastic){
			#stochastic compute error
			error <- (inputData[randomRow,] %*% t(theta)) - outputData[randomRow]
		}else{
			#compute error
			error <- (inputData %*% t(theta)) - outputData
		}

		#loop the theta/column
		for(column in 1:length(theta)){
			#check stochastic
			if(stochastic){
				term <- error * inputData[randomRow, column]
				rowLength <- 1
			}else{
				term <- error * inputData[,column]
			}

			#calculate gradient
			gradient <- sum(term) / rowLength

			#rmsprop update rule calculation
			ESG <- (momentum*ESG) + (1-momentum)*gradient^2
			RMSGradient <- sqrt(ESG + smooth)
			updateRule[1,column] <- (alpha / RMSGradient) * gradient

			#temporary change
			temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]

		}

		#update all theta in the current iteration
		theta <- temporaryTheta
		Sys.sleep(delay)
	}

	result <- theta
	return(result)
}

#' This is internal function of learning stage that implement Adaptive Moment 
#' Estimation method to create model.
#'
#' @title Adaptive Moment Estimation learn
#'
#' @param inputData a matrix of input data that created inside gradDescent.learn
#'        function.
#' @param outputData a matrix of output data that created inside gradDescent.learn
#'        function.
#' @param list a list of parameter that customize the learn.
#'        
#' \itemize{
#' \item \code{rowLength}: a integer of data length (row).
#' \item \code{theta}: a matrix of float number of current model value.
#' \item \code{alpha} : a float value for learning rate.
#' \item \code{momentum} : a float value to give a constant speed to learning process..
#' \item \code{smooth} : a float value to handle zero division issue in certain learning method.
#' \item \code{stochastic} : a boolean value to enable stochastic, which mean to select one random
#'                    value in data train, instead process all data train. 
#' \item \code{accelerate} : a boolean value to enable accelerate in the learning with momentum.
#' \item \code{maxIter} : Adaptive Moment Estimation method to calculate gradient.
#' }
#' @return a matrix of theta
#' @references
#' D. Kingman, J.L. Ba. (2015). Adam: a method for stochastic optimization.
#' International Conference on Learning Representation.
#' 
#' @export

adam <- function(
	inputData,
	outputData,
	list)
{

	rowLength  <- list$rowLength 	
	theta <- list$theta	
	alpha <- list$alpha
	momentum <- list$momentum
	smooth <- list$smooth
	stochastic <- list$stochastic 
	accelerate <- list$accelerate
	maxIter <- list$maxIter
	delay <- list$delay

	#temporary variables
	temporaryTheta <- matrix(ncol=length(theta), nrow=1)
	updateRule <- matrix(0, ncol=length(theta), nrow=1)

	#set adam exclusive variables and temporary variables
	beta1 <- 0.9
	beta2 <- 0.999
	meanMoment <- 0
	varianceMoment <- 0

	#loop the adadelta gradient descent
	for(iteration in 1:maxIter){
		#create random integer for stochastic
		randomRow <- sample(1:rowLength, 1)

		#check accelerate
		if(accelerate){
			theta <- theta - (updateRule * momentum)
		}

		#check stochastic
		if(stochastic){
			#stochastic compute error
			error <- (inputData[randomRow,] %*% t(theta)) - outputData[randomRow]
		}else{
			#compute error
			error <- (inputData %*% t(theta)) - outputData
		}

		#loop the theta/column
		for(column in 1:length(theta)){
			#check stochastic
			if(stochastic){
				term <- error * inputData[randomRow, column]
				rowLength <- 1
			}else{
				term <- error * inputData[,column]
			}

			#calculate gradient
			gradient <- sum(term) / rowLength

			#adam update rule calculation
			meanMoment <- (beta1*meanMoment) + (1-beta1)*gradient
			varianceMoment <- (beta2*varianceMoment) + (1-beta2)*(gradient^2)

			mean.hat <- meanMoment/(1-beta1)
			variance.hat <- varianceMoment/(1-beta2)

			updateRule[1,column] <- (alpha/(sqrt(variance.hat)+smooth)) * mean.hat

			#temporary change
			temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
		}

		#update all theta in the current iteration
		theta <- temporaryTheta
		Sys.sleep(delay)
	}

	result <- theta
	return(result)
}