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

getMeanStd <- function(dataSet){
	#create result matrix
	result <- matrix(dataSet, nrow = 2, ncol(dataSet))

	#loop the data column
	for(column in 1:ncol(dataSet)) {
		result[1 , column] <- mean(unlist(dataSet[column]))
		result[2 , column] <- sd(unlist(dataSet[column]))
	}
	
	return (result)
}

getMinMax <- function(dataSet){
	#create result matrix
	result <- matrix(dataSet, nrow = 2, ncol(dataSet))

	#loop the data column
	for(column in 1:ncol(dataSet)) {
		result[1 , column] <- min(unlist(dataSet[column]))
		result[2 , column] <- max(unlist(dataSet[column]))
	}
	
	return (result)
}

varianceScaling <- function(dataSet, varianceParameter){

  columnLength <- ncol(dataSet)

  meanValue <- varianceParameter[1,]
  stdValue <- varianceParameter[2,]
  
  result <- (dataSet-meanValue)/stdValue

  return(result)
}

varianceDescaling <- function(dataSet, varianceParameter){

  columnLength <- ncol(dataSet)

  meanValue <- varianceParameter[1,]
  stdValue <- varianceParameter[2,]
  
  result <- (dataSet * stdValue) + meanValue

  return(result)
}

minmaxScaling <- function(dataSet, minmaxParameter){

  columnLength <- ncol(dataSet)

  minValue <- minmaxParameter[1,]
  maxValue <- minmaxParameter[2,]
  
  result <- (dataSet-minValue)/(maxValue-minValue)

  return(result)
}

minmaxDescaling <- function(dataSet, minmaxParameter){

  columnLength <- ncol(dataSet)

  minValue <- minmaxParameter[1,]
  maxValue <- minmaxParameter[2,]
  
  result <- (dataSet * (maxValue - minValue)) + (minValue)

  return(result)
}

getTheta <- function(columnLength, minTheta=0, maxTheta=1, seed=NULL){
	#create static random
	set.seed(seed)
	#random a value
	thetaList <- runif(columnLength, min=minTheta, max=maxTheta)
	#clear static random
	set.seed(seed)
	#transform into matrix
	result <- matrix(unlist(thetaList), ncol=columnLength, nrow=1, byrow=FALSE)

	return(result)
}

mseCalculation <- function(output.data, prediction){
	#get data length
	rowLength <- nrow(cbind(output.data, prediction))
	#calculate mean-squared-error
	result <- (output.data - prediction)^2
	result <- sum(result) / (rowLength)
	
	return(result)
}

rmseCalculation <- function(output.data, prediction){
	#get data length
	rowLength <- nrow(cbind(output.data, prediction))
	#calculate root-mean-squared-error
	result <- (output.data - prediction)^2
	result <- sqrt(sum(result)) / (rowLength)
	
	return(result)
}