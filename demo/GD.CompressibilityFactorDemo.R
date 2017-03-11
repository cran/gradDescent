#load package
library(gradDescent)

#load data
data("gradDescentRData")

#preprocess data
cf.data <- gradDescent.preprocessing(
	gradDescentRData$CompressilbilityFactor,
	trainRate=0.8, 
	normalizationMethod="variance", 
	seed=1
	)

#model building / data learning
GD10.model <- gradDescent.learn(
	cf.data, 
	alpha=0.1, 
	methodType="GD", 
	maxIter=10, 
	seed=1
)

#predicting
GD10.prediction <- gradDescent.predict(GD10.model, cf.data)

#get result
GD10.prediction$predictionData
GD10.prediction$mse
GD10.prediction$rmse
