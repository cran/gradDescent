% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gradDescentR.Methods.R
\name{MGD}
\alias{MGD}
\title{Momentum Gradient Descent (MGD) Method Learning Function}
\usage{
MGD(dataTrain, alpha = 0.1, maxIter = 10, momentum = 0.9, seed = NULL)
}
\arguments{
\item{dataTrain}{a data.frame that representing training data (\eqn{m \times n}),
where \eqn{m} is the number of instances and \eqn{n} is the number
of variables where the last column is the output variable. dataTrain
must have at least two columns and ten rows of data that contain
only numbers (integer or float).}

\item{alpha}{a float value representing learning rate. Default value is 0.1}

\item{maxIter}{the maximal number of iterations.}

\item{momentum}{a float value represent momentum give a constant speed to learning process.}

\item{seed}{a integer value for static random. Default value is NULL, which means
the function will not do static random.}
}
\value{
a vector matrix of theta (coefficient) for linear model.
}
\description{
A function to build prediction model using Momentum Gradient Descent (MGD) method.
}
\details{
This function based on \code{\link{SGD}} with an optimization to speed-up the learning
by adding a constant momentum.
}
\examples{
##################################
## Learning and Build Model with MGD
## load R Package data
data(gradDescentRData)
## get z-factor data
dataSet <- gradDescentRData$CompressilbilityFactor
## split dataset
splitedDataSet <- splitData(dataSet)
## build model with MGD
MGDmodel <- MGD(splitedDataSet$dataTrain)
#show result
print(MGDmodel)

}
\references{
N. Qian
On the momentum term in gradient descent learning algorithms.,
Neural networks : the official journal of the International Neural Network Society,
pp. 145-151- (1999)
}
\seealso{
\code{\link{AGD}}
}
