% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gradDescentR.Methods.R
\name{SSGD}
\alias{SSGD}
\title{Semi Stochastic Gradient Descent (SSGD) Method Learning Function}
\usage{
SSGD(dataTrain, alpha = 0.1, maxIter = 10, lamda = 0, innerIter = 10,
  seed = NULL)
}
\arguments{
\item{dataTrain}{a data.frame that representing training data (\eqn{m \times n}),
where \eqn{m} is the number of instances and \eqn{n} is the number
of variables where the last column is the output variable. dataTrain
must have at least two columns and ten rows of data that contain
only numbers (integer or float).}

\item{alpha}{a float value representing learning rate. Default value is 0.1}

\item{maxIter}{the maximal number of iterations in outerloop.}

\item{lamda}{a float value to generate random value from innerIter with probability for innerloop.}

\item{innerIter}{the maximal number of iterations in innerloop.}

\item{seed}{a integer value for static random. Default value is NULL, which means
the function will not do static random.}
}
\value{
a vector matrix of theta (coefficient) for linear model.
}
\description{
A function to build prediction model using SSGD method.
}
\details{
This function combines elements from both \code{\link{GD}} and \code{\link{SGD}}. \code{\link{SSGD}} starts by
computing the full gradient once and then proceeds with stochastic updates by choosing one of
the gradients at a time.
}
\examples{
##################################
## Learning and Build Model with SSGD
## load R Package data
data(gradDescentRData)
## get z-factor data
dataSet <- gradDescentRData$CompressilbilityFactor
## split dataset
splitedDataSet <- splitData(dataSet)
## build model with SSGD
SSGDmodel <- SSGD(splitedDataSet$dataTrain)
#show result
print(SSGDmodel)

}
\references{
George Papamakarios
Comparison of Modern Stochastic Optimization Algorithms,
(2014)
}
\seealso{
\code{\link{SVRG}}, \code{\link{SARAH}}, \code{\link{SARAHPlus}}
}
