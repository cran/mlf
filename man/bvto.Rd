% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bvto.R
\name{bvto}
\alias{bvto}
\title{Bias-Variance Trade-Off}
\usage{
bvto(truth, estimate)
}
\arguments{
\item{truth}{test data vector or baseline accuractruth to test against.}

\item{estimate}{predicted vector}
}
\description{
Provides estimated error decomposition from model predictions (mse, bias, variance).
}
\examples{
# Sample data
test <- rnorm(25, 80, 35)
predicted <- rnorm(25, 80, 50)

mlf::bvto(test, predicted)
}
