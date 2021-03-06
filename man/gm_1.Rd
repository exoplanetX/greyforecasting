% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gm_1.R
\name{gm_1}
\alias{gm_1}
\title{Improved grey forecasting model with optimal background values}
\usage{
gm_1(y, ntest = NULL, term = 1, buff = NULL, alpha = NA)
}
\arguments{
\item{buff}{buffer operator used for original data.}

\item{alpha}{coefficient in buffer operator if used.}

\item{x}{data sequence.}

\item{present}{character vector containing xlab and ylab.}
}
\description{
weighted background gm model and solved by auxillary parameters
}
\examples{
g<-gm_1(y,term=3)
}
\references{
XU Ning, DANG Yao-guo, DING Song. Optimization method of background value in GM(1,1) model based on least error[J]. Control and Decision, 2015,30(12).
}
