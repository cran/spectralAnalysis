% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectralNMF.R
\name{predictNNLS}
\alias{predictNNLS}
\title{Based on previously obtained NMF result \code{NMFResult}, estimate coefficients for a new 
spectralData object \code{object} using non-negative least squares fitting. The result is 
returned as as an NMF model.}
\usage{
predictNNLS(object, NMFResult)
}
\arguments{
\item{object}{\code{\link{SpectraInTime-class}}}

\item{NMFResult}{Fitted NMF model}
}
\value{
Fitted non-negative least squares result in the form of an NMF model
}
\description{
Based on previously obtained NMF result \code{NMFResult}, estimate coefficients for a new 
spectralData object \code{object} using non-negative least squares fitting. The result is 
returned as as an NMF model.
}
\author{
nsauwen
}
