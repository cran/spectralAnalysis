% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenericFunctions.R, R/objectLinking.R
\docType{methods}
\name{checkCompatible}
\alias{checkCompatible}
\alias{checkCompatible,SpectraInTime,ProcessTimes-method}
\alias{[ProcessTimes,SpectraInTime-method}
\alias{[SpectraInTime,ProcessTimes-method}
\alias{checkCompatible,ProcessTimes,SpectraInTime-method}
\title{Check whether 2 objects are compatible before using them together
For instance, same experiment name and matching time frames}
\usage{
checkCompatible(x, y, ...)

\S4method{checkCompatible}{SpectraInTime,ProcessTimes}(x, y)

\S4method{checkCompatible}{ProcessTimes,SpectraInTime}(x, y)
}
\arguments{
\item{x}{first object}

\item{y}{second object}

\item{...}{additional parameters}
}
\description{
Check whether 2 objects are compatible before using them together
For instance, same experiment name and matching time frames
}
