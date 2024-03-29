% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveSpectraInTime.R
\name{saveSpectra}
\alias{saveSpectra}
\alias{readSpectra}
\alias{save}
\alias{read}
\title{read or save a \code{\link{SpectraInTime-class}} from or to a  \code{.txt} file}
\usage{
saveSpectra(object, directory, precision = 32)

readSpectra(file)
}
\arguments{
\item{object}{object to save}

\item{directory}{directory to save object}

\item{precision}{number of significant digits controlling precission}

\item{file}{to be read}
}
\value{
the path to which the file is saved

\code{\link{SpectraInTime-class}}
}
\description{
read or save a \code{\link{SpectraInTime-class}} from or to a  \code{.txt} file
}
\note{
experiment name is used to save the experiment

default time formats are assumed to convert to \code{\link{SpectraInTime-class}}

some data precession is lost because of internal conversion to JSON format
}
\examples{
  \dontshow{ directory       <-  tempdir() } 
 spectra         <-  getSpectraInTimeExample()
 saveSpectra(  spectra , directory )
 experimentName  <-  getExperimentName( spectra )
 file            <-  file.path( directory , paste0( experimentName , ".txt")  )
 spectraRead     <-  readSpectra( file )
 \dontshow{ unlink( directory ) }
}
\author{
Adriaan Blommaert
}
