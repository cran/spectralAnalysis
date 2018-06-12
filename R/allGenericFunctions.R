# Project: spectralAnalysis-R-package
# 
# Author: ablommaert
###############################################################################

#' @include internalHelpers.R
NULL



#' Get the first spectrum
#' 
#' @name firstSpectrum
#' @param ... additional parameters
#' @export
setGeneric( "firstSpectrum" , function( object , ... ) {
    standardGeneric( "firstSpectrum" ) 
  } 
)

#'  Get the last spectrum
#' 
#' @name lastSpectrum
#' @param ... additional parameters
#' @export
setGeneric( "lastSpectrum" , function( object  , ... ) {
    standardGeneric( "lastSpectrum" ) 
  } 
)




#' Check whether 2 objects are compatible before using them together
#' For instance, same experiment name and matching time frames 
#' 
#' @param x first object
#' @param y second object
#' @param ... additional parameters 
#' @export
setGeneric( "checkCompatible" , function( x , y , ... ) {
    stop(" no compatibility check defined for these objects check defined for these object types")
  }
)

#' Time align first object, using info in the second object
#' 
#' @param x and S4 object to be aligned
#' @param y object to use time information from
#' @param ... additional arguments 
#' @export 
setGeneric( "timeAlign" , function( x , y , ... ) {
    stop(" no timeAlign function defined for these objects ")
  } )

#' Align SpectraInTime objects with differing wavelength axes to a common wavelength axis 
#' using cubic spline interpolation.
#' @export 
setGeneric( "wavelengthAlign" , function( ref , toAlign ) {
			stop(" no wavelengthAlign function defined for these objects ")
		} )


### getters


#' generic function to extract \code{range}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getRange
#' @export
setGeneric( "getRange", function( object, ... ) {
    standardGeneric ( "getRange" )
  } 
)

#' generic function to extract \code{elements}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getElements
#' @export
setGeneric( "getElements", function( object, ... ) {
    standardGeneric ( "getElements" )
  } 
)


#' generic function to extract \code{spectra}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getSpectra
#' @export
setGeneric( "getSpectra", function( object, ... ) {
    standardGeneric ( "getSpectra" )
  } 
)

#' generic function to extract \code{timePoints}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getTimePoints
#' @export
setGeneric( "getTimePoints", function( object, ... ) {
    standardGeneric ( "getTimePoints" )
  } 
)

#' generic function to extract \code{experimentName}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getExperimentName
#' @export
setGeneric( "getExperimentName", function( object, ... ) {
    standardGeneric ( "getExperimentName" )
  } 
)

#' generic function to extract \code{wavelengths}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getWavelengths 
#' @export
setGeneric( "getWavelengths", function( object, ... ) {
    standardGeneric ( "getWavelengths" )
  } 
)

#' generic function to extract \code{extraInfo}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name   getExtraInfo
#' @export
setGeneric( "getExtraInfo", function( object, ... ) {
    standardGeneric ( "getExtraInfo" )
  } 
)

#' generic function to extract \code{startTime}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getStartTime 
#' @export
setGeneric( "getStartTime", function( object, ... ) {
    standardGeneric ( "getStartTime" )
  } 
)

#' generic function to extract \code{units}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getUnits 
#' @export
setGeneric( "getUnits", function( object, ... ) {
    standardGeneric ( "getUnits" )
  } 
)


#' generic function to extract \code{preprocessing}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  getPreprocessing 
#' @export
setGeneric( "getPreprocessing", function( object, ... ) {
    standardGeneric ( "getPreprocessing" )
  } 
)


### setters
#' set the experiment name
#' 
#' @param object a S4 class object
#' @param value a vector of time points
#' @usage setExperimentName(object) <- value
#' @docType methods
#' @rdname setExperimentName
#' @export setExperimentName<-
setGeneric( "setExperimentName<-", function( object, value ) {
    standardGeneric("setExperimentName<-")
  } 
)

#'set time alternative time axis
#' 
#' @param object a S4 class object
#' @param value a vector of time points
#' @usage setTimePointsAlt(object) <- value
#' @docType methods
#' @rdname setTimePointsAlt
#' @export setTimePointsAlt<-
setGeneric( "setTimePointsAlt<-", function( object, value ) {
    standardGeneric("setTimePointsAlt<-")
  } 
)



### preprocessing related generic functions 


#' generic normalization function 
#' 
#' @param object a S4 class object
#' @param ... additional parameters
#' @docType methods
#' @name  normalize 
#' @export
setGeneric( "normalize", function( object,  ... ) {
      standardGeneric ( "normalize" )
    } 
)


#' generic smoothing function
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name  smooth  
#' @export
setGeneric( "smooth", function( object,  ... ) {
      standardGeneric ( "smooth" )
    } 
)


#' generic function to perfom baseline correction
#' 
#' @param object a S4 class object
#' @docType methods
#' @name baselineCorrect 
#' @export
setGeneric( "baselineCorrect", function( object,  ... ) {
      standardGeneric ( "baselineCorrect" )
    } 
)



#' generic function to preprocess an S4 object
#' 
#' @param object a S4 class object
#' @param with an other object containing preprocessing information: other S4 object, list or expression 
#' @docType methods
#' @name preprocess 
#' @export
setGeneric( "preprocess" , function( object , with ) {
      standardGeneric( "preprocess" )
    }
)










