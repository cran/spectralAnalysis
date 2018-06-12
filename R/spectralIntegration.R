# Integrate spectral data over a wavelength range
# 
# Author: Robin Van OirBeek , Nicolas Sauwen , Adriaan Blommaert (order of making adaptations )
#
# Changes: 
#      * time selection no longer inside integration 
#      * integrationFrame no longer used, just use direct integration , double function not needed 
#
###############################################################################

#' @include internalHelpers.R objectSpectraInTime.R
NULL


if( 0 == 1 ){
  # developp + debug 
  object                              <-  getSpectraInTimeExample()
  wavelenghtRange                     <-  c( 170 , 200 )
  smoothingValue                      <-  0
}

#' Integrate spectraInTime object 
#'
#' The integrated value over a user-specified wavelength range is calculated (trapezium rule) per time point, afterwards smoothing over time can be applied 
#' @param object \code{\link{SpectraInTime-class}}
#' @param wavelenghtRange numeric vector of 2 elements i.e. integration limits
#' @param smoothingValue numeric value between 0 and 1, amount of code{\link[stats]{lowess}}-smoothing,
#'  default to \code{0} i.e no smoothing. Note that smoothing is applied  after integration
#' @param timeUnit character value, choose between: \code{second , minutes and hours}, defaults to 
#'  \code{seconds}
#' @return \code{data.frame} with variables \code{time} and \code{integratedValue}
#' @examples 
#' spectra                   <-  getSpectraInTimeExample()
#' defaults                  <-  spectralIntegration( spectra , c(200 , 300) , timeUnit = "hours" )
#' unsmoothedTrend           <-  spectralIntegration( spectra , c(200 , 300) , timeUnit = "hours" )
#' smoothedTrend             <-  spectralIntegration( spectra , c(200 , 300) ,
#'   smoothingValue = 0.5 , timeUnit = "hours" ) 
#' @importFrom methods new
#' @importFrom stats lowess
#' @export
spectralIntegration                    <- function( object, wavelenghtRange , smoothingValue = 0 , timeUnit = "seconds"  ) {
  ## input checking
  checkSmoothing                       <-  is.numeric( smoothingValue ) && ( smoothingValue  >= 0 )  &&  ( smoothingValue <= 1 ) 
  if( !checkSmoothing ) {
    stop( "'smoothingValue' should be a numeric value between 0 and 1")
  }
  
  ## subset on wavelengthRange   
  spectraSelect                        <-  object[ , r( wavelenghtRange ) ]
  ## extract elements
  times                                <-  getTimePoints( spectraSelect , timeUnit = timeUnit)
  wavelengths                          <-  getWavelengths( spectraSelect )
  pureSpectra                          <-  getSpectra( spectraSelect )
  TIMEMARGIN                           <-  1
  ## integrate
  integratedValues                     <-  apply( pureSpectra , TIMEMARGIN ,  "trapz" ,   x = wavelengths )
  ## smooth
  if(smoothingValue == 0 ) {
    integratedValuesSmooth             <-  integratedValues
  } else {
    integratedValuesSmooth             <-  lowess( times , integratedValues , f = smoothingValue )$y
  }
  
  data.frame( time = times , value = integratedValuesSmooth )
}



#' integration via trapezium ruls originally from caTools package, included to avoid dependency on old package
#' with correction inverse scale, should give still possitive values (taken absolute values )
#' 
#' @keywords internal
trapz                         <-  function( x , y ) { 

    
  idx = 2:length(x)
  return (as.double( abs(x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
}

