# Project: spectralAnalysis-R-package
# 
# Author: Adriaan Blommaert
# Aim: simple functions and methods envolving more than 1 S4 object
###############################################################################


#' @include allGenericFunctions.R objectProcessTimes.R objectSpectraInTime.R 
NULL



### compatibility checking "checkCompatible" similar to 'validObject' but for 2 objects matching each other

if( 0 == 1 ) {
  ## building 
  x  = getSpectraInTimeExample()
  y  = getProcessTimesExample()
  
  str(x)
  str(y)
  
  ## testing of error messages
     # return TRUE 
  checkCompatible(  x , y)
  checkCompatible( y , x )
  
    
    # wrong experiment name 
  xNameDif            <-  x
  xNameDif@experimentName <-  "verkeerde naam"
  checkCompatible( xNameDif , y )
  
  # wrong startime  name 
  xTimeDiff            <-  x
  xTimeDiff@startTime  <-  getStartTime( x ) + 100000
  checkCompatible( xTimeDiff , y )
  
  # NA start time: warning instead of error
  
  xTimeNA            <-  x
  xTimeNA@startTime  <-  getStartTime( x ) + NA
  checkCompatible( xTimeNA , y )
  
}

#' @rdname checkCompatible
#' @aliases [ProcessTimes,SpectraInTime-method [SpectraInTime,ProcessTimes-method
setMethod( "checkCompatible" , 
  signature = c( x = "SpectraInTime" , y = "ProcessTimes" ) , definition = function( x , y ){
    errors                             <-  character()
    
    ## checks
    checkEqualExperiment               <-  x@experimentName == y@experimentName 
    if( !checkEqualExperiment ){
      errors                           <-  addMessage( errors , "Unequal experiment names" )
    }
    checksExperimentStartForProcessStart  <-  x@startTime < y@timeHeatingAboveMin
   if( is.na( checksExperimentStartForProcessStart ) ) {
     warning( "times of objects of class 'SpectraInTime' , 'ProcessTimes' cannot be compared because of missing values" , call. = FALSE )
   }
   if( !is.na( checksExperimentStartForProcessStart) & ! checksExperimentStartForProcessStart ) {
     errors                          <-  addMessage( errors , "Time start experiment ('SpectraInTime-class') is later than timeHeatingAboveMin ('ProcessTime-class')   " )
   }
    
    ## error messages showing
    generalErrorMessage              <-  "Object 'SpectraInTime' and object 'ProcessTimes' are incompatible"
    if( length( errors ) >= 1L ){
      stop( generalErrorMessage, ": " , "\n" , errors , domain = NA , call. = FALSE  )
    } else {
      TRUE
    }
  }
)



#' @rdname checkCompatible
setMethod( "checkCompatible" , signature = c( x = "ProcessTimes"  , y = "SpectraInTime" ) , 
  definition = function( x , y ){
    checkCompatible( x = y , y = x  )
  }
)



