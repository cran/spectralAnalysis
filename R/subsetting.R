# Project: spectralAnalysis-R-package
# 
#  Subsetting methods with addition of range subsetting, and closest element matching
#
# Author: ablommaert
###############################################################################



#' @include internalHelpers.R allGenericFunctions.R objectSpectraInTime.R
NULL




#' Elements S4 class useful for closest elements subsetting
#' 
#' 
#' @slot elements numeric vector of elements
#' @author Adriaan Blommaert 
#' @name ElementsToSelect-class
#' @export 
setClass( "ElementsToSelect" , slots = c( elements = "numeric" ) )

 
#' Create an \code{\link{ElementsToSelect-class}} from a numeric vector or multiple numeric values or vectors
#' 
#' @param x numeric  vector
#' @param ... additional numeric vectors
#' @return \code{\link{ElementsToSelect-class}} with unique elements
#' @export
#' @examples
#' e( 1 , 5, 4.5  )
#' e( 1:10 , c(4 , 5 , 6 ) , 7 )
e          <-   function( x , ... ) { 
    
    new( "ElementsToSelect" , elements = unique( as.numeric( c(x , ...) ) )  )
}



### Range object for subsetting (only vectors, spectral matrix no own definition)

if( 0 == 1 ){
  # test validity check
  test          <-  new( "RangeToSubset" , range =  c(1,2) )
  str(test)
  validObject( test )
  test2         <-  new( "RangeToSubset" ,  range = c(3,4) )  # should get a an error in range validity
  validObject( test2 )
}

#' Range S4 class (range) useful for subsetting with actual values instead of indicators
#' 
#' @slot range numeric vector with min and max value 
#' @name RangeToSubset-class
#' @aliases RangeToSubset rangetosubset Rangetosubset 
#' @author Adriaan Blommaert 
#' @export 
setClass( "RangeToSubset" , slots = list( range = "numeric"  ) )


if( 0 == 1 ){
  # debuggin no error message 
  object            <-  test2
}

Range.validity           <-  function( object ) {
  ## sett abject
  errors                 <-  character()
  range                  <-  object@range
  min                    <-  range[ 1 ]
  max                    <-  range[ 2 ]
  ## tests 
  checkLength            <-   length( range ) == 2
  if( ! checkLength  ) {
    errors               <-  addMessage( errors ,  "Range object should have 2 values" ) } 
  checkMinMax            <-  min <= max
  if( ! checkMinMax  ) { 
    errors               <-  addMessage( errors ,  "'min' should be smaller then 'max'" ) } 
  ## produce error message 
  processValididtyErrors( errors )
}

setValidity( "RangeToSubset" ,  Range.validity )


#' @rdname getRange
#' @export
setMethod( f = "getRange" , signature = "RangeToSubset" , 
  definition = function( object ) { 
    return( object@range ) 
  }
)

#' @rdname getElements
#' @export
setMethod( f = "getElements" , signature = "ElementsToSelect",
  definition = function( object ){
    return( object@elements )
    
  }
)




#' create a \code{\link{RangeToSubset-class}} object from 2 elements or from a vector
#' 
#' @param x numeric value or vector of numeric values
#' @param y numeric value missing when x is a vector of values 
#' @name r
#' @export
setGeneric( name = "r" , def = function( x , y  ) { 
    rangeVec             <-  range( x )
    new( "RangeToSubset" , range = c( min = rangeVec[ 1 ]  , max = rangeVec[ 2 ] ) )
  }
)


#' @rdname r
setMethod( "r" , signature( x = "numeric" , y = "numeric" ) , definition = function( x , y ) {
    new( "RangeToSubset" , range = c( min = min( x, y ) ,  max = max( x , y ) )  )
  } 
)

#' @rdname r
setMethod( "r" , signature( x = "RangeToSubset" , y = "missing" ) , definition = function( x , y ) {
      return( x )
    } 
)


if( 0 ==  1 ) {
  
  ## example
  spectralEx                      <-  getSpectraInTimeExample()
  getTimePoints( spectralEx )
  getWavelengths( spectralEx )
  dim( getSpectra( spectralEx )  )
  # range subsetting 
  spectraSubset                   <-  spectralEx[ r( 1000 , 30000 ) , r(130 , 135 ) , timeUnit = "seconds", timePointsAlt = FALSE ]
  getTimePoints( spectraSubset )
  getWavelengths( spectraSubset )
  spectraSubsetDefaults           <-  spectralEx[ r( 1000 , 30000 ) , r(130 , 135 )  ]
  identical( spectraSubset , spectraSubsetDefaults )
  
 
  spectraTimeSubset               <-  spectralEx[  r( 1000 , 30000 ) ,] 
  # test time subsetting is possible 
  spectraTimeSubsetHours          <-  spectralEx[r( 1000/60/60 , 30000/60/60 )  ,, timeUnit = "hours"] 
  
  identical( spectraTimeSubset , spectraTimeSubsetHours ) # so works without problems 
  
  
  getTimePoints( xx )
  xx[  r( 1000 , 30000 ) ,] 
  

  
  
  
  getTimePoints( spectraTimeSubset )
  getWavelengths( spectraTimeSubset  )
#  spectraTimeSubsetAlt               <-  spectralEx[ r( 1000 , 30000 ) , , useA]
  
  spectraWavelengthSubset         <-  spectralEx[  ,  r(130 , 135 ) ]
  
  # range subsetting on alternative time points 
  
  # other types of subsetting 
  # logical
  spectraSubsetLogical            <-  spectralEx[ getTimePoints( spectralEx ) > 300   , getWavelengths( spectralEx ) <= 500 ] # TODO error not working 
  subsetLogTimeAlt                <-  spectralEx[  getTimePoints(  spectralEx , timePointsAlt = TRUE ) > 0   ,  getWavelengths( spectralEx ) <= 500 ]
  
  
  # test on 
  testSet   <-  spectralEx[ r(100, 1200) , r(300 , 308) ] 
  testSet[ getTimePoints(testSet) > 110 , r(300 , 304)]
  # integer
  
  subsetInteger                   <-  spectra[ c( 1, 5, 10)  , c( 4 , 4 , 4 , 8 , 16) ] # incorrect behaviour
  
  # closest element matching 
  
  spectraSubsetElem               <-  spectralEx[ e( 1.2*60*60 , 3.5*60*60 ) , e( 200 , 466.96  ) ]

# TODO example times formatting 
   
  # check dim
  dim( getSpectra( spectralEx ) )
  dim( getSpectra( spectraSubset ) )
  dim( getSpectra( spectraTimeSubset ) )
  dim( getSpectra( spectraWavelengthSubset ) )
  dim( spectraSubsetLog )
  dim( get( subsetLogTimeAlt ) )
  
  # check correct subsetting 
  
  
  
  i  = c( 1 , 2 )
  j  = c( 1 , 2 , 4 ) 
  x[ c( 1 , 2 , 3 , 4 ) ,  ]
  
  
  ## debugging 
  x  <-  spectralEx
  i  <-  r( 1000 , 30000 ) 
  j  <-   r(130 , 135 )
  timeUnit = "seconds" 
  timePointsAlt = FALSE
  
  x <-  xx
  g[ r( 1000 , 30000 ) , r(-Inf , Inf)  ] # only in subsetting only time points 
  x[ r( 1000 , 30000 )   ] # only in subsetting only time points 
  
}



#' internal function for subsetting 
#' @keywords internal 
subset.SpectraTime              <-  function( x , i  , j , timeUnit = "seconds" , timePointsAlt = FALSE , drop = "" ) { 

  ## extract basic elements 
  times                        <-  getTimePoints( x , timePointsAlt = timePointsAlt , timeUnit = timeUnit  )
  wavelengths                  <-  getWavelengths( x )
  specialSubsetClasses         <-  c( "RangeToSubset" , "ElementsToSelect" )
  checkIIsSpecialSubsetting    <-  class( i ) %in% specialSubsetClasses
  checkJIsSpecialSubsetting    <-  class( j ) %in% specialSubsetClasses
  
  ## Range subsetting (convert range to indicators )
  if( class( i ) == "RangeToSubset"  ) {
    iTimes                     <- flagVectorInInterval( times ,  getRange( i  )  )
  } 
  if( class( j ) == "RangeToSubset"  ) {
    jWavelengths               <- flagVectorInInterval( wavelengths , getRange( j ) )
  }
  
  ## Closest element subsetting  (convert ElementsToSelect to indicators )
  if( class( i ) == "ElementsToSelect"  ) {
    iTimes                     <- unique( getClosestElements( times ,  getElements( i  )   ) )
  } 
  if( class( j ) == "ElementsToSelect"  ) {
    jWavelengths               <- unique( getClosestElements( wavelengths , getElements( j )  ) )
  } 
  
#    ## if not range or elements, pass trough ordinary subsetting methods
  if( ! checkIIsSpecialSubsetting ) {
    iTimes                     <-  i
  }
  if( ! checkJIsSpecialSubsetting ) {
    jWavelengths               <-  j     
  }
  
  ## create new object by subsetting every time and wavelength dependent slot
  newSpectra                  <-  x
  newSpectra@spectra          <-  x@spectra[ iTimes , jWavelengths , drop = FALSE ]
  newSpectra@wavelengths      <-  x@wavelengths[ jWavelengths , drop = FALSE ]
  newSpectra@timePoints       <-  x@timePoints[ iTimes , drop = FALSE ]
  newSpectra@timePointsAlt    <-  x@timePointsAlt[ iTimes , drop = FALSE ] 
#    newSpectra@preprocessing    <-  append( x@preprocessing , list( timeSelect = i , wavelengthSelect = j ) ) # not necessary you see when timepoint do not start with one 
  validObject( newSpectra )
#  cat("version 7 used ")
  return( newSpectra )
}

## Subsetting methods: taken care of missing values


#'  Subsetting  \code{\link{SpectraInTime-class}}
#' 
#' @param x object to subset
#' @param i subsetting rows ( timePoints )
#' @param j subsetting columns ( wavelengths )
#' @param  ... additional parameters 
#'   \itemize{
#'      \item timeUnit unit at which subsetting should be done choose between \code{seconds} , \code{minutes} or \code{hours} 
#' defaults to \code{seconds}
#'      \item   timePointsAlt logical indicators whater alternative timePoints should be used 
#' }
#' @param  drop for consistancy, not used 
#' @name subset-methods
#' @rdname subset-methods
#' @aliases [,SpectraInTime-method  [,SpectraInTime,ANY,ANY,ANY-method  [,SpectraInTime,ANY,ANY-method [,SpectraInTime,missing,ANY-method  [,SpectraInTime,ANY,missing-method  [,SpectraInTime,missing,missing-method
#' @examples 
#'  ### subsetting [ time , wavelength, options ]
#' 
#'  spectralEx                <-  getSpectraInTimeExample()
#'  spectraSubset             <-  spectralEx[ r( 1000 , 30000 ) , r(130 , 135 ) ]
#'  spectraSubsetTime         <-  spectralEx[ r( 1000 , 30000 ) ,  ]
#'  spectraSubsetWavelengths  <-  spectralEx[  ,  r(130 , 135 ) ]
#'  spectraSubsetHours        <-  spectralEx[ r( 1 , 3 ) , r(130 , 135 ) , timeUnit = "hours" ]
#'  closestWavelengths        <-  spectralEx[ , e( 150, 4, 300, 500 ) ] # remark only unique values 
#'  spectraSubsetLogical      <-  spectralEx[ getTimePoints( spectralEx ) > 300   ,
#'     getWavelengths( spectralEx ) <= 500 ]
#' @importFrom BiocGenerics as.data.frame
#' @export
setMethod( "[",  signature = c( "SpectraInTime" )  ,
    definition =  function( x , i  , j , ... , drop = "" ) {
      extraSettings                  <-  list( ... )
      fullSettings                   <-  c( list( x = x , i = i , j = j ) , extraSettings )
      do.call( subset.SpectraTime , fullSettings   )
    }
)


## define methods for missing i and j (then no subsetting)



#' @rdname subset-methods
setMethod( "[", c( "SpectraInTime" , "missing" , "ANY" ) , 
  function( x , i  , j ,  ... , drop = "" ) {
    extraSettings                  <-  list( ... )
    fullSettings                   <-  c( list( x = x , i = TRUE , j = j ) , extraSettings )
    do.call( subset.SpectraTime , fullSettings  )
  }
)



## define methods for missing i and j (then no subsetting)


#' @rdname subset-methods
setMethod( "[", c( "SpectraInTime" , "ANY" , "missing" ) , 
    function( x , i  , j ,  ... , drop = "" ) {
      extraSettings                  <-  list( ... )
      fullSettings                   <-  c( list( x = x , i = i , j = TRUE ) , extraSettings )
      do.call( subset.SpectraTime , fullSettings  )
    }
)


#' @rdname subset-methods
setMethod( "[", c( "SpectraInTime" , "missing" , "missing" ) , 
  function( x , i  , j ,... ,  drop = "" ) {
    x   # no subsetting if everything is missing 
  }
)


