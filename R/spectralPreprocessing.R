# Preprocessing tools via method , option 
# internal preprocessing method has syntax ( object , method , additinal arguments ) 
# 
# Author: Adriaan Blommaert
###############################################################################




if( 0 == 1) {
  # get arguments in function call
  
  testFunc     <-  function( x , y = 3 , z ){
    argumentsSyst  <-  as.list( sys.call() ) 
    argumentsCal   <-  as.list( match.call() )
    result     <-  x+y+z
    return( list( result = result , argumentsSyst = argumentsSyst , argumentsCal = argumentsCal  ) )
  }
  
  testFunc( x = 1 ,y = 2 , z =  3)
  
  
  test1        <-  testFunc( x = 1 , z =  3)
  arguments    <-  test1$argumentsSyst
  test2        <-  do.call( as.character( arguments[[1]] ) , arguments[-1] )
  identical( test1 , test2 )
  
  
  ## test on spectralNormalization
  exSpectra                  <-  getSpectraInTimeExample()
  test1Norm                  <-  spectralNormalization( object )
  preprocessingStep          <-  getPreprocessing( test1Norm )[[1]]
  functionName               <-  preprocessingStep$preprocessing
  functionArguments          <-  list( object = object , convertNamesToCharacter( preprocessingStep$arguments ) )
  test2Norm                  <-  do.call( functionName , functionArguments  )
  identical( test1Norm , test2Norm )  # not identical ???? they should be identical
  identical( getSpectra( test1Norm) , getSpectra( test2Norm ))
  
   
  identical( getPreprocessing( test1Norm) , getPreprocessing( test2Norm ) )
  
  preprocessingStep          <-  getPreprocessing( test2Norm )[[1]]
  functionName               <-  preprocessingStep$preprocessing
  functionArguments          <-  list( object = object , convertNamesToCharacter( preprocessingStep$arguments ) )
  test2Norm                  <-  do.call( functionName , functionArguments  )
  
  
  # test directly on normalization frame 
  
  object                    <-  getSpectraInTimeExample()
  testNormMethod            <-  normalize( object , method = "integration" , wavelengthRange = c( 200 , 250 ) )
  preprocessingStep         <-  getPreprocessing( testNormMethod  )[[1]]
  functionName              <-  as.character( preprocessingStep[[1]] )
  functionArguments         <-  as.list( preprocessingStep[ -1 ] ) # keep it a list
  functionArguments[[1]]    <-  object # replace original object with new object
  
  testNormMethod2           <-  do.call( functionName , functionArguments )
  identical( testNormMethod , testNormMethod2 )
  
  identical( getSpectra( testNormMethod )  , getSpectra( testNormMethod2 ) ) # identical spectra
  getPreprocessing( testNormMethod2 ) # TODO reset preprocessing field to original arguments 
  
      
  # replace first slot with new argument     
  # test again with ,local
  
  
  convertNamesToCharacter    <-  function( list ){
    oneConversion            <-  function( x ){
      ifelse( is.name( x) , as.character(x) , x )
    }
    lapply( list , oneConversion)
    
  }



}


#' normalization function
#' @keywords internal
spectralNormalization             <-  function( object , method = "normalize" ,  wavelengthRange = r(-Inf, Inf) ,
    wavelength = NULL , scaleFunction = 'sd' , meanFunction = NULL   ){
  ## general objects
#  print(as.list(match.call())) # to check arguments 
#  print(sys.calls())
#  arguments                       <-  as.list( sys.call( 2 ) )
#  arguments                       <-  match.call()
  arguments                       <-  list( "normalize" ,  method = method , wavelengthRange = wavelengthRange , wavelength = wavelength , 
      scaleFunction = scaleFunction , meanFunction = meanFunction )
#  arguments                       <-  as.list( sys.call(1) )
  TIMEMARGIN                      <-  1  
  oldSpectra                      <-  getSpectra( object )
  
  ## check inputs 
  allowedMethods                  <-  c( "integration" , "peak" , "normalize"  )# tODO method checking 
  checkMethod                     <-  method %in% allowedMethods
  if( ! checkMethod  ) {
    stop( paste0( "'method' not recognized, choose method from :" , "\n" , "\t" , paste( allowedMethods , collapse = "; ") ) )
  }
  
  ## per method to normalization/scaling step 
  if( method == "integration" ){
    integratedValues              <-  spectralIntegration( object  , wavelenghtRange = wavelengthRange   )
    referenceValue                <-  integratedValues[ , "value" ] 
    newSpectra                    <-  sweep( oldSpectra , TIMEMARGIN , STATS =  referenceValue , FUN = "/")
  }
  if( method == "peak" ) {
    if( is.null( wavelength ) ) { stop("specify  'wavelength' to use peak normalization" ) }
    referenceValue                <-  getSpectra( object[ , e( wavelength ) ] )
    newSpectra                    <-  sweep( oldSpectra , TIMEMARGIN, STATS =  referenceValue , FUN = "/"  )
  }
    
  if( method == "normalize" ){
    if( is.null( meanFunction ) ) {
      meanCorrSpectra             <-  oldSpectra
    } else {
      meanSpec                    <-  apply( oldSpectra , TIMEMARGIN , meanFunction )
      meanCorrSpectra             <-  sweep( oldSpectra , TIMEMARGIN , STATS = meanSpec )
    }
     
    scalePerTime                  <-  apply( meanCorrSpectra  , TIMEMARGIN , scaleFunction )
    newSpectra                    <-  sweep( meanCorrSpectra  , TIMEMARGIN , STATS = scalePerTime , FUN = "/" )
  }
  
  newObject                       <-  addPreprocessingStep( object , newSpectra,  arguments )
  return( newObject )
}


#' @rdname normalize
#' @param method a method for normalization or peak correction , choose from:
#'     * normalize substract \code{mean} and divide by \code{scale}
#'     * peak scale by reference \code{wavelength}
#'     * integrate scale by integrating over \code{wavelengthRange}
#' @param  wavelengthRange range for integration if method = \code{integration} , defaults to complete range 
#' @param  wavelength reference wavelength for \code{peak} regresssion 
#' @param  scaleFunction scale function used when method = \code{normalize} defaults to \code{\link[stats]{sd}}  
#' @param  meanFunction mean function used when method = \code{normalize} defaults to \code{\link[base]{mean}} 
#' @examples 
#'  spectralEx            <-  getSpectraInTimeExample()
#'  timeRange                        <-  range( getTimePoints( spectralEx ))
#'  timesToSelect                    <-  e(  seq( timeRange[1] , timeRange[2] , length.out = 5  )   )
#'  \dontrun{
#'  plot( spectralEx )
#'  plot( spectralEx[ timesToSelect ,  ] , type = "time" )
#' }
#'  normalizePeak         <-  normalize( spectralEx , method = "peak" , wavelength = 400 )
#'  getPreprocessing( normalizePeak )
#' \dontrun{
#'  plot( normalizePeak[ timesToSelect ,  ] , type = "time" )
#'  plot( normalizePeak )
#' }
#'  normalizeIntegration  <-  normalize( spectralEx , method = "integration" )
#' \dontrun{
#'  plot( normalizeIntegration[ timesToSelect ,  ] , type = "time" )
#' }
#'  normalizedUser  <-  normalize( spectralEx , method = "normalize" , mean = "median" , scale = "sd" )
#' \dontrun{
#'  plot( normalizedUser[ timesToSelect ,  ] , type = "time" ) 
#' }
#' @importFrom baseline baseline getCorrected
#' @export 
setMethod( "normalize" , "SpectraInTime"  , definition = spectralNormalization )


if( 0 == 1 ){
  # debugging 
  object = getSpectraInTimeExample() ; method = 'lowpass'; degree = 1
  additionalArguments <-  list(  )
  
  
  testBase1                        <-  spectralBaselineCorrect(  ) 
  
}

spectralBaselineCorrect            <-  function( object , method = 'modpolyfit' , degree = 4 , ...   ) {
  additionalArguments              <-  list( ... )
#  arguments                        <-  as.list( sys.call( 2 ) )
#  arguments                       <-  as.list( sys.call(1) )
  arguments                        <-  c( list( "baselineCorrect" , method = method , degree = degree  ) , additionalArguments )
  oldSpectra                       <-  getSpectra( object )
  
  ## pass baseline correction method
  baseArguments                    <-  list( spectra = oldSpectra , method = method , degree = degree )
  if( method != 'modpolyfit' ){
    baseArguments$degree           <-  NULL 
  }
  argumentsForBaselineCorrection   <-  c( baseArguments , additionalArguments )
    
#  newSpectra                       <-  getCorrected( do.call( baseline , args = argumentsForBaselineCorrection ) )
  newSpectraBaselineFormat          <-  do.call( "baseline" , args = argumentsForBaselineCorrection )
  newSpectra                        <-  getCorrected( newSpectraBaselineFormat )  
  
  ## output new object with appended methods part
  newObject                         <-  addPreprocessingStep( object , newSpectra, arguments )
  return( newObject )
}




#'@rdname baselineCorrect 
#' 
#' @note baseline correction in the wavelength domain by linking to the \code{\link[baseline]{baseline}}
#' @param method method of baseline correction, default value is to \code{'modpolyfit'},  see \code{\link[baseline]{baseline.modpolyfit}}
#' @param degree numeric value, degree of the polynomial used only if \code{method} is code{'modpolyfit'}
#' @param ... other parameters passed to \code{\link[baseline]{baseline}}
#' @examples 
#'  spectralEx           <-  getSpectraInTimeExample()
#'  plot( spectralEx )   
#'  timeRange            <-  range( getTimePoints( spectralEx ) )
#'  timesToSelect        <-  e(  seq( timeRange[1] , timeRange[2] , length.out = 5  )   )
#'  baselineDefault      <-  baselineCorrect( spectralEx )
#'  baselineHighPolynomial  <-  baselineCorrect( spectralEx, 
#'    method = 'modpolyfit', degree = 4 )
#' 
#'  # filtering with fast fourier transform, not so good on example 
#'  baselineLowpass         <-  baselineCorrect( spectralEx , method = "lowpass" )
#' 
#'  # visual inspection
#'  plot( baselineDefault[ timesToSelect , ] , type = "time"  )
#'  plot( baselineHighPolynomial[ timesToSelect , ] , type = "time"  )
#'  plot( baselineLowpass[ timesToSelect , ] , type = "time"  ) 
#' 
#' @export
setMethod( "baselineCorrect" , "SpectraInTime" , spectralBaselineCorrect )


#' internal smoothing and differentiation function
#' @keywords internal
spectralSmoothAndDifferentiate     <-  function( object, method = "sg" , order = 3 ,  window = order + 3 - order%%2 , derivative = 0  ){
#  arguments                        <-  as.list( sys.call(2) )
#  arguments                       <-  as.list( sys.call(1) )
  arguments                        <-  list( "smooth" , method = method , order = order , 
      window = window , derivative = derivative )
  TIMEMARGIN                       <-  1  
  oldSpectra                       <-  getSpectra( object )

  
  ##  check inputs 
    #  derivative and order
  checkOrderDerivative             <- order > derivative
  if( ! checkOrderDerivative ) {
    stop( "'order' should be larger than 'derivative'")
  }
    # method
  allowedMethods                   <-  c( "sg"  )
  checkMethod( method = method , allowedMethods = allowedMethods )
  
  ## SG filtering  
  # tODO:test
  if( method == 'sg') {
    filterFunction                 <- function( row ) {
      result                       <-  sgolayfilt( x = row , p = order  , n = window , m = derivative )
      return( result )
    }   
  }
  
  ## adapt object
  newSpectra                       <-  t( apply( oldSpectra , TIMEMARGIN , filterFunction )  ) 
  
  newObject                        <-  addPreprocessingStep( object = object , newSpectra = newSpectra , arguments )
  return( newObject ) 
}



#' Apply Savitzky-Golay smoothing filter and optionally return smoothed spectra or dif
#' 
#' smoothing is applied in the wavelength domain, not in the time domain 
#' @param method character vector smoothing method, default = 'sg', i.e Savitsky-Golay filter.
#'   currently only implemented smoothing method
#' @param order numeric value,  order of the polynomial used to interpolate, should be larger than derivative order,
#'  defaults to 3 + derivative 
#' @param  window width of the smoothing 
#' @param derivative derivative to be taken, defaults to \code{0}
#' @rdname smooth
#' @examples 
#'      spectralEx     <-  getSpectraInTimeExample()
#'     smoothDefault   <-  smooth( spectralEx )
#'     timeRange       <-  range( getTimePoints( spectralEx ))
#'     timesToSelect   <-  e( seq( timeRange[1] , timeRange[2] , length.out = 5  )  )
#'     # plot( smoothDefault  )
#'     # plot( smoothDefault[ timesToSelect , ] , type = "time")
#'     smoothALot      <-  smooth( spectralEx ,  order = 2 , window = 301  ) 
#'     # plot( smoothALot )
#'     # plot( smoothALot[  timesToSelect , ] , type = "time" )
#'     derivative1     <-  smooth( spectralEx , derivative = 1 )
#'     # plot( derivative1 )
#'     # plot( derivative1[  timesToSelect ,] , type = "time" )
#'     
#'     derivative2     <-  smooth( spectralEx , derivative = 2 )
#'     # plot(  derivative2 )
#'     # plot( derivative2[  timesToSelect , ] , type = "time" )
#'     
#' @note equal distances between wavelenght intervals are assumed
#' @importFrom signal sgolayfilt
setMethod( "smooth" , "SpectraInTime" ,  spectralSmoothAndDifferentiate )


### general helper function

if( 0 == 1 ){
  object           <-  getSpectraInTimeExample() 
  newSpectra       <-  (object@spectra/2)
  preprocessingInfo  <-  list( blabla = "blabla" )
  
  test1 <-  addPreprocessingStep( object , newSpectra , preprocessingInfo )
  getPreprocessing( test1 )[[1]]
  test2 <-  addPreprocessingStep( test1, newSpectra/2 , preprocessingInfo = list( 1234, "hv" , "hv"))
}




if( 0 == 1 ){
  object            <-   getSpectraInTimeExample()
  object@preprocessing <-  list( step1 = list( 1 ,2 , 3) , step2 = list('a' , 'b' , 'c' , c(1,2,3)) )
  newSpectra        <-  getSpectra( object ) * 0 
  preprocessingInfo    <-  list("this is the latest step ")
  
  addPreprocessingStep( object , newSpectra , preprocessingInfo )
}

#' function to use by preprocessing step
#' 
#' change the spectra and add preprocessing info, check whehther the object is valid 
#' @param object \code{\link{SpectraInTime-class}}
#' @param newSpectra numeric matrix new spectral data
#' @param list of preprocessing info
#' @return code{\link{SpectraInTime-class}}
#' @keywords internal 
addPreprocessingStep               <-  function( object , newSpectra , preprocessingInfo ){
  newObject                        <-  object
  newObject@spectra                <-  newSpectra
  nPreprocessing                   <-  length( object@preprocessing ) + 1 # add current preprocessing step
  newObject@preprocessing[[ nPreprocessing ]] <- preprocessingInfo
  names( newObject@preprocessing ) <-  paste0( "step" , seq_len( nPreprocessing ) )
  validObject( newObject )
  return( newObject ) 
}


#' check method in list of allowd method
#' 
#' @param method character of method to check
#' @param allowedMethod character vector of allowed method
#' @return nothing just stop if method not allowed
#' @keywords internal 
checkMethod                      <-  function( method , allowedMethods ) {
  checkLength                    <-  length( method ) == 1 
  checkCharacter                 <-  is.character( method )
  if( ! ( checkLength && checkCharacter ) ) {
    stop( "specify only 1 method as a character" )
  }
  checkMethod                    <-  method %in% allowedMethods
  if( ! checkMethod  ) {
    stop( paste0( "'method' not recognized, choose method from :" , "\n" , "\t" , paste( allowedMethods , collapse = "; ") ) )
  } 
}


### preprocess (repeat preprocessing step)

if( 0 ==  1) {
  ## bug in setting preprocessing steps 
  objectExample    <-   getSpectraInTimeExample()
  test1  <-  normalize( objectExample ) 
  getPreprocessing( test1 )
  test2  <-  baselineCorrect( test1 )
  getPreprocessing( test2 )
  test3   <- smooth( test2 )
  getPreprocessing( test3 )
  
    
  test3Bis         <-  smooth( baselineCorrect( normalize( objectExample ) ) )
  getPreprocessing( test3Bis )
  
  identical( getSpectra( test3 ) , getSpectra( test3Bis ) ) 
  
  test3Tris        <-  preprocessSpectraInTimeWithList( objectExample , getPreprocessing( test3 ) )
  
  identical( getSpectra(test3) , getSpectra(test3Bis) ) 
  
  getPreprocessing( smooth( objectExample ) )
  testje            <-   smooth( baselineCorrect( objectExample ) ) 
  
  
  
  objectExample                          <-  getSpectraInTimeExample()
#  objectPreprocessed                     <-  baselineCorrect( smooth( normalize( objectExample ) ) )
  objectPreprocessed                     <-  baselineCorrect( smooth( normalize( objectExample , method = "integration" ) ) )
  preprocessingSteps                     <-  getPreprocessing( objectPreprocessed )
  object                                 <-  objectExample
  
  objectPreprocessed2                    <-  preprocessSpectraInTimeWithList( objectExample , preprocessingSteps )
  identical( getSpectra( objectPreprocessed) , getSpectra( objectPreprocessed2) )
}

#' internal function to wrap multiple preprocessing steps
#' @keywords internal
preprocessSpectraInTimeWithList          <-  function( object , with ){
  
  # TODO: input checking 
  preprocessingSteps                     <-  with
  nSteps                                 <-  length( preprocessingSteps )  
  objectPreprocessing                    <-  object # initialization of object 
  # iStep = 1 
  for( iStep in seq_len( nSteps) ){
    preprocessStep                       <-  preprocessingSteps[[ iStep ]]
    functionName                         <-  as.character( preprocessStep[[1]] )
    functionArguments                    <-  as.list( preprocessStep[ -1 ] ) # remove function
#    functionArguments[[1]]               <-  objectPreprocessing # replace object
    objectPreprocessing                  <-  do.call( functionName , c( list( object = objectPreprocessing ) , functionArguments ) )
  }
  newObject                              <-  objectPreprocessing
  newObject@preprocessing                <-  preprocessingSteps
  validObject( newObject )
  return( newObject )
}

## specific preprocessing methods 
#' @rdname preprocess
#' @examples
#' 
#'  object1   <-  getSpectraInTimeExample()
#'  object2   <-  getSpectraInTimeExample
#' 
#' @export
setMethod( "preprocess" ,  c("SpectraInTime" , "list" ) , preprocessSpectraInTimeWithList )

#' @rdname preprocess
#' @export
setMethod( "preprocess" ,  c( object = "SpectraInTime" , with = "SpectraInTime" ) , function( object , with ){
      preprocessingList       <-  getPreprocessing( with )
      preprocess( object , preprocessingList )
    }
)



if( 0 == 1 ){
  object           <-  getSpectraInTimeExample()
  plot( object )
  
  baseWavelengths = 200
  
  baseWavelengths = c( 200 , 400 )
}

#' local baseline correct, substract a baseline either trough 1 or 2 points
#' 
#' @param object  \code{\link{SpectraInTime-class}}
#' @param baseWavelengths numeric vector of 1 or 2 wavelength use to draw a baseline trough,
#' defaults to \code{NULL} when no baseline correction is performed
#' @return  \code{\link{SpectraInTime-class}} with baseline subset
#' @examples 
#' spectra              <-  getSpectraInTimeExample()
#' spectraConstCorrect  <-  localBaselineCorrect( spectra , baseWavelengths = 240  )
#' spectraLinCorrect    <-  localBaselineCorrect( spectra , c( 250 , 330 )  ) 
#' \dontrun{
#' plot( spectra )
#' plot( spectraConstCorrect )
#' plot( spectraLinCorrect ) 
#' }
#' @author Adriaan Blommaert
#' @export
localBaselineCorrect           <-  function( object , baseWavelengths = NULL  ) {
  
  ## no baselines specified, return without correction 
  if( is.null( baseWavelengths) ){
    return( object ) # object returned no correction 
  }
  
  ## general calculations 
  baselineInfo                 <-  getSpectra( object[ , e( baseWavelengths ) ] )
  originalSpectra              <-  getSpectra( object )
  spectralDims                 <-  dim( originalSpectra )
  
  
  ##  1 baseline wavelength ( substract constanct in wavelength domain)
  nBaseWavelengths             <-  length( baseWavelengths )
  if( nBaseWavelengths == 1 ){
    baseSpectrumMat               <-  matrix( baselineInfo , nrow = spectralDims[1] , ncol = spectralDims[2] )
    correctedSpectrum          <-  originalSpectra -  baseSpectrumMat    
  } else if ( nBaseWavelengths == 2 ){
    ##  2 baseline wavelength ( substract line in wavelength domain)
    
    baseWL1                      <-  baseWavelengths[ 1 ]
    baseWL2                      <-  baseWavelengths[ 2 ]
    baseData1                    <-  baselineInfo[ , 1 ]
    baseData2                    <-  baselineInfo[ , 2 ]
    deltaX                       <-  baseWL2 - baseWL1
    deltaY                       <-  baseData2 - baseData1
    rico                         <-  deltaY/deltaX
    waveDiffVersusBase1          <-  getWavelengths( object ) -  baseWL1
    
    # convert everything to correct dimensions
    baseData1Mat                 <-  matrix( baseData1  , byrow = FALSE , nrow = spectralDims[1] , ncol = spectralDims[2] )
    ricoMat                      <-  matrix( rico  , byrow = FALSE , nrow = spectralDims[1] , ncol = spectralDims[2] )
    waveDiffVersusBase1Mat       <-  matrix( waveDiffVersusBase1  , byrow = TRUE , nrow = spectralDims[1] , ncol = spectralDims[2] )
    
    
    baseSpectrum                 <-  baseData1Mat +   ricoMat * waveDiffVersusBase1Mat
    if( 0 == 1 ){ # check out base spectrum substracted 
      testBaseSpec               <-  object
      testBaseSpec@spectra        <-  baseSpectrum   
      plot( testBaseSpec )
    }
    
    
    correctedSpectrum            <-  originalSpectra - baseSpectrum 
   
  } else {
    stop( "'baseWavelenghts should be either NULL or a numeric vector of length 1 or 2'" )
  }
  
  
  ## output prep and check 
  
  
  baselineCorrectedSpectrum    <-  object 
  baselineCorrectedSpectrum@spectra  <-  correctedSpectrum 
  validObject                  <-  validObject( baselineCorrectedSpectrum  )
  if( validObject ) {
    return( baselineCorrectedSpectrum )
  } 
} # TODO modify preprocessing field












