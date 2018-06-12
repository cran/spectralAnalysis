# Plotly renewed 3dPlot 
# 
# Author: Adriaan Blommaert
###############################################################################



#' help function to make plotly axis title
#' 
#' @keywords internal 
plotlyAxis                       <-  function( title ) {
  listSettings                   <-  list( title = title )
  
  ## conditional settings
  isWavelength                <-  grep( "wave" , x = title , ignore.case = TRUE  )
  if( length( isWavelength) > 1 ) {
    listSettings$autorange       <-  "reversed"       
  }
  return( listSettings )
}


### Plotting methods 

#' Plotting methods for 'spectralAnalysis'
#' 
#' @param x the object to be plotted
#' @param y not used, for consitency with plot method
#' @param type character choice of plot type between:
#'   * \code{3D} surface plot (default)
#'   * \code{wavelenth} wavelength in legend, time on the axis
#'   * \code{time} time in legend, wavelength on the axis
#' @param colors colorblind friendly palettes are used from the de \code{\link[viridis]{viridis_pal}} choice between code{"A"} (magna),
#'  code{"B"} (inferno),  \code{"C"} (plasma) and \code{"D"} the default viridis color palette
#' @importFrom plotly plot_ly add_surface layout add_lines
#' @importFrom magrittr  %>%
#' @importFrom viridis viridis_pal
#' @examples 
#' 
#' ### visualization 
#'   \dontrun{
#'   data = getSpectraInTimeExample()
#'   plot( x =  data , type = "3D" , timeUnit = "hours" , timePointsAlt = FALSE )
#'   plot( x =  data[  , r(500, 350) ] , type = "3D" ,
#'       timeUnit = "hours" , timePointsAlt = TRUE , colors = "B"  )
#'   plot( x =  data[ e( 1 , 2 , 3) , , timeUnit = "hours" ] ,
#'       type = "time" , timeUnit = "hours" , timePointsAlt = FALSE ) 
#'   plot( x =  data[ , e( seq( 200 , 400 , 50 ) ) ] , 
#'      type = "wavelength" , timeUnit = "minutes" , timePointsAlt = TRUE , colors = "A" )
#' }
#' 
#'   
#' @rdname SpectraInTime-class                  
#' @export
setMethod( "plot" , signature =  c( x = "SpectraInTime" , y = "missing" ) , function( x , y , type = "3D" , timeUnit = "hours"  , 
        timePointsAlt = FALSE , colors = "D" ) {
      ##  check inputs
#      cat("plot method is used" , "\n" )
      allowedPlotTypes               <-  c( "3D" , "wavelength" , "time" )
      plotTypesFormat                <-  paste0( "'" , paste( allowedPlotTypes , collapse = "', '") , "'" )
      checkPlotTypeAllowed           <-  type %in% allowedPlotTypes
      if( ! checkPlotTypeAllowed ) {
        stop( "Choose plotType out of: " , plotTypesFormat )
      }
      
      ##  call plot subtype
      arguments                       <-  list( x = x , y = NULL , timeUnit = timeUnit , timePointsAlt = timePointsAlt , colorPalette = colors  )
      if( type  == "3D" ){  
#        cat("3D plot chosen")
        plotlyPlot                      <-  do.call( plot.spectrum3D , args = arguments  ) 
      }
      if( type == "wavelength" ){   
        plotlyPlot                      <-  do.call( plot.wavelengths , args = arguments  ) 
      }
      if( type == "time" ) {
        plotlyPlot                      <-  do.call( plot.times , args = arguments  ) 
      }
      return( plotlyPlot )
    }
)


if( 0 == 1 ) {
  library( spectralAnalysis )
  x     <-  getSpectraInTimeExample()
  spectra    <-  getSpectra( object )
  y = NULL
  
  plot_ly( z = getSpectra( object ) , y = getTimePoints( object ) ,  x = getWavelengths( object ) )  %>% add_surface()
  
  
  plot  <-  plot.spectrum3D(  x = getSpectraInTimeExample() , y = NULL ,  timeUnit = "minutes" , timePointsAlt = TRUE , colorPalette )
  plot2  <- do.call(plot.spectrum3D , args = list( x = getSpectraInTimeExample() , y = NULL ,  timeUnit = "minutes" , timePointsAlt = TRUE , colorPalette )) 
}


plot.spectrum3D           <-  function( x , y , timeUnit , timePointsAlt , colorPalette ) {
  spectra                 <-  getSpectra( x )
  timePoints              <-  getTimePoints( x , timeUnit = timeUnit  , timePointsAlt = timePointsAlt  )
  wavelength              <-  getWavelengths( x )
  nColors                 <-  100 # large number plot works fine              
  
  p                       <-  plot_ly( z = spectra , y = timePoints , x = wavelength , colors = viridis_pal(option = colorPalette )( nColors ) ) %>% add_surface()
  plot                    <-  p %>% layout( scene = list( 
          xaxis = plotlyAxis( "x = Wavelength" ) ,
          zaxis = plotlyAxis( "z = Response" ) ,
          yaxis = plotlyAxis( paste0("y = Time in " , timeUnit )  )
      )
  ) 
  return( plot )
}


if( 0 == 1 ) {
  x            <-  getSpectraInTimeExample()[ , e( seq( 200 , 400 , by = 50  ) )] # use closest element finding
  getWavelengths( x )
}

plot.wavelengths          <-  function( x , y ,  timeUnit  , timePointsAlt , colorPalette  ) {
  spectraFlat             <-  as.data.frame( x ,  timeUnit = timeUnit , timePointsAlt = timePointsAlt ) 
  spectraFlat$Wavelength  <-  paste0( "Wavelength = ",  spectraFlat$wavelength )
  nColors                 <-  length( getWavelengths( x ) )
  p                      <-  plot_ly(spectraFlat ,  x = ~timePoints , y = ~response  , type = "scatter" ,
      color = ~Wavelength  ,  symbol =  ~Wavelength  , 
      mode = 'lines+markers' ,
      colors = viridis_pal(option = colorPalette )( nColors ) )
  plot                   <- p    %>% layout( 
      xaxis = plotlyAxis( paste0("Time in " , timeUnit ) ) ,
      yaxis = plotlyAxis( "Response" )
  ) 
  return( plot )
  
}



plot.times                <-  function( x , y ,  timeUnit  , timePointsAlt , colorPalette  ) {
  spectraFlat             <-  as.data.frame( x ,  timeUnit = timeUnit , timePointsAlt = timePointsAlt ) 
  spectraFlat$Time        <-  paste0( "Time = ",  spectraFlat$timePoints , timeUnit  )
  nColors                 <-  length( getTimePoints( x ) )
  p                       <-  plot_ly(spectraFlat ,  x = ~wavelength , y = ~response  , type = "scatter" ,
      color = ~Time , symbol = ~Time , mode = 'lines+markers' , colors = viridis_pal(option = colorPalette )( nColors )
  ) 
  plot                    <- p    %>% layout( 
      xaxis =  plotlyAxis( "Wavelength" ) ,
      yaxis =  plotlyAxis( "Response" )
  ) 
  return( plot )
  
}




### plotting multiple spectra in a list 

if( 0 == 1 ){
  spectra1  <-  getSpectraInTimeExample()
  spectra2  <-  getSpectraInTimeExample()
  spectra2@spectra  <-  spectra1@spectra  * 1.1 
  spectra2@experimentName   = "ablommaert_test2"
  spectra3  <-  getSpectraInTimeExample()
  spectra3@spectra  <-  spectra1@spectra  * 1.3
  spectra3@experimentName  <-  "ablommaert_test3"
  
  listOfSpectra    <-  list( spectra1 , spectra2 , spectra3 )
  times     =  0:3 
  timeUnit = "hours"
  x  = listOfSpectra
  colors = "D"
  
  plot( listOfSpectra , y = NULL ,  time = c(1) , timeUnit = "hours", colors = "A" )
  plot( listOfSpectra  ,  time = c(1) , timeUnit = "hours", colors = "A" )
  
  
  x
  args   <-  list(  times     =  0:3 ,
  timeUnit = "hours" ,  colors = "D")
}


plotListOfSpectra        <-  function( dataList , y = "not important" ,   times = 0  , timeUnit = "seconds",  timePointsAlt = FALSE, colors = "D" ){
  ## checks 
  # matching spectra
  nExperiments          <-  length( dataList )
  nTimes                <-  length( times )
  wavelengthAxis        <-  lapply( dataList , getWavelengths ) 
  wavelengths           <-  wavelengthAxis[[ 1 ]]
  identicalWavelengths  <-  sapply( wavelengthAxis , function( ax ) { identical( ax , wavelengths )}) 
  if( ! all( identicalWavelengths ) ){stop( "unequal wavelength axis over experiments" )}
  
  nWavelengths          <-  length( wavelengths )
  ## subset data
  experimentNames       <-  lapply( dataList , getExperimentName )
  dataSubset            <-  lapply( dataList , FUN= function( spec ){
      specSelect        <-  spec[ e( times ) , , timeUnit = timeUnit , timePointsAlt = timePointsAlt ] 
      specData          <-  getSpectra( specSelect )
      specData
    } 
  ) 
  
  dataToPlotList        <-  mapply( FUN = function( experiment , dataSet ){
      response          <-  as.vector( t( dataSet ) )
      Time              <-  paste0( "time = " , rep( times , rep( nWavelengths , nTimes ) ) ," " ,timeUnit )
      plotDataExp       <-  data.frame( wavelength = wavelengths, response = response , experiment = experiment , Time = Time )
      
    } ,
    experiment  = experimentNames , dataSet = dataSubset , SIMPLIFY = FALSE  )
  
  dataToPlot            <-  data.frame( rbindlist( dataToPlotList  ) )
  
  
#     ##construct plotly plot  # PLOTLY does not support this double grouping, make ggplot instead
#     colorPalette            <-  colors
#     nColors                 <-  nExperiments
#     p                       <-  plot_ly( dataToPlot ,  x = ~wavelength , y = ~response  , type = "scatter" ,
#       color = ~Time , symbol = ~experiment , mode = 'lines+markers' ,
#       colors = viridis_pal(option = colorPalette )( nColors ) 
#     ) 
#     plot                    <- p    %>% layout( 
#       xaxis =  plotlyAxis( "Wavelength" ) ,
#       yaxis =  plotlyAxis( "Response" )
#     ) 
  
  with( data = dataToPlot  , {
      experimentPlot      <-  ggplot( data = dataToPlot , aes( x = wavelength , y = response , group = interaction( Time , experiment )  ) ) +
        geom_line( aes( color = experiment , linetype = Time ) ) +
        scale_color_viridis( option = colors , discrete = TRUE  )
      return( experimentPlot )
    }
  )
}


#' @rdname SpectraInTime-class                  
#' @export 
#' @param ... additional argument, for plotting a list of spectra one can use:
#' \itemize{
#'  \item \code{times} numeric vector of time points to plot
#'  \item \code{timeUnit} time unit for \code{times} default to "seconds"
#'  \item \code{timePointsAlt} logical value indicating whether alternative time axis should be used, defaults to \code{FALSE}
#' }
#' @importFrom data.table rbindlist
#' @import ggplot2 
#' @importFrom viridis scale_color_viridis
#' 
#' @examples
#'  ## plotting a list of spectra
#'     
#'  listOfSpectra     <-  getListOfSpectraExample()
#'  plot( listOfSpectra , times = 1 , timeUnit = "hours" )
#'  plot( listOfSpectra , times = 1 , timeUnit = "hours" , timePointsAlt = TRUE  )
#'  plot( listOfSpectra , times = 1:3 , timeUnit = "hours" , colors = "B" ) 
#' @method plot list
#' @name plot
plot.list               <-  function( x  , ... ){
  arguments             <-  list( ... )
  argsExtended          <-  c(  arguments , list( dataList = x ) )
  
  ## check class
  allSpectralClass      <-  checkIdenticalClass( x , "SpectraInTime" )
  if( allSpectralClass ){
    resultPlot    <-  do.call( plotListOfSpectra , args = argsExtended )
  } else {
    stop( "plotting method not defined or objects of different class" )
  }
  
  ## pass to class specific method
  resultPlot
}




