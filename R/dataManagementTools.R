###############################################################################
# Project: Spectral Analysis
# 
# Description:
#
# Collection of functions to read in the data and to perform some basic data steps
# 
# Author: rvanoirbeek
#
# Maintainer: rvanoirbeek <adriaan.blommaert@openanalytics.eu> 
#
# Version: 2.0
#
###############################################################################

## general dependencies: 

#' @importFrom  plyr   aaply  llply  laply  alply  
NULL


# Some internal functions are defined first, which will be used in the main functions of this R file

strsplitVectorElement <- function(x, splitter, element){
  unname(aaply(x, 1, 
    function(xx) unlist(strsplit(xx, splitter, fixed = T))[element]
  ))
}	

substrLeft <- function(x, n){
	unname(aaply(x, 1, 
					function(xx) substr(xx, 1, n)
			))
}

substrMiddle <- function(x, nRemoveLeft, nRemoveRight){
  xWithoutRightPart <- unname(aaply(as.matrix(x), 1, 
    function(xx) substr(xx, 1, nchar(xx)-nRemoveRight)
  ))
  unname(aaply(as.matrix(xWithoutRightPart), 1, 
    function(xx) substr(xx, 1+nRemoveLeft, nchar(xx))
  ))
}

substrRight <- function(x, n){
  unname(aaply(as.matrix(x), 1, 
    function(xx) substr(xx, (nchar(xx)-n+1), nchar(xx))  
  ))
}

positionMatchingIDs <- function(x, uniqueIDs){
  unname(aaply(as.matrix(x), 1, 
    function(xx){
      result <- which(uniqueIDs == xx)
      if(!length(result)) result <- NA
      result
    }
  ))
}

#transformExperimentNameToUPLC <- function(spectralDataObject){
#  if(length(spectralDataObject) == 1){
#	stopIfNotSpectralData(spectralDataObject)
#  	experimentNameParts <- unlist(strsplit(spectralDataObject@experimentName, '-', fixed = T))[-1]
#  	return(paste(experimentNameParts[1], as.integer(experimentNameParts[2]), sep = "-"))
#  } else if (length(spectralDataObject) > 1) {
#	stopIfNotListOfSpectralData(spectralDataObject)
#	experimentNames <- rep(NA, length(spectralDataObject))
#	for(iList in 1:length(spectralDataObject)){
#	  experimentNameParts <- unlist(strsplit(spectralDataObject[[iList]]@experimentName, '-', fixed = T))[-1]
#	  experimentNames[iList] <- (paste(experimentNameParts[1], as.integer(experimentNameParts[2]), sep = "-"))
#	}
#	return(experimentNames)
#  } else {
#	stop("Please provide a valid list of 'spectralData' objects to this function!")
#  }
#}
#
strsplitVectorLength <- function(x, splitter){
  unname(aaply(x, 1, 
          function(xx) length(unlist(strsplit(xx, splitter, fixed = T)))
      ))
}

strsplitVectorLength <- function(x, splitter){
  unname(aaply(x, 1, 
          function(xx) length(unlist(strsplit(xx, splitter, fixed = T)))
      ))
}

selectPartsStrings <- function(allFiles, splitter, selectedElements){
  result <- rep(NA, length(allFiles))
  for(iSplit in 1:length(allFiles)){
    result[iSplit] <- paste(unlist(strsplit(allFiles[iSplit], splitter, fixed = T))[selectedElements[[iSplit]]], collapse = splitter)
  }
  result
}

# The standard method in R is way to slow such that a method adapted to our specific needs was developed, hereby increasing the calculation speed by a factor of 7.25

timeInSeconds <- function(x, element){
  unname(aaply(x, 1, function(xx)
    sum(as.numeric(unlist(strsplit(xx, ":", fixed = T)))*c(3600, 60, 1))
  ))
}	
#
#
#
###TODO AB: is this used?
###' An S4 class to represent meta data of spectral measurements
###' 
###' @slot metaData  a frame with metadata
###' @slot experimentName character vector name experiment
###' @slot startData character value with date time insight
###' 
##setClass("metaData",
##    representation = representation (
##        metaData = "data.frame",
##        experimentName = "character",
##        startData = "character"
##    )
##)
##
##stopIfNotSpectralData <- function(object){
##	if(!isSpectralData(object)) stop("Please convert your spectral data to the class 'spectralData'")
##}
##
##stopIfNotMetaData <- function(object){
##  if(!isMetaData(object)) stop("Please convert your metadata to the class 'metaData'")
##}
##
##stopIfNotListOfSpectralData <- function(list){
##	for(iList in 1:length(list)) if(!isSpectralData(list[[iList]])) stop("Please convert at least one element of your list of metadata to the class 'spectralData'")
##}
##
##stopIfNotListOfMetaData <- function(list){
##  for(iList in 1:length(list)) if(!isMetaData(list[[iList]])) stop("Please convert at least one element of your list of metadata to the class 'metaData'")
##}
##
##isSpectralData <- function(object){
##	is(object = object, class2 = "spectralData")
##}
##
##isMetaData <- function(object){
##  is(object = object, class2 = "metaData")
##}
##
##stopIfNotdfUPLC <- function(object){
##	if(!sum(names(object) %in% c("experimentName", "timePoints", "compoundName", "measurement")) == 4) stop("Please provide the output object of the 'generatingDataFrameUPLC' function to the 'dfUPLC' argument.")	
##}
##
##stopIfNotOutlierDetectionTimeSeriesObject <- function(object){
##  if(!sum(names(object) %in% c("ggplotObject", "ggplotObjectZoom", "ggplotObjectBegin", "ggplotObjectEnd", "outlyingTimePoints", "experimentName")) == 6) stop("Please provide the output object of the 'outlierDetectionTimeSeries' function to the 'resultOutlierDetectionList' argument.")
##}
#
##' Load all or a selection of SPC files from a given directory.
##' 
##'  
##' @param directoryFiles Character vector indicating the directory from which the files needs to be downloaded. Note that files with an other extension than '.spc' can be stored in this directory.
##' @param selectedFiles Character vector listing which files of the chosen directory (as expressed by the 'directoryFiles' argument) should be processed. This argument is used when one wants to process a subset of the spc files of the selected directory only. Note that one should add the complete file name to this list, including the file extension! This is an optional argument with as default value NULL, meaning that by default all files of the selected directory are considered. 
##' @return A list is returned of which each element contains a processed SPC file. Each SPC file is represented as a 'spectralData' S4 object. 
##' @examples 
##' directoryFiles <- file.path("", "home", "ablommaert", "git", "spectral-analysis-flow", "Spectral Analysis", 
##'   "Input client", "")
##' allSPCFiles <- loadAllSPCFiles(directoryFiles)
##' spcFile <- allSPCFiles[[1]]
##' @importFrom plyr laply
##' @export 
#loadAllSPCFiles <- function(directoryFiles, selectedFiles = NULL){
#
#  oldwd <- getwd()
#  setwd(directoryFiles)
#  
#  if(is.null(selectedFiles)){ 
#    selectedFiles <- list.files(directoryFiles)
#  } else {
#    allFiles <- list.files(directoryFiles)
#    for(iFile in 1:length(selectedFiles)){
#      differentPartsFileName <- unlist(strsplit(selectedFiles[iFile], ".", fixed = T))
#      if(tolower(differentPartsFileName[length(differentPartsFileName)]) != "spc") selectedFiles[iFile] <- paste(selectedFiles[iFile], '.spc', sep = "")
#      positionSelectedFile <- which(allFiles == selectedFiles[iFile]) 
#      if(length(positionSelectedFile) == 0) stop(paste(paste("File '", selectedFiles[iFile], sep = ""), "' is not part of the directory specified by the 'directoryFiles' argument. Please check for spelling errors and do not forget to add its file extension.", sep = ""))
#    }
#  }
#  
#  numberOfSplits               <-  strsplitVectorLength(selectedFiles, splitter = ".")
#  fileNames                    <-  selectPartsStrings(selectedFiles, splitter = ".", alply(numberOfSplits-1, 1, function(xx) 1:xx))
#  fileExtensions               <-  selectPartsStrings(selectedFiles, splitter = ".", as.list(numberOfSplits))
#  selectedFilesSPC             <-  selectedFiles[which(fileExtensions == "spc")] 
#  
#  experimentNames <- fileNames[which(fileExtensions == "spc")]
#  spcFiles <- list()
#  for(iRun in 1:length(selectedFilesSPC)){
#    spcFiles[[iRun]] <- readSPC(selectedFilesSPC[iRun], keys.log2data = TRUE)
#    spcFiles[[iRun]]@experimentName <- experimentNames[iRun]
#    #spcFiles[[iRun]]@startData <- spcFiles[[iRun]]@backgroundInformation[1,3]
#  }
#
#  on.exit(setwd(oldwd))	 
#  spcFiles
#  
#}
#
#extractExperimentNames <- function(spectralDataObjectList){
#	
#  if(length(spectralDataObjectList) == 1){
#	stopIfNotSpectralData(spectralDataObjectList)
#	return(spectralDataObjectList@experimentName)
#  } else if (length(spectralDataObjectList) > 1) {
#	stopIfNotListOfSpectralData(spectralDataObjectList)
#	return(laply(as.list(spectralDataObjectList), function(tmp){tmp@experimentName}))
#  } else {
#    stop("Please provide a valid list of 'spectralData' objects to this function!")
#  }
#}
#
##' Load all or a selection of XML files from a given directory.
##' 
##' This function automatically recognizes all the files bearing an '.xml' extension and returns a list in which each element corresponds to a different xml file.
##' @param directoryXML Character vector indicating the directory from which the files needs to be downloaded. Note that files with an other extension than '.xml' can be stored in this directory.
##' @param selectedFiles Character vector listing which files of the chosen directory (as expressed by the 'directoryXML' argument) should be processed. This argument is used when one wants to process a subset of the xml files of the selected directory only. Note that one should add the complete file name to this list, including the file extension! This is an optional argument with as default value NULL, meaning that by default all files of the selected directory are considered.
##' @param extractTr Logical variable indicating whether or not one wants to extract the reactor temperature from the XML file or not. The default value is TRUE.
##' @param extractTj Logical variable indicating whether or not one wants to extract the jacket temperature from the XML file or not. The default value is TRUE.
##' @param extractTrMinusTj Logical variable indicating whether or not one wants to extract the difference between the jacket and reactor temperature from the XML file or not. The default value is TRUE.
##' @param extractTset Logical variable indicating whether or not one wants to extract the set temperature from the XML file or not. The default value is TRUE.
##' @param extractRpm Logical variable indicating whether or not one wants to extract the rpm of the stirrer from the XML file or not. The default value is TRUE.
##' @return A list is returned of which each element contains a processed XML file. Each XML file is represented as an 'metaData' S4 object. 
##' @export 
##' @examples 
##' directoryXML <- file.path("", "home", "ablommaert", "git", "spectralAnalysis-R-package", "XML")
##' loadAllXMLFiles(directoryXML)
##' @export 
#loadAllXMLFiles <- function(directoryXML, selectedFiles = NULL, extractTr = TRUE, extractTj = TRUE, extractTrMinusTj = TRUE, extractTset = TRUE, extractRpm = TRUE){
#  
#  if(is.null(selectedFiles)){ 
#    selectedFiles <- list.files(directoryXML)
#  } else {
#    allFiles <- list.files(directoryXML)
#    for(iFile in 1:length(selectedFiles)){
#      differentPartsFileName <- unlist(strsplit(selectedFiles[iFile], ".", fixed = T))
#      if(tolower(differentPartsFileName[length(differentPartsFileName)]) != "xml") selectedFiles[iFile] <- paste(selectedFiles[iFile], '.xml', sep = "")
#      positionSelectedFile <- which(allFiles == selectedFiles[iFile]) 
#      if(length(positionSelectedFile) == 0) stop(paste(paste("File '", selectedFiles[iFile], sep = ""), "' is not part of the directory specified by the 'directoryXML' argument. Please check for spelling errors and do not forget to add its file extension.", sep = ""))
#    }
#  }
#  
#  numberOfSplits <- strsplitVectorLength(selectedFiles, splitter = ".")
#  fileNames <- selectPartsStrings(selectedFiles, splitter = ".", alply(numberOfSplits-1, 1, function(xx) 1:xx))
#  fileExtensions <- selectPartsStrings(selectedFiles, splitter = ".", as.list(numberOfSplits))
#  selectedFilesXML <- selectedFiles[which(fileExtensions == "xml")] 
#  
#  experimentNames <- fileNames[which(fileExtensions == "xml")]
#  xmlFiles <- list()
#  for(iRun in 1:length(selectedFilesXML)){
#    xmlFiles[[iRun]] <- readXML(directoryXML, selectedFilesXML[iRun], extractTr, extractTj, extractTrMinusTj, extractTset, extractRpm)
#  }
#  
#  xmlFiles
#  
#}
#
##'Read in xml files
##' 
##' @export  
##' @importFrom  xml2   read_xml  xml_attrs  xml_children  xml_text  xml_name   
#readXML <- function(directoryXML, fileNameXML, extractTr = TRUE, extractTj = TRUE, extractTrMinusTj = TRUE, extractTset = TRUE, extractRpm = TRUE){
#  
#  xmlFile <- read_xml(file.path(directoryXML, fileNameXML))
#  
#  experimentName <- xml_attrs(xmlFile)["Name"]
#  xmlLevel1 <- xml_children(xmlFile)
#  xmlLevel1Trends <- xmlLevel1[which(xml_name(xmlLevel1) == "Trends")]
#  xmlLevel2Trends <- xml_children(xmlLevel1Trends)
#  
#  startData <- xml_attrs(xmlLevel1)[[2]]
#  startData2Parts <- unlist(strsplit(startData, 'T', fixed = T))
#  startData <- paste(paste(rev(unlist(strsplit(startData2Parts[1], '-', fixed = T))), collapse = "/"), startData2Parts[2], sep = " ")
#  
#  listTrendsAttr <- xml_attrs(xmlLevel2Trends)
#  trendNames <- laply(listTrendsAttr, function(xx) xx[names(xx) == "Name"])
#  
#  timeTr <- 0
#  timeTj <- 0
#  timeTrMinusTj <- 0
#  timeTset <- 0
#  timeR <- 0
#
#  if(extractTr == TRUE){
#    elementTr <- xml_children(xmlLevel2Trends[which(trendNames == "Tr")])
#    textTr <- as.numeric(xml_text(elementTr))
#    timeTr <- timeInSecondsCpp(as.character(unlist(xml_attrs(elementTr))))
#  }
#  if(extractTj == TRUE){
#    elementTj <- xml_children(xmlLevel2Trends[which(trendNames == "Tj")])
#    textTj <- as.numeric(xml_text(elementTj))
#    timeTj <- timeInSecondsCpp(as.character(unlist(xml_attrs(elementTj))))
#  }
#  if(extractTrMinusTj == TRUE){
#    elementTrMinusTj <- xml_children(xmlLevel2Trends[which(trendNames == "Tr-Tj")])
#    textTrMinusTj <- as.numeric(xml_text(elementTrMinusTj))
#    timeTrMinusTj <- timeInSecondsCpp(as.character(unlist(xml_attrs(elementTrMinusTj))))
#  }
#  if(extractTset == TRUE){
#    elementTset <- xml_children(xmlLevel2Trends[which(trendNames == "Tset")])
#    textTset <- as.numeric(xml_text(elementTset))
#    timeTset <- timeInSecondsCpp(as.character(unlist(xml_attrs(elementTset))))
#  }
#  if(extractRpm == TRUE){
#    elementR <- xml_children(xmlLevel2Trends[which(trendNames == "R")])
#    textR <- as.numeric(xml_text(elementR))  
#    timeR <- timeInSecondsCpp(as.character(unlist(xml_attrs(elementR))))
#  }
#  
#  step <- 2
#  maxTimePoint <- max(timeTr, timeTj, timeTrMinusTj, timeTset, timeR)
#  timePoints <- seq(step, maxTimePoint, by = step)
#
#  tempReactor <- rep(NA, length(timePoints))
#  tempJacket <- rep(NA, length(timePoints))
#  differenceInTemp <- rep(NA, length(timePoints))
#  tempSet <- rep(NA, length(timePoints))
#  rpm <- rep(NA, length(timePoints))
#  
#  if(extractTr == TRUE) tempReactor <- alignTimePoints(timeTr, textTr, step, maxTimePoint)$noGaps
#  if(extractTj == TRUE) tempJacket <- alignTimePoints(timeTj, textTj, step, maxTimePoint)$noGaps
#  if(extractTrMinusTj == TRUE) differenceInTemp <- alignTimePoints(timeTrMinusTj, textTrMinusTj, step, maxTimePoint)$noGaps
#  if(extractTset == TRUE) tempSet <- alignTimePoints(timeTset, textTset, step, maxTimePoint)$noGaps
#  if(extractRpm == TRUE) rpm <- alignTimePoints(timeR, textR, step, maxTimePoint)$noGaps
#  
#  result <- data.frame(timePoints = timePoints, differenceInTemp = differenceInTemp, tempReactor = tempReactor, tempJacket = tempJacket, tempSet = tempSet, rpm = rpm)
#  return(new ("metaData", metaData = result, experimentName = experimentName, startData = startData))
#  
#}
#
#
#if( 0 == 1 ){
#directoryFiles <- file.path("", "home", "ablommaert", "git", "spectral-analysis-flow", "Spectral Analysis", 
#   "Input client")
# load(file.path(directoryFiles, 'T3686.UPLC.RData'))
# valuesUPLC <- T3686.UPLC[-c(1:11)]
# experimentNamesUPLC <- T3686.UPLC[,11]
# dfUPLC <- generatingDataFrameUPLC(experimentNamesUPLC, valuesUPLC)
# str(dfUPLC)
#  
#}
#
##' Construct a dataframe consisting of all relevant UPLC data.
##' 
##' A dataframe is constructed in which all the measured UPLC values (as stored in the 'valuesUPLC' matrix) are coupled to the experimental conditions under which they were generated (as expressed by the 'experimentNamesUPLC' vector).  
##' @param experimentNamesUPLC Character vector containing the names of all the experiments for which a dataframe needs to be constructed. The order of the experimental names is the same as the order of the experimental conditions of the valuesUPLC matrix (which are expressed by the rows of this matrix). 
##' @param valuesUPLC Matrix of numerical elements expressing the different UPLC values for the different time point and compound combination (each column refers to a different time point and compound), measured for each different experimental condition (each row represents a different experimental condition).
##' @return A dataframe is returned where each row corresponds to the UPLC measurement (column 4) of a unique experiment (column 1), time point (column 2) and compound combination (column 3). 
##' @examples 
##' directoryFiles <- file.path("", "home", "ablommaert", "git", "spectral-analysis-flow", "Spectral Analysis", 
##'   "Input client")
##' load(file.path(directoryFiles, 'T3686.UPLC.RData'))
##' valuesUPLC <- T3686.UPLC[-c(1:11)]
##' experimentNamesUPLC <- T3686.UPLC[,11]
##' dfUPLC <- generatingDataFrameUPLC(experimentNamesUPLC, valuesUPLC)
##' @export 
#generatingDataFrameUPLC <- function(experimentNamesUPLC, valuesUPLC){
#  #uniqueIDsUPLC <- strsplitVectorElement(as.character(experimentNamesUPLC), splitter = '-', 2)
#  uniqueIDsUPLC <- experimentNamesUPLC
#  chemicalNamesUPLC <- tolower(strsplitVectorElement(names(valuesUPLC), splitter = '.', 1))
#  timePointsNotCleanUPLC <- strsplitVectorElement(names(valuesUPLC), splitter = '.', 2) 
#  differentChemicalNamesUPLC <- unique(chemicalNamesUPLC)
#  timePointsUPLC <- as.numeric(substrMiddle(timePointsNotCleanUPLC, 1, 1))
#  differentTimePointsUPLC <- unique(timePointsUPLC)
#  
#  dfUPLC <- expand.grid(experimentName = uniqueIDsUPLC, timePoints = differentTimePointsUPLC, compoundName = differentChemicalNamesUPLC, measurement = NA)
#  for(iRun in 1:NROW(dfUPLC)){
#    theRowIndex <- which(dfUPLC$experimentName[iRun] == uniqueIDsUPLC)
#    possibleIndices <- which(dfUPLC$compoundName[iRun] == chemicalNamesUPLC)
#    theColumnIndex <- which(dfUPLC$timePoints[iRun] == timePointsUPLC[possibleIndices])
#    dfUPLC$measurement[iRun] <- valuesUPLC[theRowIndex, possibleIndices[theColumnIndex]] 
#  } 
#  dfUPLC$compoundName <- as.factor(dfUPLC$compoundName)
#  dfUPLC$experimentName <- as.factor(dfUPLC$experimentName)
#  dfUPLC
#}
#
##' Constructing a dataframe consisting of all relevant temperature data.
##' 
##' Extraction of the relevant temperature data from the input temperature file. This comprises the difference in temperature ('differenceInTemp') and the relative time ('relTime'). 
##' @param spectralDataObjectList List of objects of the 'spectralData' class. Temperature information will be collected for the selected experiments only.  
##' @param temperatureObject Object containing on each row the different temperature values of a given relative time point (i.e. the time since the beginning of the experiment). This object contains this information for multiple experiments, and the column names of this object express to which experiment the information pertains. 
##' @return A list of dataframes is returned where each element of the list corresponds to the complete relevant temperature information of a given experiment, i.e. the difference in temperature ('differenceInTemp') and the relative time ('relTime'). This information will be collected for the selected experiments only (as expressed by the 'allSPCFiles' argument).
##' @importFrom ggplot2 ggplot aes
##' @examples
##' directoryFiles <- file.path("", "home", "ablommaert", "git", "spectral-analysis-flow", "Spectral Analysis", 
##'   "Input client")
##' load(file.path(directoryFiles, 'T3686.T.rpm.RData'))
##' temperatureObject <- T3686.T.rpm
##' directoryFiles <- file.path("", "home", "ablommaert", "git", "spectral-analysis-flow", "Spectral Analysis", 
##'   "Input client", "")
##' allSPCFiles <- loadAllSPCFiles(directoryFiles)
##' dfTempDiff <- generatingDataFrameTemperatureDifference(allSPCFiles[1:5], temperatureObject)
##' @export 
#generatingDataFrameTemperatureDifference <- function(spectralDataObjectList, temperatureObject){
#  
#  uniqueIDsFiles <- substrRight(extractExperimentNames(spectralDataObjectList), 3)
#  
#  relTimeIndices <- which(substrRight(names(temperatureObject), 8) == 'Time.Rel')
#  uniqueIDsWithRelTime <- strsplitVectorElement(names(temperatureObject)[relTimeIndices], splitter = '.', 3)
#  PositionRelTimeIndicesFiles <- positionMatchingIDs(uniqueIDsFiles, uniqueIDsWithRelTime)
#  PositionRelTimeIndicesFiles <- PositionRelTimeIndicesFiles[!is.na(PositionRelTimeIndicesFiles)]
#  relTimeIndicesFiles <- relTimeIndices[PositionRelTimeIndicesFiles]
#  
#  differenceInTempIndices <- which(substrRight(names(temperatureObject), 9) == 'Tr.min.Tj')
#  uniqueIDsWithDifferenceInTemp <- strsplitVectorElement(names(temperatureObject)[differenceInTempIndices], splitter = '.', 3)
#  PositionDifferenceInTempIndicesFiles <- positionMatchingIDs(uniqueIDsFiles, uniqueIDsWithDifferenceInTemp)
#  PositionDifferenceInTempIndicesFiles <- PositionDifferenceInTempIndicesFiles[!is.na(PositionDifferenceInTempIndicesFiles)]
#  differenceInTempIndicesFiles <- differenceInTempIndices[PositionDifferenceInTempIndicesFiles]
#  
#  TempDiffFile <- list()
#  for(iRun in 1:length(differenceInTempIndicesFiles)){
#    if(is.na(differenceInTempIndicesFiles[iRun])){
#      TempDiffFile[[iRun]] <- NULL		
#    } else {
#      TempDiffFile[[iRun]] <- data.frame(differenceInTemp = unlist(temperatureObject[differenceInTempIndicesFiles[iRun]]), relTime = timeInSecondsCpp(as.character(temperatureObject[[relTimeIndicesFiles[iRun]]]))/3600)
#    } 
#  }
#  names(TempDiffFile) <- uniqueIDsWithRelTime[PositionDifferenceInTempIndicesFiles]
#  TempDiffFile
#}
#
#computingTimeLag <- function(spcFile, xmlFiles){
#  
#  spcFileStartDate <- unlist(strsplit(spcFile@startData, " ", fixed = T))[1]
#  spcFileStartHour <- unlist(strsplit(spcFile@startData, " ", fixed = T))[2]
#  xmlFilestartDate <- unlist(strsplit(xmlFiles[[1]]@startData, " ", fixed = T))[1]
#  xmlFilestartHour <- unlist(strsplit(xmlFiles[[1]]@startData, " ", fixed = T))[2]
#  
#  spcFileStartDate <- as.Date(strptime(spcFileStartDate, "%d/%m/%Y"))
#  xmlFilestartDate <- as.Date(strptime(xmlFilestartDate, "%d/%m/%Y"))
#  differenceInDate <- spcFileStartDate - xmlFilestartDate
#  differenceInSeconds <- as.numeric(differenceInDate*3600*24)
#  differenceInSeconds <- differenceInSeconds + timeInSeconds(spcFileStartHour) - timeInSeconds(xmlFilestartHour)
#  
#  return(differenceInSeconds)
#  
#}  
#
#detectingGapfreeIntervals <- function(values){
#  
#  blocks <- list()
#  if(length(values) == 1){
#    blocks[[1]] <- c(values[1], values[1])
#  } else if (length(values) > 1){
#    
#    listNotCompleted <- TRUE
#    index <- 2
#    blockIndex <- 1
#    beginCurrentBlock <- values[1]
#    endCurrentBlock <- values[1]
#    while(listNotCompleted){
#      if(values[index] == (endCurrentBlock + 1)){
#        endCurrentBlock <- values[index]
#        if(index == length(values)){
#          blocks[[blockIndex]] <- c(beginCurrentBlock, endCurrentBlock)
#          listNotCompleted <- FALSE
#        }  
#      } else {
#        blocks[[blockIndex]] <- c(beginCurrentBlock, endCurrentBlock)
#        blockIndex <- blockIndex + 1
#        if(index < length(values)){
#          beginCurrentBlock <- values[index + 1]
#          endCurrentBlock <- values[index + 1]
#          index <- index + 1
#        } else {
#          listNotCompleted <- FALSE
#        }  
#      }
#      index <- index + 1  
#    }
#  }
#  return(blocks)
#}



