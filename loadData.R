# import functions
source("rworkspace/surveyTest/settings.R")
source("rworkspace/surveyTest/getSurveyData.R")
source("rworkspace/surveyTest/removeOutliers.R")

# import csv data
surveyDataCsvTHI <- read.csv("SurveyExport.csv", na.strings = c("", " "))
surveyDataCsvKU <- read.csv("SurveyExport_KU.csv", na.strings = c("", " "))
surveyDataCsvHamm <- read.csv("SurveyExport_Hamm.csv", na.strings = c("", " "))
surveyDataCsvFb <- read.csv("SurveyExport_Fb.csv", na.strings = c("", " "))

# combined csv list
combinedAdaptedCsvDataList <- list(surveyDataCsvFb, surveyDataCsvHamm, surveyDataCsvKU)
combinedCsvDataList <- combinedAdaptedCsvDataList
combinedCsvDataList[[length(combinedCsvDataList)+1]] <- surveyDataCsvTHI
combinedCsvDataList <- rev(combinedCsvDataList)

# get clean data
surveyDataList <- getSurveyData(surveyDataCsvTHI, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc)
surveyDataAdaptedList <- lapply(combinedAdaptedCsvDataList, function(x) getSurveyDataAdapted(x, selectionMatrixBigFiveAdapted, isInvertedNfc, isInvertedBigFiveAdapted, attributesBigFive, breaksNfc, labelsNfc))
surveyDataAdaptedList[[length(surveyDataAdaptedList)+1]] <- surveyDataList
surveyDataAdaptedList <- rev(surveyDataAdaptedList)

# get combinded data frames
combinedSurveyDataList <- lapply(surveyDataAdaptedList, function(x) bind_rows(x[1], x[2]))

# combine data
surveyData <- combinedSurveyDataList[[1]]
surveyDataAdapted <- bind_rows(combinedSurveyDataList[[2]], combinedSurveyDataList[[3]], combinedSurveyDataList[[4]])
surveyDataCombined <- bind_rows(surveyData, surveyDataAdapted)

# clean data
surveyDataCombinedClean <- removeOutliers(surveyDataCombined, timeToFinishThreshold, timeOnTextThreshold)