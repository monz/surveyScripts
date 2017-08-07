# import functions
source("rworkspace/surveyTest/settings.R")
source("rworkspace/surveyTest/surveyEvaluation.R")
source("rworkspace/surveyTest/surveyEvaluation_adapted.R")

# import csv data
surveyDataCsvTHI <- read.csv("SurveyExport.csv", na.strings = c("", " "))
surveyDataCsvKU <- read.csv("SurveyExport_KU.csv", na.strings = c("", " "))
surveyDataCsvHamm <- read.csv("SurveyExport_Hamm.csv", na.strings = c("", " "))
surveyDataCsvBotar <- read.csv("SurveyExport_Botar.csv", na.strings = c("", " "))
#surveyDataCsvKrinner <- read.csv("SurveyExport_Krinner.csv", na.strings = c("", " "))

# combined csv list
combinedCsvData <- list(surveyDataCsvBotar, surveyDataCsvHamm, surveyDataCsvKU)

# get clean data
surveyData <- getSurveyData(surveyDataCsvTHI, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc)
surveyDataList <- lapply(combinedCsvData, function(x) getSurveyDataAdapted(x, selectionMatrixBigFiveAdapted, isInvertedNfc, isInvertedBigFiveAdapted, attributesBigFive, breaksNfc, labelsNfc))
surveyDataList[[length(surveyDataList)+1]] <- surveyData
surveyDataList <- rev(surveyDataList)

# get combinded data frames
combinedSurveyData <- lapply(surveyDataList, function(x) bind_rows(x[1], x[2]))

### age, mean and sd
print(lapply(combinedSurveyData, function(x) summarise(x, mean(age), sd(age))))

### personality
print(lapply(combinedSurveyData, function(x) xtabs(~ personality, data = x)))