# import external libraries
library(dplyr)
library(gmodels)

# import functions
source("rworkspace/surveyTest/surveyEvaluation.R")
source("rworkspace/surveyTest/surveyEvaluation_adapted.R")

# common data
breaksOpinionChanged = c(-3,-1,0,2)
breaksOpinionChanged2 = c(-3,0,2)

isInvertedNfc <- c(TRUE, FALSE, FALSE, TRUE) # points out which values must get inverted
isInvertedBigFive <- logical(length = 15)
isInvertedBigFive[c(3,6,8,15)] <- TRUE
attributesBigFive <- c("O","C","E","A","N")
selectionMatrixBigFive <- matrix(c(4,1,2,3,5, 10,8,6,7,11, 14,12,9,13,15), ncol = 3)

isInvertedBigFiveAdapted <- logical(length = 16)
isInvertedBigFiveAdapted[c(3,6,8,15)] <- TRUE
selectionMatrixBigFiveAdapted <- matrix(c(4,1,2,3,5, 10,8,6,7,11, 14,12,9,13,15, 16,NA,NA,NA,NA), ncol = 4)

# read file
surveyDataCsv <- read.csv("SurveyExport.csv", na.strings = c("", " "))
surveyDataAdaptedCsv <- read.csv("SurveyExport_adapted.csv", na.strings = c("", " "))

# get survey data
surveyData <- getSurveyData(surveyDataCsv, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive)
surveyDataAdapted <- getSurveyDataAdapted(surveyDataAdaptedCsv, selectionMatrixBigFiveAdapted, isInvertedNfc, isInvertedBigFiveAdapted, attributesBigFive)

# combine data
testGroupDataClean <- rbind(surveyData$testGroupDataClean, surveyDataAdapted$testGroupDataClean)
controlGroupDataClean <- rbind(surveyData$controlGroupDataClean, surveyDataAdapted$controlGroupDataClean)

## get rid of possible outliers
# omit extreme data - e.g. responses which overused same response
testGroupDataClean <- filter(testGroupDataClean, is.na(overusedResponse))
controlGroupDataClean <- filter(controlGroupDataClean, is.na(overusedResponse))
# omit responses with total time to finish survey is below 120 seconds
usedTimeThreshold <- 120
testGroupDataClean <- filter(testGroupDataClean, timeToFinish >= usedTimeThreshold)
controlGroupDataClean <- filter(controlGroupDataClean, timeToFinish >= usedTimeThreshold)
# omit responses with multiple possible personalities -because always the first personality
# was used if multiple personalities had the same maximum questionnaire result
testGroupDataClean <- filter(testGroupDataClean, personalityCount < 2)
# omit responses with time on "persuasive" text below 25 seconds
timeOnTextThreshold <- 25
testGroupDataClean <- filter(testGroupDataClean, timeOnText >= timeOnTextThreshold | is.na(timeOnText))
controlGroupDataClean <- filter(controlGroupDataClean, timeOnText >= timeOnTextThreshold | is.na(timeOnText))

# combine data of groups
surveyDataClean <- rbind(testGroupDataClean, controlGroupDataClean)
## hypothesis testing
#### neg+neut, pos
testOpinionChangedByGroup <- fisher.test(table(surveyDataClean$group, cut(surveyDataClean$opinion_changed, breaks = breaksOpinionChanged2)), alternative = "greater")
CrossTable(table(surveyDataClean$group, cut(surveyDataClean$opinion_changed, breaks = breaksOpinionChanged2)), fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")