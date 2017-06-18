# import external libraries
library("dplyr")
library("tidyr")

# import functions
source("rworkspace/surveyTest/getCleanData.R")
source("rworkspace/surveyTest/questionnaireUtil.R")

# read file
surveyData <- read.csv("SurveyExport.csv", na.strings = c("", " "))

# common data
isInvertedNfc <- c(TRUE, FALSE, FALSE, TRUE) # points out which values must get inverted
isInvertedBigFive <- logical(length = 15)
isInvertedBigFive[c(3,6,8,15)] <- TRUE
attributesBigFive <- c("O","C","E","A","N")
selectionMatrixBigFive <- matrix(c(4,1,2,3,5,10,8,6,7,11,14,12,9,13,15), ncol = 3)

## functions
getPersonalities <- function(questionnaireResults) {
  questionnaireResponses <- getResponseValue(questionnaireResults, isInvertedBigFive)
  personalityValues <- apply(questionnaireResponses, 1, function(x) getEvaluatedValue(x, attributesBigFive, selectionMatrixBigFive, mean))
  personalities <- getAttribute(personalityValues, max)
  
  return(personalities)
}

getPersonalityCount <- function(questionnaireResults) {
  questionnaireResponses <- getResponseValue(questionnaireResults, isInvertedBigFive)
  personalityValues <- apply(questionnaireResponses, 1, function(x) getEvaluatedValue(x, attributesBigFive, selectionMatrixBigFive, mean))
  maxPersonalityValueCount <- getCountOfMaxPersonalityValues(personalityValues)
  
  return(maxPersonalityValueCount)
}

# get information of responses which seem to have invalid questionnaire answers
overuseThreshold <- 8
overusedSameResponse <- apply(select(surveyData, X1.personality_questionnaire:X15.personality_questionnaire), 1, function(x) sameResponseOveruse(x, overuseThreshold))
surveyData <- mutate(surveyData, overusedResponse = sapply(overusedSameResponse, function(x) {ifelse(is.data.frame(x), levels(x[[1]])[x[[1]]], NA)}))
surveyData <- mutate(surveyData, overusedResponseCount = sapply(overusedSameResponse, function(x) {ifelse(is.data.frame(x), x[[2]], NA)}))

# get information of personality count - personality questionnaire results are ambiguous
personalityCount <- getPersonalityCount(select(surveyData, X1.personality_questionnaire:X15.personality_questionnaire))
surveyData <- mutate(surveyData, personalityCount = personalityCount)

# calculate new values
dateFormat = "%Y-%m-%d %H:%M:%S"
surveyData <- mutate(surveyData, timeToFinish = (
  difftime(strptime(surveyData$Date.Submitted, format = dateFormat),
    strptime(surveyData$Time.Started, format = dateFormat), units = "secs")))

# extract clean test group data
testGroupData <- filter(surveyData, Status == "Complete", !is.na(opinion_after_test))
testGroupData <- rename(testGroupData, opinion_after = opinion_after_test)
testGroupDataClean <- getCleanData(testGroupData)


# extract clean control group data
controlGroupData <- filter(surveyData, Status == "Complete", is.na(opinion_after_test))
controlGroupData <- rename(controlGroupData, opinion_after = opinion_after_control)
personalities <- getPersonalities(select(controlGroupData, X1.personality_questionnaire:X15.personality_questionnaire))
controlGroupData <- mutate(controlGroupData, personality = personalities)
controlGroupDataClean <- getCleanData(controlGroupData)

# remove unused variables from workspace
rm(testGroupData, controlGroupData, personalities)
