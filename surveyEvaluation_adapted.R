# import external libraries
library("dplyr")
library("tidyr")

# import functions
source("rworkspace/surveyTest/getCleanData.R")
source("rworkspace/surveyTest/questionnaireUtil.R")

# get information of responses which seem to have invalid questionnaire answers
getSurveyDataAdapted <- function(surveyData, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc, overuseThreshold = 8) {
  overusedSameResponse <- apply(select(surveyData, X1.personality_questionnaire:X16.personality_questionnaire), 1, function(x) sameResponseOveruse(x, overuseThreshold))
  surveyData <- mutate(surveyData, overusedResponse = sapply(overusedSameResponse, function(x) {ifelse(is.data.frame(x), levels(x[[1]])[x[[1]]], NA)}))
  surveyData <- mutate(surveyData, overusedResponseCount = sapply(overusedSameResponse, function(x) {ifelse(is.data.frame(x), x[[2]], NA)}))
  
  # get information of personality count - personality questionnaire results are ambiguous
  personalityCount <- getPersonalityCount(select(surveyData, X1.personality_questionnaire:X16.personality_questionnaire), isInvertedBigFive, attributesBigFive, selectionMatrixBigFive)
  surveyData <- mutate(surveyData, personalityCount = personalityCount)
  
  # calculate new values
  dateFormat = "%Y-%m-%d %H:%M:%S"
  surveyData <- mutate(surveyData, timeToFinish = (
    difftime(strptime(surveyData$Date.Submitted, format = dateFormat),
             strptime(surveyData$Time.Started, format = dateFormat), units = "secs")))
  
  # extract clean test group data
  testGroupData <- filter(surveyData, Status == "Complete", !is.na(opinion_after_test))
  testGroupData <- rename(testGroupData, opinion_after = opinion_after_test)
  testGroupData <- rename(testGroupData, timeOnText = Time.spent.on.page..Test.text)
  testGroupDataClean <- getCleanDataAdapted(testGroupData, "test", isInvertedNfc, breaksNfc, labelsNfc)
  
  # extract clean control group data
  controlGroupData <- filter(surveyData, Status == "Complete", is.na(opinion_after_test))
  controlGroupData <- rename(controlGroupData, opinion_after = opinion_after_control)
  controlGroupData <- rename(controlGroupData, timeOnText = Time.spent.on.page..Control.text)
  personalities <- getPersonalities(select(controlGroupData, X1.personality_questionnaire:X16.personality_questionnaire), isInvertedBigFive, attributesBigFive, selectionMatrixBigFive)
  controlGroupData <- mutate(controlGroupData, personality = personalities)
  controlGroupDataClean <- getCleanDataAdapted(controlGroupData, "control", isInvertedNfc, breaksNfc, labelsNfc)
  
  # return values
  return(list(testGroupDataClean=testGroupDataClean,controlGroupDataClean=controlGroupDataClean))
}