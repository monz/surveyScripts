# import external libraries
library("dplyr")
library("tidyr")

# import functions
source("rworkspace/surveyTest/getCleanData.R")
source("rworkspace/surveyTest/questionnaireUtil.R")

prepareSurveyData <- function(data, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, overuseThreshold) {
  overusedSameResponse <- apply(questionnaireResponses, 1, function(x) sameResponseOveruse(x, overuseThreshold))
  surveyData <- mutate(data, overusedResponse = sapply(overusedSameResponse, function(x) {ifelse(is.data.frame(x), levels(x[[1]])[x[[1]]], NA)}))
  surveyData <- mutate(surveyData, overusedResponseCount = sapply(overusedSameResponse, function(x) {ifelse(is.data.frame(x), x[[2]], NA)}))
  
  # get information of personality count - personality questionnaire results are ambiguous
  personalityCount <- getPersonalityCount(questionnaireResponses, isInvertedBigFive, attributesBigFive, selectionMatrixBigFive)
  surveyData <- mutate(surveyData, personalityCount = personalityCount)
  
  # calculate new values
  dateFormat = "%Y-%m-%d %H:%M:%S"
  surveyData <- mutate(surveyData, timeToFinish = (
    difftime(strptime(surveyData$Date.Submitted, format = dateFormat),
             strptime(surveyData$Time.Started, format = dateFormat), units = "secs")))
  
  # convert timezone of submit date/time
  surveyData <- mutate(surveyData, dateSubmitted = (
    format(as.POSIXct(strptime(surveyData$Date.Submitted, tz = "America/New_York", format = dateFormat)), tz = "Europe/Berlin")))
  
  return(surveyData)
}

cleanTestGroup <- function(data, isAdaptedSurvey, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc) {
  # extract clean test group data
  testGroupData <- rename(data, opinion_after = opinion_after_test)
  if (isAdaptedSurvey) {
    testGroupData <- rename(testGroupData, timeOnText = Time.spent.on.page..Test.text)
  }
  testGroupData <- bind_cols(testGroupData, getPersonalityValues(questionnaireResponses, isInvertedBigFive, attributesBigFive, selectionMatrixBigFive))
  testGroupDataClean <- getCleanData(testGroupData, isAdaptedSurvey, "test", isInvertedNfc, breaksNfc, labelsNfc)
  
  return(testGroupDataClean)
}

cleanControlGroup <- function(data, isAdaptedSurvey, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc) {
  # extract clean control group data
  controlGroupData <- rename(data, opinion_after = opinion_after_control)
  if (isAdaptedSurvey) {
    controlGroupData <- rename(controlGroupData, timeOnText = Time.spent.on.page..Control.text)
  }
  personalities <- getPersonalities(questionnaireResponses, isInvertedBigFive, attributesBigFive, selectionMatrixBigFive)
  controlGroupData <- mutate(controlGroupData, personality = personalities)
  controlGroupData <- bind_cols(controlGroupData, getPersonalityValues(questionnaireResponses, isInvertedBigFive, attributesBigFive, selectionMatrixBigFive))
  controlGroupDataClean <- getCleanData(controlGroupData, isAdaptedSurvey, "control", isInvertedNfc, breaksNfc, labelsNfc)
  
  return(controlGroupDataClean)
}

# get information of responses which seem to have invalid questionnaire answers
getSurveyData <- function(data, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc, overuseThreshold = 8) {
  questionnaireResponses <- select(data, X1.personality_questionnaire:X15.personality_questionnaire)
  surveyData <- prepareSurveyData(data, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, overuseThreshold)
  
  testGroupData <- filter(surveyData, Status == "Complete", !is.na(opinion_after_test))
  questionnaireResponses <- select(testGroupData, X1.personality_questionnaire:X15.personality_questionnaire)
  testGroupDataClean <- cleanTestGroup(testGroupData, FALSE, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc)
  
  controlGroupData <- filter(surveyData, Status == "Complete", is.na(opinion_after_test))
  questionnaireResponses <- select(controlGroupData, X1.personality_questionnaire:X15.personality_questionnaire)
  controlGroupDataClean <- cleanControlGroup(controlGroupData, FALSE, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc)
  
  # return values
  return(list(testGroupDataClean=testGroupDataClean,controlGroupDataClean=controlGroupDataClean))
}

# get information of responses which seem to have invalid questionnaire answers
getSurveyDataAdapted <- function(data, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc, overuseThreshold = 8) {
  questionnaireResponses <- select(data, X1.personality_questionnaire:X16.personality_questionnaire)
  surveyData <- prepareSurveyData(data, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, overuseThreshold)
  
  testGroupData <- filter(surveyData, Status == "Complete", !is.na(opinion_after_test))
  questionnaireResponses <- select(testGroupData, X1.personality_questionnaire:X16.personality_questionnaire)
  testGroupDataClean <- cleanTestGroup(testGroupData, TRUE, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc)
  
  controlGroupData <- filter(surveyData, Status == "Complete", is.na(opinion_after_test))
  questionnaireResponses <- select(controlGroupData, X1.personality_questionnaire:X16.personality_questionnaire)
  controlGroupDataClean <- cleanControlGroup(controlGroupData, TRUE, questionnaireResponses, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc)
  
  # return values
  return(list(testGroupDataClean=testGroupDataClean,controlGroupDataClean=controlGroupDataClean))
}