# import external libraries
library("dplyr")

# import functions
source("rworkspace/surveyTest/basicStatistics.R")
source("rworkspace/surveyTest/personalityTypesStatistics.R")

# read file
surveyData <- read.csv("SurveyExport.csv")

# calculate new values
dateFormat = "%Y-%m-%d %H:%M:%S"
surveyData <- mutate(surveyData, timeToFinish = (
  strptime(surveyData$Date.Submitted, format = dateFormat) - 
  strptime(surveyData$Time.Started, format = dateFormat)))

# common data
personalityTypes <- c("O","C","E","A","N")
responseCount <- nrow(surveyData)
isComplete <- surveyData$Status == "Complete"
isPartial <- surveyData$Status == "Partial"
completeResponseCount <-length(which(isComplete))
partialResponseCount <-length(which(isPartial))

surveyDataCompleted <- surveyData[isComplete,]
surveyDataPartial <- surveyData[isPartial,]

# test group
testStatistics <- basicStatistics(surveyDataCompleted, "opinion_after_test")
testStatisticsByPersonality <- personalityTypesStatistics(surveyDataCompleted, personalityTypes)

# control group
controlStatistics <- basicStatistics(surveyDataCompleted, "opinion_after_control")

# combine
combinedStatistics <- rbind(testStatistics, controlStatistics)
