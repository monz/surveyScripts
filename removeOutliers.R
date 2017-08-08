# import external libraries
library(dplyr)

# import functions
source("rworkspace/surveyTest/settings.R")

removeOutliers <- function(data, timeToFinishThreshold = 120, timeOnTextThreshold = 25) {
  ## get rid of possible outliers
  # omit extreme data - e.g. responses which overused same response
  cleanedData <- filter(data, is.na(overusedResponse))
  # omit responses with total time to finish survey is below threshold
  cleanedData <- filter(cleanedData, timeToFinish >= timeToFinishThreshold)
  # omit responses with multiple possible personalities -because always the first personality
  # was used if multiple personalities had the same maximum questionnaire result
  cleanedData <- filter(cleanedData, !(group == "test" & personalityCount > 1))
  # omit responses with time on "persuasive" text below threshold
  cleanedData <- filter(cleanedData, timeOnText >= timeOnTextThreshold | is.na(timeOnText))

  return(cleanedData)
}