personalityTypesStatistics <- function(surveyData, personalityTypes) {
  occurences <- sapply(personalityTypes, function(x) length(which(surveyData$personality == x) ))
  distribution <- sapply(occurences, function(x) x/sum(occurences))
  
  logDecisionRemained <- surveyData$opinion_after_test == surveyData$opinion_before
  decisionRemainedCount <- sapply(personalityTypes, function(x) length(which(surveyData$personality == x & logDecisionRemained) ))
  decisionRemainedPercent <- mapply(function(x, y) y / x, occurences, decisionRemainedCount)
  
  logDecisionChanged <- surveyData$opinion_after_test != surveyData$opinion_before
  decisionChangedCount <- sapply(personalityTypes, function(x) length(which(surveyData$personality == x & logDecisionChanged) ))
  decisionChangedPercent <- mapply(function(x, y) y / x, occurences, decisionChangedCount)
  
  logDecisionChangedPositive <- (surveyData$opinion_after_test == 1 & surveyData$opinion_before >= 2) | (surveyData$opinion_after_test == 2 & surveyData$opinion_before == 3)
  decisionChangedPositiveCount <- sapply(personalityTypes, function(x) length(which(surveyData$personality == x & logDecisionChangedPositive) ))
  decisionChangedPositivePercent <- mapply(function(x, y) y / x, decisionChangedCount, decisionChangedPositiveCount)
  
  logDecisionChangedNegative <- (surveyData$opinion_after_test == 3 & surveyData$opinion_before <= 2) | (surveyData$opinion_after_test == 2 & surveyData$opinion_before == 1)
  decisionChangedNegativeCount <- sapply(personalityTypes, function(x) length(which(surveyData$personality == x & logDecisionChangedNegative) ))
  decisionChangedNegativePercent <- mapply(function(x, y) y / x, decisionChangedCount, decisionChangedNegativeCount)
  
  # create data frame
  df <- data.frame(personalityTypes, occurences, distribution,
                   decisionRemainedCount, decisionRemainedPercent,
                   decisionChangedCount, decisionChangedPercent, 
                   decisionChangedPositiveCount, decisionChangedPositivePercent,
                   decisionChangedNegativeCount, decisionChangedNegativePercent)
  colnames(df) <- c("personalityType", "occurences", "distribution",
                    "decisionRemainedCount", "decisionRemainedPercent",
                    "decisionChangedCount", "decisionChangedPercent",
                    "decisionChangedPositiveCount", "decisionChangedPositivePercent",
                    "decisionChangedNegativeCount", "decisionChangedNegativePercent")
  
  # return data frame
  return(df)
}