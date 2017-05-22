# import external libraries
library(ggplot2)
library(dplyr)

# import functions
source("rworkspace/surveyTest/surveyEvaluation.R")

toNamedDataFrame <- function(data, names) {
  df <- data.frame(data)
  colnames(df) <- names
  return(df)
}

combineData <- function(...) {
  data.frame(table(rbind(...)))
}

# add group identifier for convenience
testGroupDataClean <- mutate(testGroupDataClean, group = "test")
controlGroupDataClean <- mutate(controlGroupDataClean, group = "control")

# extract data
countResponse <- toNamedDataFrame(nrow(surveyData), c("respones"))
countByStatus <- toNamedDataFrame(table(surveyData$Status), c("status", "count"))
countByPersonality <- combineData(select(testGroupDataClean, personality, group), select(controlGroupDataClean, personality, group))
countByNfc <- combineData(select(testGroupDataClean, nfcR, group), select(controlGroupDataClean, nfcR, group))
countDecisionByGroup <- data.frame(table(rbind(select(testGroupDataClean, opinion_changed, group), select(controlGroupDataClean, opinion_changed, group))))

## percentage values
countDecisionByPersonalityTest <- table(testGroupDataClean$personality, testGroupDataClean$opinion_changed)
countDecisionByPersonalityTest <- format(round(countDecisionByPersonalityTest/sum(countDecisionByPersonalityTest), 2), nsmall = 2)
countDecisionByPersonalityControl <- table(controlGroupDataClean$personality, controlGroupDataClean$opinion_changed)
countDecisionByPersonalityControl <- format(round(countDecisionByPersonalityControl/sum(countDecisionByPersonalityControl), 2), nsmall = 2)

countDecisionByNfcTest <- table(testGroupDataClean$nfcR, testGroupDataClean$opinion_changed)
countDecisionByNfcTest <- format(round(countDecisionByNfcTest/sum(countDecisionByNfcTest), 2), nsmall = 2)
countDecisionByNfcControl <- table(controlGroupDataClean$nfcR, controlGroupDataClean$opinion_changed)
countDecisionByNfcControl <- format(round(countDecisionByNfcControl/sum(countDecisionByNfcControl), 2), nsmall = 2)

# plots
surveyDataClean <- rbind(testGroupDataClean, controlGroupDataClean)
print(qplot(opinion_changed, data = surveyDataClean, facets = . ~ group, binwidth = 1, main = "Opinion Change by Group"))
print(qplot(opinion_changed, data = surveyDataClean, facets = group ~ nfcR, binwidth = 1, main = "Opinion Change by NFC"))
print(qplot(opinion_changed, data = surveyDataClean, facets = group ~ personality, binwidth = 1, main = "Opinion Change by Personality"))
print(qplot(opinion_changed, data = surveyDataClean, facets = group ~ sex, binwidth = 1, main = "Opinion Change by Sex"))
print(qplot(opinion_changed, data = surveyDataClean, facets = group ~ nfcR + personality, binwidth = 1, main = "Opinion Change by NFC and Personality"))
print(qplot(opinion_changed, data = subset(surveyDataClean, sex != "NA"), facets = group + sex ~ nfcR + personality, binwidth = 1, main = "Opinion Change by NFC, Personality and Sex"))
