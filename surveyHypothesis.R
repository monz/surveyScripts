# import external libraries
library(dplyr)

# import functions
source("rworkspace/surveyTest/surveyEvaluation.R")

# common data
breaksOpinionChanged = c(-3,-1,0,2)

# add group identifier for convenience
testGroupDataClean <- mutate(testGroupDataClean, group = "test")
controlGroupDataClean <- mutate(controlGroupDataClean, group = "control")

surveyDataClean <- rbind(testGroupDataClean, controlGroupDataClean)

# omit extreme data - e.g. responses which overused same response
surveyDataClean <- filter(surveyDataClean, is.na(overusedResponse))
testGroupDataClean <- filter(testGroupDataClean, is.na(overusedResponse))
controlGroupDataClean <- filter(controlGroupDataClean, is.na(overusedResponse))
# omit responses with total time to finish survey is below 120 seconds
usedTimeThreshold <- 120
surveyDataClean <- filter(surveyDataClean, timeToFinish >= usedTimeThreshold)
testGroupDataClean <- filter(testGroupDataClean, timeToFinish >= usedTimeThreshold)
controlGroupDataClean <- filter(controlGroupDataClean, timeToFinish >= usedTimeThreshold)

## hypothesis testing
# 
# opinion changed by group - divided into 5 groups: g elem [-2, 2]
testOpinionChangedByGroup <- fisher.test(table(surveyDataClean$group, surveyDataClean$opinion_changed))
# opinion changed by group - divided into 3 groups: neg, neut, pos
testOpinionChangedByGroupSimple <- fisher.test(table(surveyDataClean$group, cut(surveyDataClean$opinion_changed, breaks = breaksOpinionChanged)))

# opinion changed by nfc - used data from test and control combined
testOpinionChangedByNfcOverallSimple <- fisher.test(table(surveyDataClean$nfcR, cut(surveyDataClean$opinion_changed, breaks = breaksOpinionChanged)))
# opinion changed by nfc - used data from test
testOpinionChangedByNfcTestSimple <- fisher.test(table(testGroupDataClean$nfcR, cut(testGroupDataClean$opinion_changed, breaks = breaksOpinionChanged)))
# opinion changed by nfc - used data from control
testOpinionChangedByNfcControlSimple <- fisher.test(table(controlGroupDataClean$nfcR, cut(controlGroupDataClean$opinion_changed, breaks = breaksOpinionChanged)))

# opinion changed by personality used data from test
testOpinionChangedByPersonalityTestSimple <- fisher.test(table(testGroupDataClean$personality, cut(testGroupDataClean$opinion_changed, breaksOpinionChanged)))
# opinion changed by personality used data from control
testOpinionChangedByPersonalityControlSimple <- fisher.test(table(controlGroupDataClean$personality, cut(controlGroupDataClean$opinion_changed, breaksOpinionChanged)))
