# import external libraries
library(dplyr)
library(gmodels)

# import functions
source("rworkspace/surveyTest/surveyEvaluation.R")

# common data
breaksOpinionChanged = c(-3,-1,0,2)
breaksOpinionChanged2 = c(-3,0,2)

# add group identifier for convenience
testGroupDataClean <- mutate(testGroupDataClean, group = "test")
controlGroupDataClean <- mutate(controlGroupDataClean, group = "control")

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
excludeData <- testGroupDataClean$Response.ID[testGroupDataClean$personalityCount > 1]
testGroupDataClean <- filter(testGroupDataClean, !(Response.ID %in% excludeData))

# combine data of groups
surveyDataClean <- rbind(testGroupDataClean, controlGroupDataClean)

## hypothesis testing
# 
#### neg+neut, pos
testOpinionChangedByGroup <- fisher.test(table(surveyDataClean$group, cut(surveyDataClean$opinion_changed, breaks = breaksOpinionChanged2)), alternative = "greater")
CrossTable(table(surveyDataClean$group, cut(surveyDataClean$opinion_changed, breaks = breaksOpinionChanged2)), fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#### pos, neg, neut
# # opinion changed by group - divided into 5 groups: g elem [-2, 2]
# testOpinionChangedByGroup <- fisher.test(table(surveyDataClean$group, surveyDataClean$opinion_changed))
# # opinion changed by group - divided into 3 groups: neg, neut, pos
# testOpinionChangedByGroupSimple <- fisher.test(table(surveyDataClean$group, cut(surveyDataClean$opinion_changed, breaks = breaksOpinionChanged)))
# 
# # opinion changed by nfc - used data from test and control combined
# testOpinionChangedByNfcOverallSimple <- fisher.test(table(surveyDataClean$nfcR, cut(surveyDataClean$opinion_changed, breaks = breaksOpinionChanged)))
# # opinion changed by nfc - used data from test
# testOpinionChangedByNfcTestSimple <- fisher.test(table(testGroupDataClean$nfcR, cut(testGroupDataClean$opinion_changed, breaks = breaksOpinionChanged)))
# # opinion changed by nfc - used data from control
# testOpinionChangedByNfcControlSimple <- fisher.test(table(controlGroupDataClean$nfcR, cut(controlGroupDataClean$opinion_changed, breaks = breaksOpinionChanged)))
# 
# # opinion changed by personality used data from test
# testOpinionChangedByPersonalityTestSimple <- fisher.test(table(testGroupDataClean$personality, cut(testGroupDataClean$opinion_changed, breaksOpinionChanged)))
# # opinion changed by personality used data from control
# testOpinionChangedByPersonalityControlSimple <- fisher.test(table(controlGroupDataClean$personality, cut(controlGroupDataClean$opinion_changed, breaksOpinionChanged)))
