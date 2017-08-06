# import external libraries
library(dplyr)
library(gmodels)
library(likert)
library(pastecs)
library(car)

# import functions
source("rworkspace/surveyTest/surveyEvaluation.R")
source("rworkspace/surveyTest/surveyEvaluation_adapted.R")

# common data
breaksOpinionChanged = c(-3,-1,0,2)
breaksOpinionChanged2 = c(-3,0,2)
breaksNfc = c(0, 3.84, 5.68, Inf) # former nfc mean +- 1sd
labelsNfc = c("niedrig", "mittel", "hoch")
# breaksNfc = c(0, 5.7, Inf)
# labelsNfc = c("niedrig", "hoch")

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
surveyData <- getSurveyData(surveyDataCsv, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc)
surveyDataAdapted <- getSurveyDataAdapted(surveyDataAdaptedCsv, selectionMatrixBigFiveAdapted, isInvertedNfc, isInvertedBigFiveAdapted, attributesBigFive, breaksNfc, labelsNfc)

# combine data
testGroupDataClean <- surveyData$testGroupDataClean
controlGroupDataClean <- surveyData$controlGroupDataClean
surveyDataClean <- rbind(testGroupDataClean, controlGroupDataClean)

testGroupDataAdaptedClean <- surveyDataAdapted$testGroupDataClean
controlGroupDataAdaptedClean <- surveyDataAdapted$controlGroupDataClean
surveyDataAdaptedClean <- rbind(testGroupDataAdaptedClean, controlGroupDataAdaptedClean)

#testGroupDataClean <- rbind(surveyData$testGroupDataClean, surveyDataAdapted$testGroupDataClean)
#controlGroupDataClean <- rbind(surveyData$controlGroupDataClean, surveyDataAdapted$controlGroupDataClean)
surveyDataCombinedClean <- rbind(surveyDataClean, surveyDataAdaptedClean)

## hypothesis timeSubmitted to opinion_changed(-3,0,2) within test group
rFromWilcox <- function(model, N) {
  z <- qnorm(model$p.value/2)
  r <- z / sqrt(N)
  cat(model$data.name, "Effect Size, r = ", r, "\n")
}
a <- select(surveyDataCombinedClean, dateSubmitted, group, personality, opinion_changed) %>% mutate(op = cut(opinion_changed, breaks = c(-3,0,2))) %>% mutate(timeSubmitted = as.numeric(hms(format(as.POSIXct(dateSubmitted, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")))/3600)
a <- filter(a, group == "test")
# test for normality
print(stat.desc(a, basic = FALSE, norm = TRUE))
# test for similar variances
print(leveneTest(timeSubmitted ~ op, data = a))
# wilcoxon
wm <- wilcox.test(timeSubmitted ~ op, data = a)
print(wm)
rFromWilcox(wm, nrow(a))
# same test, with direction
wm <- wilcox.test(timeSubmitted ~ op, data = a, conf.int = TRUE, alternative = "greater")
print(wm)
rFromWilcox(wm, nrow(a))