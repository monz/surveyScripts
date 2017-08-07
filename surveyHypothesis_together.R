# import external libraries
library(dplyr)
library(gmodels)

# import functions
source("rworkspace/surveyTest/loadData.R")

## get rid of possible outliers
# omit extreme data - e.g. responses which overused same response
surveyDataCombinedClean <- filter(surveyDataCombined, is.na(overusedResponse))
# omit responses with total time to finish survey is below 120 seconds
usedTimeThreshold <- 120
surveyDataCombinedClean <- filter(surveyDataCombinedClean, timeToFinish >= usedTimeThreshold)
# omit responses with multiple possible personalities -because always the first personality
# was used if multiple personalities had the same maximum questionnaire result
surveyDataCombinedClean <- filter(surveyDataCombinedClean, !(group == "test" & personalityCount > 1))
# omit responses with time on "persuasive" text below 25 seconds
timeOnTextThreshold <- 25
surveyDataCombinedClean <- filter(surveyDataCombinedClean, timeOnText >= timeOnTextThreshold | is.na(timeOnText))
# create opinion changed with only 2 breakpoints
surveyDataCombinedClean <- mutate(surveyDataCombinedClean, opinion_changed_two = cut(opinion_changed, breaks = breaksOpinionChanged2))

## hypothesis testing
#### neg+neut, pos
opinionChangeTable <- xtabs(~ group + opinion_changed_two, data = surveyDataCombinedClean)

testOpinionChangedByGroup <- fisher.test(opinionChangeTable, alternative = "greater")
CrossTable(opinionChangeTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

## loglinear analysis
# extract table
opinionChangeNfcTable <- xtabs(~ group + opinion_changed_two + nfcR, data = surveyDataCombinedClean)
# create saturated model
saturated <- MASS::loglm(~ group*opinion_changed_two*nfcR, data = opinionChangeNfcTable, fit = TRUE)
# create next simpler model
#s1 <- loglm(~ group + opinion_changed_two + nfcR + group:opinion_changed_two + group:nfcR + nfcR:opinion_changed_two, data = opinionChangeNfcTable)
s1 <- update(saturated, .~. -group:opinion_changed_two:nfcR)
# create next simpler model
s2 <- update(s1, .~. -nfcR:opinion_changed_two)
s3 <- update(s1, .~. -group:nfcR) # treatment:nfcR
s4 <- update(s1, .~. -group:opinion_changed_two) # treatment:opinion_changed_two
# examine resultes
print(anova(saturated, s1))
print(anova(s1, s2))
print(anova(s1, s3))
print(anova(s1, s4))
# plot table
mosaicplot(opinionChangeNfcTable, shade = TRUE)
mosaicplot(opinionChangeTable, shade = TRUE)