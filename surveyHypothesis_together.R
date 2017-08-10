# import external libraries
library(gmodels)

# import functions
source("rworkspace/surveyTest/loadData.R")

## hypothesis testing
#### neg+neut, pos
opinionChangeTable <- xtabs(~ group + opinion_changed_two_levels, data = surveyDataCombinedClean)

testOpinionChangedByGroup <- fisher.test(opinionChangeTable, alternative = "greater")
CrossTable(opinionChangeTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

## loglinear analysis
# extract table
opinionChangeNfcTable <- xtabs(~ group + opinion_changed_two_levels + nfcR, data = surveyDataCombinedClean)
# create saturated model
saturated <- MASS::loglm(~ group*opinion_changed_two_levels*nfcR, data = opinionChangeNfcTable, fit = TRUE)
# create next simpler model
#s1 <- loglm(~ group + opinion_changed_two + nfcR + group:opinion_changed_two + group:nfcR + nfcR:opinion_changed_two, data = opinionChangeNfcTable)
s1 <- update(saturated, .~. -group:opinion_changed_two_levels:nfcR)
# create next simpler model
s2 <- update(s1, .~. -nfcR:opinion_changed_two_levels)
s3 <- update(s1, .~. -group:nfcR) # treatment:nfcR
s4 <- update(s1, .~. -group:opinion_changed_two_levels) # treatment:opinion_changed_two
# examine resultes
print(anova(saturated, s1))
print(anova(s1, s2))
print(anova(s1, s3))
print(anova(s1, s4))
# plot table
mosaicplot(opinionChangeNfcTable, shade = TRUE)
mosaicplot(opinionChangeTable, shade = TRUE)