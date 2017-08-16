# import external libraries
library(dplyr)
library(pastecs)
library(car)

# import functions
source("rworkspace/surveyTest/loadData.R")

sep <- "##### \n"

## hypothesis timeSubmitted to opinion_changed(-3,0,2) within test group
# helper function to calculate effect size
rFromWilcox <- function(model, N) {
  z <- qnorm(model$p.value/2)
  r <- z / sqrt(N)
  cat(model$data.name, "Effect Size, r = ", r, "\n")
}
# extract data
cat(sep, "wilcoxon-mann-whitney test --- timeSubmitted ~ opinion changed two levels\n")
testGroupDataCombinedClean <- filter(surveyDataCombinedClean, group == "test") %>% select(Response.ID, timeSubmitted, dateSubmitted, group, personality, opinion_changed_two_levels)
# test for normality
print(stat.desc(testGroupDataCombinedClean, basic = FALSE, norm = TRUE))
# test for similar variances - must be non-significant
print(leveneTest(timeSubmitted ~ opinion_changed_two_levels, data = testGroupDataCombinedClean))
# wilcoxon
wm <- wilcox.test(timeSubmitted ~ opinion_changed_two_levels, data = testGroupDataCombinedClean)
print(wm)
rFromWilcox(wm, nrow(testGroupDataCombinedClean))
# same test, with direction
wm <- wilcox.test(timeSubmitted ~ opinion_changed_two_levels, data = testGroupDataCombinedClean, conf.int = TRUE, alternative = "greater")
print(wm)
rFromWilcox(wm, nrow(testGroupDataCombinedClean))
# clean up
rm(testGroupDataCombinedClean)

### Wilcoxon-Mann-Whitney-U test, opinion_changed_two_levels ~ group
cat(sep, "wilcoxon-mann-withney test --- opinion changed two levels ~ group\n")
# test for normality
print(stat.desc(as.numeric(surveyDataCombinedClean$opinion_changed_two_levels), basic = FALSE, norm = TRUE))
# test for similar variances - must be non-significant, BUT IT IS p < .5!!!
print(leveneTest(as.numeric(opinion_changed_two_levels) ~ factor(group), data = surveyDataCombinedClean))
# test
wm <- wilcox.test(as.numeric(opinion_changed_two_levels) ~ as.numeric(factor(group, levels = c("test", "control"))), data = surveyDataCombinedClean, conf.int = TRUE, alternative = "greater")
print(wm)
rFromWilcox(wm, nrow(surveyDataCombinedClean))