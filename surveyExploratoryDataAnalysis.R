# import external libraries
library(dplyr)
library(gmodels)
library(pastecs)
library(car)
library(lubridate)

# import functions
source("rworkspace/surveyTest/loadData.R")

## hypothesis timeSubmitted to opinion_changed(-3,0,2) within test group
rFromWilcox <- function(model, N) {
  z <- qnorm(model$p.value/2)
  r <- z / sqrt(N)
  cat(model$data.name, "Effect Size, r = ", r, "\n")
}
testGroupDataCombined <- filter(surveyDataCombined, group == "test")
# test for normality
print(stat.desc(testGroupDataCombined, basic = FALSE, norm = TRUE))
# test for similar variances
print(leveneTest(timeSubmitted ~ opinion_changed_two_levels, data = testGroupDataCombined))
# wilcoxon
wm <- wilcox.test(timeSubmitted ~ opinion_changed_two_levels, data = testGroupDataCombined)
print(wm)
rFromWilcox(wm, nrow(testGroupDataCombined))
# same test, with direction
wm <- wilcox.test(timeSubmitted ~ opinion_changed_two_levels, data = testGroupDataCombined, conf.int = TRUE, alternative = "greater")
print(wm)
rFromWilcox(wm, nrow(testGroupDataCombined))
# clean up
rm(testGroupDataCombined)