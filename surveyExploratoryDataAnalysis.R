# import external libraries
library(dplyr)
library(pastecs)
library(car)

# import functions
source("rworkspace/surveyTest/loadData.R")

## hypothesis timeSubmitted to opinion_changed(-3,0,2) within test group
# helper function to calculate effect size
rFromWilcox <- function(model, N) {
  z <- qnorm(model$p.value/2)
  r <- z / sqrt(N)
  cat(model$data.name, "Effect Size, r = ", r, "\n")
}
testGroupDataCombinedClean <- filter(surveyDataCombinedClean, group == "test") %>% select(Response.ID, timeSubmitted, dateSubmitted, group, personality, opinion_changed_two_levels)
# test for normality
print(stat.desc(testGroupDataCombinedClean, basic = FALSE, norm = TRUE))
# test for similar variances
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