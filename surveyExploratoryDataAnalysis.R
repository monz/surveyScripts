# import external libraries
library(dplyr)
library(gmodels)
library(likert)
library(pastecs)
library(car)
library(lubridate)

# import functions
source("rworkspace/surveyTest/loadData.R")

# combine data
surveyDataClean <- combinedSurveyDataList[[1]]
surveyDataAdaptedClean <- rbind(combinedSurveyDataList[[2]], combinedSurveyDataList[[3]], combinedSurveyDataList[[4]])
surveyDataCombinedClean <- bind_rows(surveyDataClean, surveyDataAdaptedClean)

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