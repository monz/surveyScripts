# import functions
source("rworkspace/surveyTest/loadData.R")

### age, mean and sd
print(lapply(combinedSurveyDataList, function(x) summarise(x, mean(age), sd(age))))

### personality
print(lapply(combinedSurveyDataList, function(x) xtabs(~ personality, data = x)))