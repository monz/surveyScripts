# import functions
source("rworkspace/surveyTest/loadData.R")

### age, mean and sd by inquiry
sep <- "##### \n"
cat(sep, "age, mean and sd by inquiry\n")
print(lapply(combinedSurveyDataList, function(x) summarise(x, mean(age), sd(age))))
### age, mean, sd, min and max
cat(sep, "age, mean, sd, min, max combined\n")
print(summarise(surveyDataCombined, mean(age), sd(age), min(age), max(age)))
### age, mean, sd by group
cat(sep, "age, mean, sd by group\n")
print(group_by(surveyDataCombined, group) %>% summarise(mean(age), sd(age)))


### personality by inquiry
cat(sep, "personality by inquiry\n")
print(lapply(combinedSurveyDataList, function(x) xtabs(~ personality, data = x)))
### personality by group
cat(sep, "personality by group\n")
print(xtabs(~ group + personality, data = surveyDataCombined))


### sex by inquiry
cat(sep, "sex xtabs by inquiry\n")
print(lapply(combinedSurveyDataList, function(x) xtabs(~ sex, data = x)))
### sex group
cat(sep, "sex by group\n")
print(xtabs(~ group + sex, data = surveyDataCombined))


### participant count completed and partial by inquiry
cat(sep, "participant count completed and partial by inquiry\n")
print(lapply(combinedCsvDataList, function(x) {a <- xtabs(~ Status, data = x); b <- nrow(x); return(list(a,b))}))
### participant count by group
cat(sep, "participant count by group\n")
print(xtabs(~ group, data = surveyDataCombined))


### nfc, mean, sd, min and max by group
cat(sep, "nfc, mean, sd by group\n")
print(group_by(surveyDataCombined, group) %>% summarise(mean(nfc), sd(nfc)))
