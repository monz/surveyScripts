# import functions
source("rworkspace/surveyTest/loadData.R")

### age, mean, sd, median by inquiry
sep <- "##### \n"
cat(sep, "age, mean, sd, median by inquiry\n")
print(lapply(combinedSurveyDataList, function(x) summarise(x, mean(age), sd(age), median(age))))
### age, mean, sd, median, min and max
cat(sep, "age, mean, sd, median, min, max combined\n")
print(summarise(surveyDataCombined, mean(age), sd(age), median(age), min(age), max(age)))
### age, mean, sd by group
cat(sep, "age, mean, sd, median by group\n")
print(group_by(surveyDataCombined, group) %>% summarise(mean(age), sd(age), median(age)))


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

### time to finish
cat(sep, "time to finish survey quartiles\n")
print(summary(surveyDataCombined$timeToFinish))
### time to finish, lower bound
cat(sep, "time to finish lower bound\n")
print(sum(summarise(surveyDataCombined, welcomePage = quantile(timeToFinish - (timeOnQuestionnaire + timeOnText + timeOnNfc),.1, na.rm = TRUE), questionnaire = quantile(timeOnQuestionnaire, .1, na.rm = TRUE), message = quantile(timeOnText,.1, na.rm = TRUE), nfc = quantile(timeOnNfc, .1, na.rm = TRUE))))

### unique subjects of study
cat(sep, "unique subjects of study\n")
print(unique(unlist(
  lapply(combinedCsvDataList, function(x) {
  subjects <- select(filter(x, Status == "Complete"), subject_of_study)
  unique(sapply(subjects, tolower))
}))))