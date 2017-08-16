# import libraries
library(boot)

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
### personality count by inquiry
cat(sep, "personality count by inquiry\n")
print(lapply(combinedSurveyDataList, function(x) xtabs(~ personalityCount, data = x)))
### personality count - test group by inquiry
cat(sep, "personality count - test group by inquiry\n")
print(lapply(combinedSurveyDataList, function (x) xtabs(~ personalityCount, data = filter(x, group == "test"))))
### personality count - test group
cat(sep, "personality count - test group\n")
print(xtabs(~ personalityCount, data = filter(surveyDataCombined, group == "test")))

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
# print(unique(unlist(
#   lapply(combinedCsvDataList, function(x) {
#   subjects <- select(filter(x, Status == "Complete"), subject_of_study)
#   unique(sapply(subjects, tolower))
# }))))
print(sort(unique(surveyDataCombined$subject_of_study)))
### top ten subjects of study (participant count)
cat(sep, "top ten subjects of study (participant count)\n")
print(as.data.frame(head(sort(table(surveyDataCombined$subject_of_study), decreasing = TRUE), 10)))
### top ten subjects of study (participant count) cleaned data
cat(sep, "top ten subjects of study (participant count) cleaned data\n")
print(as.data.frame(head(sort(table(surveyDataCombinedClean$subject_of_study), decreasing = TRUE), 10)))

### correlation of personality trait and nfc; Field, p.225
cat(sep, "correlation of personality trait and nfc, Kendall\n")
cor.test(surveyDataCombined$O, surveyDataCombined$nfc, method = "kendall")
cor.test(surveyDataCombined$C, surveyDataCombined$nfc, method = "kendall")
cor.test(surveyDataCombined$E, surveyDataCombined$nfc, method = "kendall")
cor.test(surveyDataCombined$A, surveyDataCombined$nfc, method = "kendall")
cor.test(surveyDataCombined$N, surveyDataCombined$nfc, method = "kendall")

### correlation bootstrapped, personality trait and nfc
cat(sep, "Bootstrapped correlation of personality trait and nfc, Kendall\n")
personalityNfc <- gather(select(surveyDataCombined, O,C,E,A,N,nfc), "personality", "personality_trait_value", O,C,E,A,N)
ff <- function(data, i) {
  cor(data$personality_trait_value[i], data$nfc[i], use = "complete.obs", method = "kendall")
}
sapply(c("O","C","E","A","N"), function(x) {
  boot_kendall <- boot(filter(personalityNfc, personality == x), ff, 2000)
  print(boot_kendall)
  print(boot.ci(boot_kendall))
})

### search for extreme checker below 8 threshold, nothing found
cat(sep, "responses which might be extreme checkers below 8 throshold\n")
d <- getSurveyData(surveyDataCsvTHI, selectionMatrixBigFive, isInvertedNfc, isInvertedBigFive, attributesBigFive, breaksNfc, labelsNfc, overuseThreshold = 6)
e <- bind_rows(d$testGroupDataClean, d$controlGroupDataClean)
print(filter(e, overusedResponse == 7 | overusedResponse == 1, timeToFinish >= 120, overusedResponseCount < 8))

### overused responses
cat(sep, "participants overused response count\n")
print(sum(surveyDataCombinedClean$overusedResponseCount >= 8, na.rm = TRUE))
cat(sep, "extreme checkers count\n")
print(with(surveyDataCombinedClean, sum(overusedResponseCount >= 8 & (overusedResponse == 7 | overusedResponse == 1), na.rm = TRUE)))
