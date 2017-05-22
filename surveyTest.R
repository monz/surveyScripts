# import external libraries
library("dplyr")
library("tidyr")

# import functions
source("rworkspace/surveyTest/basicStatistics.R")
source("rworkspace/surveyTest/personalityTypesStatistics.R")
source("rworkspace/surveyTest/getResponseValue.R")

# read file
surveyData <- read.csv("SurveyExport.csv", na.strings = c("", " "))
#surveyData <- read.csv("SurveyExport.csv")

# calculate new values
dateFormat = "%Y-%m-%d %H:%M:%S"
surveyData <- mutate(surveyData, timeToFinish = (
  strptime(surveyData$Date.Submitted, format = dateFormat) - 
  strptime(surveyData$Time.Started, format = dateFormat)))

# common data
personalityTypes <- c("O","C","E","A","N")
responseCount <- nrow(surveyData)
isComplete <- surveyData$Status == "Complete"
isPartial <- surveyData$Status == "Partial"
completeResponseCount <-length(which(isComplete))
partialResponseCount <-length(which(isPartial))

surveyDataCompleted <- surveyData[isComplete,]
surveyDataPartial <- surveyData[isPartial,]

# questionnaire resulsts
questionnaireResults <- select(surveyDataCompleted, contains("personality_questionnaire"))
questionnaireMeans <- data.frame(sapply(questionnaireResults, mean))
questionnaireStandardDeviations <- data.frame(sapply(questionnaireResults, sd))

# test group
surveyDataCompletedTest <- surveyDataCompleted[!is.na(surveyDataCompleted$personality), ]
surveyDataCompletedTest <- with(surveyDataCompletedTest, select(surveyDataCompletedTest, Response.ID, sex, age, opinion_before, opinion_after_test, personality))
surveyDataCompletedTest <- with(surveyDataCompletedTest, mutate(surveyDataCompletedTest, decisionChanged = (opinion_before != opinion_after_test)))
surveyDataCompletedTest <- with(surveyDataCompletedTest, mutate(surveyDataCompletedTest, decisionPositive = ((opinion_after_test == 1 & opinion_before >= 2) | (opinion_after_test == 2 & opinion_before == 3))))
surveyDataCompletedTest <- with(surveyDataCompletedTest, mutate(surveyDataCompletedTest, decisionNegative = !(!decisionChanged | decisionPositive))) # logical implication (!p || q), here the negated implication is used
surveyDataCompletedTest <- mutate(surveyDataCompletedTest, recPers = getAmbiguousPersonalities(getPersonalityVectors(getResponseValue(surveyDataCompleted[!is.na(surveyDataCompleted$opinion_after_test), ][14:28]))))

testStatistics <- basicStatistics(surveyDataCompleted, "opinion_after_test")
testStatisticsByPersonality <- personalityTypesStatistics(surveyDataCompleted, personalityTypes)

# test sub group, personality trait selection
clearPersonality <- surveyDataCompletedTest[sapply(lapply(surveyDataCompletedTest$recPers, function(x) if(length(x) >= 2) return(x)), is.null), ]
ambiguousPersonality <- surveyDataCompletedTest[!sapply(lapply(surveyDataCompletedTest$recPers, function(x) if(length(x) >= 2) return(x)), is.null), ]

# control group
surveyDataCompletedControl <- surveyDataCompleted[is.na(surveyDataCompleted$personality), ]
surveyDataCompletedControl <- with(surveyDataCompletedControl, select(surveyDataCompletedControl, Response.ID, sex, age, opinion_before, opinion_after_control, personality))
surveyDataCompletedControl <- with(surveyDataCompletedControl, mutate(surveyDataCompletedControl, decisionChanged = (opinion_before != opinion_after_control)))
surveyDataCompletedControl <- with(surveyDataCompletedControl, mutate(surveyDataCompletedControl, decisionPositive = ((opinion_after_control == 1 & opinion_before >= 2) | (opinion_after_control == 2 & opinion_before == 3))))
surveyDataCompletedControl <- with(surveyDataCompletedControl, mutate(surveyDataCompletedControl, decisionNegative = !(!decisionChanged | decisionPositive))) # logical implication (!p || q), here the negated implication is used

controlStatistics <- basicStatistics(surveyDataCompleted, "opinion_after_control")

# combine
combinedStatistics <- rbind(testStatistics, controlStatistics)
combinedStatistics <- mutate(combinedStatistics, group = c("test", "control"))

# decision distribution + plot
decisionDistribution <- select(combinedStatistics, deniedCount.After:agreeCount.Before, group)
colnames(decisionDistribution) <- gsub("Count", "", colnames(decisionDistribution), fixed = TRUE)
decisionDistribution <- gather(decisionDistribution, decision, count, denied.After:agree.Before)
decisionDistribution <- separate(decisionDistribution, decision, c("decision", "state"), sep = "\\.", remove = TRUE)
decisionDistribution <- arrange(decisionDistribution, desc(group), desc(state))
decisionDistribution <- decisionDistribution %>% group_by(group, state) %>% mutate(percentage=100 * (count/sum(count)))
## pie plot
par(mfrow=c(2,2))
# test
with(decisionDistribution, pie(percentage[group == "test" & state == "Before"], paste(decision, sprintf("%.2f", percentage[group == "test" & state == "Before"])), main = "test group before persuasion"))
with(decisionDistribution, pie(percentage[group == "test" & state == "After"], paste(decision, sprintf("%.2f", percentage[group == "test" & state == "After"])), main = "test group after persuasion"))
# control
with(decisionDistribution, pie(percentage[group == "control" & state == "Before"], paste(decision, sprintf("%.2f", percentage[group == "control" & state == "Before"])), main = "control group before persuasion"))
with(decisionDistribution, pie(percentage[group == "control" & state == "After"], paste(decision, sprintf("%.2f", percentage[group == "control" & state == "After"])), main = "control group after persuasion"))
