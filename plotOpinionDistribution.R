# import libraries
library(ggplot2)
library(likert)

# import functions
source("rworkspace/surveyTest/loadData.R")

printOrExport <- function(plotVar, filename = "RPlot%03d", export = FALSE) {
  filename <- ifelse(is.null(filename), sprintf("RPlot%06d", sample.int(999999, size = 1)), filename)
  if(export) {
    pdf(paste(filename, ".pdf", sep = ""))
    print(plotVar)
    dev.off()
  } else {
    print(plotVar)
  }
}
# create list of all plots
p <- list()
####

controlBefore <- factor(select(filter(surveyDataCombined, group == "control"), opinion_before)[,], labels = c("pos", "neut", "neg"))
testBefore <- factor(select(filter(surveyDataCombined, group == "test"), opinion_before)[,], labels = c("pos", "neut", "neg"))

controlAfter <- factor(select(filter(surveyDataCombined, group == "control"), opinion_after)[,], labels = c("pos", "neut", "neg"))
testAfter <- factor(select(filter(surveyDataCombined, group == "test"), opinion_after)[,], labels = c("pos", "neut", "neg"))

controlChange <- factor(select(filter(surveyDataCombined, group == "control"), opinion_changed)[,], labels = c("neg2", "neg1", "neut", "pos1", "pos2"))
testChange <- factor(select(filter(surveyDataCombined, group == "test"), opinion_changed)[,], labels = c("neg2", "neg1", "neut", "pos1", "pos2"))

length(controlBefore) = length(testBefore)
concatenatedBefore <- bind_cols(as.data.frame(controlBefore),as.data.frame(testBefore))

length(controlAfter) = length(testAfter)
concatenatedAfter <- bind_cols(as.data.frame(controlAfter),as.data.frame(testAfter))

length(controlChange) = length(testChange)
concatenatedChange <- bind_cols(as.data.frame(controlChange),as.data.frame(testChange))

p <- append(p, list(plot(likert(concatenatedBefore))))
p <- append(p, list(plot(likert(concatenatedAfter))))
p <- append(p, list(plot(likert(concatenatedChange))))

# stacked bar plot
p <- append(p, list(qplot(opinion_changed, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_after, labels = c("pos", "neut", "neg")), col=I("black"))))
# gouped bar plot
tableChanged <- as.data.frame(xtabs(~ group + opinion_changed, data = surveyDataCombined))
p <- append(p, list(ggplot(tableChanged, aes(factor(opinion_changed), Freq, fill = group)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1")))
# histogram age distribution, groups
p <- append(p, list(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 2, fill = group, col=I("black"))))
# histogram age by group
p <- append(p, list(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 2, col=I("black"))))
# histogram age distribution, groups and opinion_changed_5
p <- append(p, list(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_changed), col=I("black"))))
# histogram age distribution, groups and opinion_changed_3
p <- append(p, list(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_changed_two_levels), col=I("black"))))
# histogram personality by groups
p <- append(p, list(qplot(factor(personality, levels = c("O", "C", "E", "A", "N")), data = surveyDataCombined, facets = . ~ group, fill = factor(sex), col=I("black"))))

# histogram opinion_changed, compared to opinion_before and sex
p <- append(p, list(qplot(opinion_changed, data = filter(surveyDataCombined, sex != "NA"), binwidth = 1, facets =  . ~ group + opinion_before, fill = sex)))
# histogram opinion_changed, compared to opinion_after and sex
p <- append(p, list(qplot(opinion_changed, data = filter(surveyDataCombined, sex != "NA"), binwidth = 1, facets =  . ~ group + opinion_after, fill = sex)))
# histogram opinion_changed by personality and group
p <- append(p, list(qplot(opinion_changed, data = filter(surveyDataCombined, sex != "NA"), binwidth = 1, facets =  group ~ factor(personality, levels = c("O", "C", "E", "A", "N")), fill = sex, color = I("black"))))

# boxplot time to finish, sex, time < 600
p <- append(p, list(qplot(sex, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), facets = ~ group, geom = "boxplot")))
# boxplot time to finish, nfcR time < 600
p <- append(p, list(qplot(nfcR, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), facets = ~ group, geom = "boxplot")))
p <- append(p, list(qplot(nfcR, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), geom = "boxplot")))
# boxplot time to finish, nfcR + sex, time < 600
p <- append(p, list(qplot(sex, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600, sex != "NA"), facets = ~ group + nfcR, geom = "boxplot")))
# boxplot time to finish, nfcR + personality, time < 600
p <- append(p, list(qplot(personality, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600, sex != "NA"), facets = ~ group + nfcR, geom = "boxplot")))
# boxplot time to finish, personality, time < 600
p <- append(p, list(qplot(personality, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600, sex != "NA"), facets = ~ group, geom = "boxplot")))
# boxplot time to finish, group + sex time < 600
p <- append(p, qplot(y = timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), facets = ~ group, geom = "boxplot") + 
  geom_hline(yintercept = 120, color = I("red")) + 
  annotate("text", 1, 135, label = "cutoff = 120 sec", color = I("red")))
# boxplot time to finish, group time < 600
p <- append(p, qplot(group, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), geom = "boxplot") + 
  geom_hline(yintercept = 120, color = I("red")) + 
  annotate("text", .6, 135, label = "cutoff = 120 sec", color = I("red")))

# fit straight line, split by sex
p <- append(p, list(qplot(x = nfc, y = O, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x)))
p <- append(p, list(qplot(x = nfc, y = C, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x)))
p <- append(p, list(qplot(x = nfc, y = E, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x)))
p <- append(p, list(qplot(x = nfc, y = A, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x)))
p <- append(p, list(qplot(x = nfc, y = N, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x)))
# fit straight line, split by group + sex
p <- append(p, list(qplot(x = nfc, y = O, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x)))
p <- append(p, list(qplot(x = nfc, y = C, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x)))
p <- append(p, list(qplot(x = nfc, y = E, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x)))
p <- append(p, list(qplot(x = nfc, y = A, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x)))
p <- append(p, list(qplot(x = nfc, y = N, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x)))

# timeSubmitted vs personality
p <- append(p, list(qplot(timeSubmitted, personality, data = surveyDataCombined,  facets = ~ group)))
p <- append(p, list(qplot(timeSubmitted, data = surveyDataCombined, binwidth = 1, facets = ~ personality + group, fill = sex)))
# timeSubmitted vs opinionChanged
p <- append(p, list(qplot(x = opinion_changed_two_levels, y = timeSubmitted, data = surveyDataCombined, facets = ~ group, geom = "boxplot")))

# print or export all plots
lapply(p, function(x) printOrExport(x, x$labels$title, export = FALSE))
