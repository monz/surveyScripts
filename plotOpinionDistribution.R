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

print(plot(likert(concatenatedBefore)))
print(plot(likert(concatenatedAfter)))
print(plot(likert(concatenatedChange)))

# stacked bar plot
print(qplot(opinion_changed, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_after, labels = c("pos", "neut", "neg")), col=I("black")))
# gouped bar plot
tableChanged <- as.data.frame(xtabs(~ group + opinion_changed, data = surveyDataCombined))
print(ggplot(tableChanged, aes(factor(opinion_changed), Freq, fill = group)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1"))
# histogram age distribution, groups
print(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 2, fill = group, col=I("black")))
# histogram age distribution, groups and opinion_changed_5
print(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_changed), col=I("black")))
# histogram age distribution, groups and opinion_changed_3
print(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_changed_two_levels), col=I("black")))

# histogram opinion_changed, compared to opinion_before and sex
print(qplot(opinion_changed, data = filter(surveyDataCombined, sex != "NA"), binwidth = 1, facets =  . ~ group + opinion_before, fill = sex))
# histogram opinion_changed, compared to opinion_after and sex
print(qplot(opinion_changed, data = filter(surveyDataCombined, sex != "NA"), binwidth = 1, facets =  . ~ group + opinion_after, fill = sex))

# boxplot time to finish, sex, time < 600
print(qplot(sex, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), facets = ~ group, geom = "boxplot"))
# boxplot time to finish, nfcR time < 600
print(qplot(nfcR, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), facets = ~ group, geom = "boxplot"))
print(qplot(nfcR, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), geom = "boxplot"))
# boxplot time to finish, nfcR + sex, time < 600
print(qplot(sex, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600, sex != "NA"), facets = ~ group + nfcR, geom = "boxplot"))
# boxplot time to finish, nfcR + personality, time < 600
print(qplot(personality, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600, sex != "NA"), facets = ~ group + nfcR, geom = "boxplot"))
# boxplot time to finish, personality, time < 600
print(qplot(personality, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600, sex != "NA"), facets = ~ group, geom = "boxplot"))

# fit straight line, split by sex
print(qplot(x = nfc, y = O, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = C, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = E, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = A, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = N, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
# fit straight line, split by group + sex
print(qplot(x = nfc, y = O, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = C, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = E, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = A, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = N, data = filter(surveyDataCombined, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))

# timeSubmitted vs personality
print(qplot(timeSubmitted, personality, data = surveyDataCombined,  facets = ~ group))
print(qplot(timeSubmitted, data = surveyDataCombined, binwidth = 1, facets = ~ personality + group, fill = sex))
# timeSubmitted vs opinionChanged
print(qplot(x = opinion_changed_two_levels, y = timeSubmitted, data = surveyDataCombined, facets = ~ group, geom = "boxplot"))