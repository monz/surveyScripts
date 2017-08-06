controlBefore <- factor(select(filter(surveyDataClean, group == "control"), opinion_before)[,], labels = c("pos", "neut", "neg"))
testBefore <- factor(select(filter(surveyDataClean, group == "test"), opinion_before)[,], labels = c("pos", "neut", "neg"))

controlAfter <- factor(select(filter(surveyDataClean, group == "control"), opinion_after)[,], labels = c("pos", "neut", "neg"))
testAfter <- factor(select(filter(surveyDataClean, group == "test"), opinion_after)[,], labels = c("pos", "neut", "neg"))

controlChange <- factor(select(filter(surveyDataClean, group == "control"), opinion_changed)[,], labels = c("neg2", "neg1", "neut", "pos1", "pos2"))
testChange <- factor(select(filter(surveyDataClean, group == "test"), opinion_changed)[,], labels = c("neg2", "neg1", "neut", "pos1", "pos2"))

length(controlBefore) = length(testBefore)
concatenatedBefore <- bind_cols(as.data.frame(controlBefore),as.data.frame(testBefore))

length(controlAfter) = length(testAfter)
concatenatedAfter <- bind_cols(as.data.frame(controlAfter),as.data.frame(testAfter))

length(controlChange) = length(testChange)
concatenatedChange <- bind_cols(as.data.frame(controlChange),as.data.frame(testChange))

print(plot(likert(concatenatedBefore)))
print(plot(likert(concatenatedAfter)))
print(plot(likert(concatenatedChange)))

#print(qplot(opinion_changed, data = surveyDataClean, facets = . ~ group, binwidth = 1, main = "Opinion Change by Group"))
#print(qplot(opinion_changed, data = surveyDataClean, facets = group ~ nfcR, binwidth = 1, main = "Opinion Change by NFC"))
#print(qplot(opinion_changed, data = surveyDataClean, facets = group ~ personality, binwidth = 1, main = "Opinion Change by Personality"))
#print(qplot(opinion_changed, data = surveyDataClean, facets = group ~ sex, binwidth = 1, main = "Opinion Change by Sex"))
#print(qplot(opinion_changed, data = surveyDataClean, facets = group ~ nfcR + personality, binwidth = 1, main = "Opinion Change by NFC and Personality"))
#print(qplot(opinion_changed, data = subset(surveyDataClean, sex != "NA"), facets = group + sex ~ nfcR + personality, binwidth = 1, main = "Opinion Change by NFC, Personality and Sex"))

# stacked bar plot
print(qplot(opinion_changed, data = surveyDataClean, facets = . ~ group, binwidth = 1, fill = factor(opinion_after, labels = c("pos", "neut", "neg")), col=I("black")))
# gouped bar plot
tableChanged <- as.data.frame(xtabs(~ group + opinion_changed, data = surveyDataClean))
print(ggplot(tableChanged, aes(factor(opinion_changed), Freq, fill = group)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1"))
# histogram age distribution, groups
print(qplot(age, data = surveyDataClean, facets = . ~ group, binwidth = 2, fill = group, col=I("black")))
# histogram age distribution, groups and opinion_changed_5
print(qplot(age, data = surveyDataClean, facets = . ~ group, binwidth = 1, fill = factor(opinion_changed), col=I("black")))
# histogram age distribution, groups and opinion_changed_3
print(qplot(age, data = surveyDataClean, facets = . ~ group, binwidth = 1, fill = factor(cut(opinion_changed, breaks = c(-3,-1,0,2))), col=I("black")))

# histogram opinion_changed, compared to opinion_before and sex
print(qplot(opinion_changed, data = filter(surveyDataCombinedClean, sex != "NA"), binwidth = 1, facets =  . ~ group + opinion_before, fill = sex))
# histogram opinion_changed, compared to opinion_after and sex
print(qplot(opinion_changed, data = filter(surveyDataCombinedClean, sex != "NA"), binwidth = 1, facets =  . ~ group + opinion_after, fill = sex))

# boxplot time to finish, sex, time < 600
print(qplot(sex, timeToFinish, data = filter(surveyDataCombinedClean, timeToFinish < 600), facets = ~ group, geom = "boxplot"))
# boxplot time to finish, nfcR time < 600
print(qplot(nfcR, timeToFinish, data = filter(surveyDataCombinedClean, timeToFinish < 600), facets = ~ group, geom = "boxplot"))
print(qplot(nfcR, timeToFinish, data = filter(surveyDataCombinedClean, timeToFinish < 600), geom = "boxplot"))

# fit straight line, split by sex
print(qplot(x = nfc, y = O, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = C, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = E, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = A, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = N, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ sex) + geom_smooth(method='lm',formula=y~x))
# fit straight line, split by group + sex
print(qplot(x = nfc, y = O, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = C, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = E, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = A, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))
print(qplot(x = nfc, y = N, data = filter(surveyDataCombinedClean, sex != "NA"), geom = c("point"), facets = ~ group + sex) + geom_smooth(method='lm',formula=y~x))

# time vs personality
print(qplot(as.numeric(hms(format(as.POSIXct(dateSubmitted, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")))/3600, personality, data = surveyDataClean,  facets = ~ group))
print(qplot(as.numeric(hms(format(as.POSIXct(dateSubmitted, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")))/3600, data = surveyDataClean, binwidth = 1, facets = ~ personality + group, fill = sex))