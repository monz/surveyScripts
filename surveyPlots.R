# import libraries
library(plyr) # incompatible if packet 'dplyr' was loaded before; if problems occur, restart R-Studio and run this R-Script first
library(ggplot2)
library(likert)
library(scales)

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

## set font size
theme_set(theme_gray(base_size = 16))
####
maxValue <- max(filter(surveyDataCombined, group == "control")$opinion_before)
controlBefore <- factor(maxValue - select(filter(surveyDataCombined, group == "control"), opinion_before)[,], labels = c("neg", "neut", "pos"))
maxValue <- max(filter(surveyDataCombined, group == "test")$opinion_before)
testBefore <- factor(maxValue - select(filter(surveyDataCombined, group == "test"), opinion_before)[,], labels = c("neg", "neut", "pos"))

maxValue <- max(filter(surveyDataCombined, group == "control")$opinion_after)
controlAfter <- factor(maxValue - select(filter(surveyDataCombined, group == "control"), opinion_after)[,], labels = c("neg", "neut", "pos"))
maxValue <- max(filter(surveyDataCombined, group == "test")$opinion_after)
testAfter <- factor(maxValue - select(filter(surveyDataCombined, group == "test"), opinion_after)[,], labels = c("neg", "neut", "pos"))

controlChange <- factor(select(filter(surveyDataCombined, group == "control"), opinion_changed)[,], labels = c("neg2", "neg1", "neut", "pos1", "pos2"))
testChange <- factor(select(filter(surveyDataCombined, group == "test"), opinion_changed)[,], labels = c("neg2", "neg1", "neut", "pos1", "pos2"))

length(controlBefore) = length(testBefore)
concatenatedBefore <- bind_cols(as.data.frame(controlBefore),as.data.frame(testBefore))

length(controlAfter) = length(testAfter)
concatenatedAfter <- bind_cols(as.data.frame(controlAfter),as.data.frame(testAfter))

concatenatedCombined <- bind_cols(concatenatedBefore, concatenatedAfter)

length(controlChange) = length(testChange)
concatenatedChange <- bind_cols(as.data.frame(controlChange),as.data.frame(testChange))

p <- append(p, list(plot(likert(concatenatedBefore))))
p <- append(p, list(plot(likert(concatenatedAfter))))
p <- append(p, list(plot(likert(concatenatedCombined))))
p <- append(p, list(plot(likert(concatenatedChange))))

### stacked bar plot opinion before
b <- xtabs(~ group + factor(opinion_before, levels = c(3,2,1), labels = c("opponent", "undecided", "supporter")), data = surveyDataCombined)
d <- as.data.frame(prop.table(b, 1))
colnames(d) <- c("group", "opinion", "percentage")
d <- mutate(d, percentage2 = round(percentage*100,0))
d[6,4] <- 32
d <- ddply(d, .(group), transform, pos = 100 - (cumsum(percentage2) - (0.5 * percentage2)))
stacked <- ggplot() + geom_bar(aes(y = percentage2, x = group, fill = opinion), data  = d, stat = "identity")
stacked <- stacked + geom_text(data=d, aes(x = group, y = pos, label = paste0(percentage2,"%")), size=4)
stacked <- stacked + labs(x="Group", y="Percentage") +  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""))
stacked <- stacked + scale_fill_discrete(name="Opinion Before")
p <- append(p, list(stacked))

### stacked bar plot opinion change
b <- xtabs(~ group + factor(opinion_changed), data = surveyDataCombined)
d <- as.data.frame(prop.table(b, 1))
colnames(d) <- c("group", "opinion", "percentage")
d <- mutate(d, percentage2 = round(percentage*100,0))
d[2,4] <- 4
d <- ddply(d, .(group), transform, pos = 100 - (cumsum(percentage2) - (0.5 * percentage2)))
stacked <- ggplot() + geom_bar(aes(y = percentage2, x = group, fill = opinion), data = d, stat = "identity")
stacked <- stacked + geom_text(data=d, aes(x = group, y = pos, label = paste0(percentage2,"%")), size=4)
stacked <- stacked + labs(x="Group", y="Percentage") +  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""))
stacked <- stacked + scale_fill_discrete(name="Opinion Change")
p <- append(p, list(stacked))

# stacked bar plot
p <- append(p, list(qplot(opinion_changed, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_after, labels = c("pos", "neut", "neg")), col=I("black"))))
# gouped bar plot
tableChanged <- as.data.frame(xtabs(~ group + opinion_changed, data = surveyDataCombined))
p <- append(p, list(ggplot(tableChanged, aes(factor(opinion_changed), Freq, fill = group)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(name = "Group", palette = "Set1") +
  labs(x = "Opinion Change", y = "Count")))
# gouped bar plot clean data opinon change
tableChanged <- as.data.frame(xtabs(~ group + opinion_changed, data = surveyDataCombinedClean))
p <- append(p, list(ggplot(tableChanged, aes(factor(opinion_changed), Freq, fill = group)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(name = "Group", palette = "Set1") +
    labs(x = "Opinion Change", y = "Count")))
# gouped bar plot clean data opinion change two level
tableChanged <- as.data.frame(xtabs(~ group + opinion_changed_two_levels, data = surveyDataCombinedClean))
p <- append(p, list(ggplot(tableChanged, aes(factor(opinion_changed_two_levels), Freq, fill = group)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(name = "Group", palette = "Set1") +
  labs(x = "Opinion Change", y = "Count")))
# histogram age distribution, groups
p <- append(p, list(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 2, fill = group, col=I("black"))))
# histogram age by group
p <- append(p, list(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 2, col=I("black"))))
# histogram age
p <- append(p, list(qplot(age, data = surveyDataCombined, binwidth = 2, col=I("black"))))
# histogram age - fill sex
p <- append(p, list(qplot(age, data = surveyDataCombined, binwidth = 2, fill = sex, col=I("black"), xlab = "Age", ylab = "Count")))
# histogram age distribution, groups and opinion_changed_5
p <- append(p, list(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_changed), col=I("black"))))
# histogram age distribution, groups and opinion_changed_3
p <- append(p, list(qplot(age, data = surveyDataCombined, facets = . ~ group, binwidth = 1, fill = factor(opinion_changed_two_levels), col=I("black"))))
# histogram personality by groups
p <- append(p, list(qplot(factor(personality, levels = c("O", "C", "E", "A", "N")), data = surveyDataCombined, facets = . ~ group, fill = sex, col=I("black"), xlab = "Personality Trait", ylab = "Count")))
# histogram personality
p <- append(p, list(qplot(factor(personality, levels = c("O", "C", "E", "A", "N")), data = surveyDataCombined, fill = factor(sex), col=I("black"))))
# histogram personality by groups - cleaned data
p <- append(p, list(qplot(factor(personality, levels = c("O", "C", "E", "A", "N")), data = surveyDataCombinedClean, facets = . ~ group, fill = factor(sex), col=I("black"))))

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
p <- append(p, list(qplot(sex, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), facets = ~ group, geom = "boxplot") + 
  geom_hline(yintercept = 120, color = I("red")) + 
  annotate("text", 1, 135, label = "cutoff = 120 sec", color = I("red"))))
# boxplot time to finish, group time < 600 - cutoff
p <- append(p, list(qplot(group, timeToFinish, data = filter(surveyDataCombined, timeToFinish < 600), xlab = "Group", ylab = "Time To Finish in Seconds", geom = "boxplot") + 
  geom_hline(yintercept = 120, color = I("red")) + 
  annotate("text", .6, 135, label = "cutoff = 120 sec", color = I("red"))))

# fit straight line
personalityNfc <- gather(select(surveyDataCombined, O,C,E,A,N,nfc), "personality", "personality_trait_value", O,C,E,A,N)
p <- append(p, list(qplot(nfc, personality_trait_value, data = personalityNfc, facets = ~ factor(personality, levels = c("O", "C", "E", "A", "N"))) + geom_smooth(method = "lm", formula = y~x)))
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

# relationship between personality value and opinion change
qplot(factor(opinion_changed), O, data = surveyDataCombined, geom = "boxplot")
qplot(factor(opinion_changed), C, data = surveyDataCombined, geom = "boxplot")
qplot(factor(opinion_changed), E, data = surveyDataCombined, geom = "boxplot")
qplot(factor(opinion_changed), A, data = surveyDataCombined, geom = "boxplot")
qplot(factor(opinion_changed), N, data = surveyDataCombined, geom = "boxplot")
qplot(opinion_changed, O, data = surveyDataCombined) + geom_smooth(method = "lm")
qplot(opinion_changed, C, data = surveyDataCombined) + geom_smooth(method = "lm")
qplot(opinion_changed, E, data = surveyDataCombined) + geom_smooth(method = "lm")
qplot(opinion_changed, A, data = surveyDataCombined) + geom_smooth(method = "lm")
qplot(opinion_changed, N, data = surveyDataCombined) + geom_smooth(method = "lm")

# print or export all plots
lapply(p, function(x) printOrExport(x, x$labels$title, export = FALSE))
