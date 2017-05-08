basicStatistics <- function(groupData, opinionAfter) {
  groupCount <- length(which(!is.na(groupData[[opinionAfter]])))
  
  deniedCountBefore <- length(which(groupData$opinion_before == 3 & !is.na(groupData[[opinionAfter]])))
  neutralCountBefore <- length(which(groupData$opinion_before == 2 & !is.na(groupData[[opinionAfter]])))
  agreeCountBefore <- length(which(groupData$opinion_before == 1 & !is.na(groupData[[opinionAfter]])))
  
  deniedCountAfter <- length(which(groupData[[opinionAfter]] == 3))
  neutralCountAfter <- length(which(groupData[[opinionAfter]] == 2))
  agreeCountAfter <- length(which(groupData[[opinionAfter]] == 1))
  
  decisionRemainedCount <- length(which(groupData[[opinionAfter]] == groupData$opinion_before))
  decisionChangedCount <- length(which(groupData[[opinionAfter]] != groupData$opinion_before))
  decisionChangedPositiveCount <- length(which((groupData[[opinionAfter]] == 1 & groupData$opinion_before >= 2) | (groupData[[opinionAfter]] == 2 & groupData$opinion_before == 3) ))
  decisionChangedNegativeCount <- length(which((groupData[[opinionAfter]] == 3 & groupData$opinion_before <= 2) | (groupData[[opinionAfter]] == 2 & groupData$opinion_before == 1) ))
  
  decisionChangedPercent <- decisionChangedCount / groupCount
  decisionChangedPositivePercent <- decisionChangedPositiveCount / decisionChangedCount
  decisionChangedNegativePercent <- decisionChangedNegativeCount / decisionChangedCount

  df <- data.frame(groupCount, deniedCountAfter, neutralCountAfter, agreeCountAfter,
                   deniedCountBefore, neutralCountBefore, agreeCountBefore,
                   decisionRemainedCount, decisionChangedCount,
                   decisionChangedPositiveCount, decisionChangedNegativeCount,
                   decisionChangedPercent, decisionChangedPositivePercent,
                   decisionChangedNegativePercent)

  colnames(df) <- c("groupCount", "deniedCount.After", "neutralCount.After", "agreeCount.After",
                    "deniedCount.Before", "neutralCount.Before", "agreeCount.Before",
                   "decisionRemainedCount", "decisionChangedCount",
                   "decisionChangedPositiveCount", "decisionChangedNegativeCount",
                   "decisionChangedPercent", "decisionChangedPositivePercent",
                   "decisionChangedNegativePercent")

  return(df)
}