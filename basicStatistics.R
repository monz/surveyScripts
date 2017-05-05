basicStatistics <- function(groupData, opinionAfter) {
  groupCount <- length(which(!is.na(groupData[[opinionAfter]])))
  deniedCount <- length(which(groupData[[opinionAfter]] == 3))
  neutralCount <- length(which(groupData[[opinionAfter]] == 2))
  agreeCount <- length(which(groupData[[opinionAfter]] == 1))
  decisionRemainedCount <- length(which(groupData[[opinionAfter]] == groupData$opinion_before))
  decisionChangedCount <- length(which(groupData[[opinionAfter]] != groupData$opinion_before))
  decisionChangedPositiveCount <- length(which((groupData[[opinionAfter]] == 1 & groupData$opinion_before >= 2) | (groupData[[opinionAfter]] == 2 & groupData$opinion_before == 3) ))
  decisionChangedNegativeCount <- length(which((groupData[[opinionAfter]] == 3 & groupData$opinion_before <= 2) | (groupData[[opinionAfter]] == 2 & groupData$opinion_before == 1) ))
  decisionChangedPercent <- decisionChangedCount / groupCount
  decisionChangedPositivePercent <- decisionChangedPositiveCount / decisionChangedCount
  decisionChangedNegativePercent <- decisionChangedNegativeCount / decisionChangedCount

  df <- data.frame(groupCount, deniedCount, neutralCount, agreeCount,
                   decisionRemainedCount, decisionChangedCount,
                   decisionChangedPositiveCount, decisionChangedNegativeCount,
                   decisionChangedPercent, decisionChangedPositivePercent,
                   decisionChangedNegativePercent)

  colnames(df) <- c("groupCount", "deniedCount", "neutralCount", "agreeCount",
                   "decisionRemainedCount", "decisionChangedCount",
                   "decisionChangedPositiveCount", "decisionChangedNegativeCount",
                   "decisionChangedPercent", "decisionChangedPositivePercent",
                   "decisionChangedNegativePercent")

  return(df)
}