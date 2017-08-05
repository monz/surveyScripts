getNfcValue <- function(responses, isInverted, scaleDimension = 7) {
  resp <- getResponseValue(responses, isInverted, scaleDimension)
  
  return(mean(resp))
}

getCleanData <- function(sourceData, groupIdentifier, isInvertedNfc, breaksNfc, labelsNfc) {
  cleanData <- select(sourceData, Response.ID, sex, age, opinion_before, opinion_after, personality, O, C, E, A, N, dateSubmitted, timeToFinish, overusedResponse, overusedResponseCount, personalityCount)
  cleanData <- mutate(cleanData, timeOnQuestionnaire = NA, timeOnText = NA, timeOnNfc = NA)
  cleanData <- mutate(cleanData, opinion_changed = opinion_before - opinion_after)
  cleanData <- mutate(cleanData, nfc = apply(select(sourceData, X1.nfc:X4.nfc), 1, function(x) getNfcValue(x, isInvertedNfc)))
  cleanData <- mutate(cleanData, nfcR = cut(cleanData$nfc, breaks = breaksNfc, labels = labelsNfc))
  cleanData <- mutate(cleanData, group = groupIdentifier)
}

getCleanDataAdapted <- function(sourceData, groupIdentifier, isInvertedNfc, breaksNfc, labelsNfc) {
  cleanData <- select(sourceData, Response.ID, sex, age, opinion_before, opinion_after, personality, O, C, E, A, N, dateSubmitted, timeToFinish, overusedResponse, overusedResponseCount, personalityCount, timeOnQuestionnaire = Time.spent.on.page..Information.collection, timeOnText, timeOnNfc = Time.spent.on.page..Collect.additional.information)
  cleanData <- mutate(cleanData, opinion_changed = opinion_before - opinion_after)
  cleanData <- mutate(cleanData, nfc = apply(select(sourceData, X1.nfc:X4.nfc), 1, function(x) getNfcValue(x, isInvertedNfc)))
  cleanData <- mutate(cleanData, nfcR = cut(cleanData$nfc, breaks = breaksNfc, labels = labelsNfc))
  cleanData <- mutate(cleanData, group = groupIdentifier)
}
