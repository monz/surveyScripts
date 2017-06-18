getNfcValue <- function(responses, isInverted, scaleDimension = 7) {
  resp <- getResponseValue(responses, isInverted, scaleDimension)
  
  return(mean(resp))
}

getCleanData <- function(sourceData) {
  cleanData <- select(sourceData, Response.ID, sex, age, opinion_before, opinion_after, personality, timeToFinish, overusedResponse, overusedResponseCount, personalityCount)
  cleanData <- mutate(cleanData, opinion_changed = opinion_before - opinion_after)
  cleanData <- mutate(cleanData, nfc = apply(select(sourceData, X1.nfc:X4.nfc), 1, function(x) getNfcValue(x, isInvertedNfc)))
  cleanData <- mutate(cleanData, nfcR = cut(cleanData$nfc, breaks = c(0, 3.84, 5.68, 6.75), labels = c("niedrig", "mittel", "hoch")))
}