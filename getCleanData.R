getNfcValue <- function(responses, isInverted, scaleDimension = 7) {
  resp <- getResponseValue(responses, isInverted, scaleDimension)
  
  return(mean(resp))
}

commonOperations <- function(data, sourceData, groupIdentifier, isInvertedNfc, breaksNfc, labelsNfc) {
  cleanData <- mutate(data, opinion_changed = opinion_before - opinion_after) %>%
    mutate(nfc = apply(select(sourceData, X1.nfc:X4.nfc), 1, function(x) getNfcValue(x, isInvertedNfc))) %>%
    mutate(group = groupIdentifier) %>%
    mutate(timeSubmitted = as.numeric(hms(format(as.POSIXct(dateSubmitted, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")))/3600)
  # depend on previous data  
  cleanData <- mutate(cleanData, opinion_changed_two_levels = cut(opinion_changed, breaks = c(-3,0,2))) %>%
    mutate(nfcR = cut(cleanData$nfc, breaks = breaksNfc, labels = labelsNfc))
    
  return(cleanData)
}

getCleanData <- function(sourceData, groupIdentifier, isInvertedNfc, breaksNfc, labelsNfc) {
  cleanData <- select(sourceData, Response.ID, sex, age, opinion_before, opinion_after, personality, O, C, E, A, N, dateSubmitted, timeToFinish, overusedResponse, overusedResponseCount, personalityCount) %>%
    mutate(timeOnQuestionnaire = NA, timeOnText = NA, timeOnNfc = NA)
  cleanData <- commonOperations(cleanData, sourceData, groupIdentifier, isInvertedNfc, breaksNfc, labelsNfc)
}

getCleanDataAdapted <- function(sourceData, groupIdentifier, isInvertedNfc, breaksNfc, labelsNfc) {
  cleanData <- select(sourceData, Response.ID, sex, age, opinion_before, opinion_after, personality, O, C, E, A, N, dateSubmitted, timeToFinish, overusedResponse, overusedResponseCount, personalityCount, timeOnQuestionnaire = Time.spent.on.page..Information.collection, timeOnText, timeOnNfc = Time.spent.on.page..Collect.additional.information)
  cleanData <- commonOperations(cleanData, sourceData, groupIdentifier, isInvertedNfc, breaksNfc, labelsNfc)
}
