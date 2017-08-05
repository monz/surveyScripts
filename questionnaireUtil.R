getResponseValue <- function(responses, isInverted, scaleDimension = 7) {
  offset <- scaleDimension + 1
  resp <- mapply(function(x, y) {
    value <- x
    if (y) {
      value <- offset - x
    }
    return(value)
  }, responses, isInverted)
  
  return(resp)
}

getEvaluatedValue <- function(data, attributes, selectionMatrix, func, ...) {
  values <- apply(selectionMatrix, 1, function(x) func(data[x], ...))
  
  return(data.frame(attributes, values))
}

getAttribute <- function(evaluatedValues, func) {
  unlist(lapply(evaluatedValues, function(x) {x[[1]][which(x[[2]] == func(x[[2]]))][1]}))
}

getCountOfMaxPersonalityValues <- function(evaluatedValues) {
  unlist(lapply(evaluatedValues, function(x) length(which(x[[2]] == max(x[[2]])))))
}

sameResponseOveruse <- function(responses, threshold) {
  # return if there is no data
  if(sum(is.na(responses)) == length(responses)) {
    return(NA)
  }
  
  responseFrequencies <- as.data.frame(table(responses))
  
  maxResponse <- max(responseFrequencies$Freq, na.rm = TRUE)
  
  ifelse(maxResponse >= threshold,
         overusedResponse <- responseFrequencies[which(responseFrequencies$Freq == maxResponse),], 
         overusedResponse <- NA)
  
  return(overusedResponse)
}

responseOveruse <- function(responses, response, threshold) {
  # return if there is no data
  if(sum(is.na(responses)) == length(responses)) {
    return(NA)
  } else if (!(response %in% responses)) {
    # searched response is not in data
    return(NA)
  }
  
  responseFrequencies <- as.data.frame(table(responses))
  
  ifelse(responseFrequencies$Freq[responseFrequencies$responses == response] >= threshold,
         overusedResponse <- responseFrequencies[responseFrequencies$responses == response,],
         overusedResponse <- NA)
  
  return(overusedResponse)
}

getPersonalityValues <- function(questionnaireResults, isInvertedBigFive, attributesBigFive, selectionMatrixBigFive) {
  questionnaireResponses <- getResponseValue(questionnaireResults, isInvertedBigFive)
  personalityValues <- apply(questionnaireResponses, 1, function(x) getEvaluatedValue(x, attributesBigFive, selectionMatrixBigFive, mean, na.rm = TRUE))
  personalityValuesDataFrame <- data.frame(matrix(unlist(lapply(personalityValues, function(x) spread(x, attributes, values))), ncol = 5, byrow = TRUE))
  names(personalityValuesDataFrame) <- c("A","C","E","N","O")

  return(personalityValuesDataFrame)
}

getPersonalities <- function(questionnaireResults, isInvertedBigFive, attributesBigFive, selectionMatrixBigFive) {
  questionnaireResponses <- getResponseValue(questionnaireResults, isInvertedBigFive)
  personalityValues <- apply(questionnaireResponses, 1, function(x) getEvaluatedValue(x, attributesBigFive, selectionMatrixBigFive, mean, na.rm = TRUE))
  personalities <- getAttribute(personalityValues, max)
  
  return(personalities)
}

getPersonalityCount <- function(questionnaireResults, isInvertedBigFive, attributesBigFive, selectionMatrixBigFive) {
  questionnaireResponses <- getResponseValue(questionnaireResults, isInvertedBigFive)
  personalityValues <- apply(questionnaireResponses, 1, function(x) getEvaluatedValue(x, attributesBigFive, selectionMatrixBigFive, mean, na.rm = TRUE))
  maxPersonalityValueCount <- getCountOfMaxPersonalityValues(personalityValues)
  
  return(maxPersonalityValueCount)
}