p_getResponseValue <- function(responseId, responseValue) {
  value <- responseValue
  # due to numeric switch upfuck use if-statement instead xD
  if (responseId == 3 | responseId == 6 | responseId == 8 | responseId == 15) {
    value <- 8 - responseValue
  }
  
  return(value)
}

getResponseValue <- function(responseId, responseValue) {
  mapply(function(x,y) p_getResponseValue(x, y), responseId, responseValue)
}

getResponseValue <- function(responseValue) {
  # dimValue <- dim(responseValue)[2]
  # if(is.null(dimValue) | dimValue != 15) {
  #   return(NULL)
  # }
  mapply(function(x,y) p_getResponseValue(x, y), 1:15, responseValue)
}

getPersonalityVectors <- function(questionnaireValues) {
  openToExperience <- rowMeans(questionnaireValues[, c(4,10,14)])
  conscientiousness <- rowMeans(questionnaireValues[, c(1,8,12)])
  extraversion <- rowMeans(questionnaireValues[, c(2,6,9)])
  agreeable <- rowMeans(questionnaireValues[, c(3,7,13)])
  neuroticism <- rowMeans(questionnaireValues[, c(5,11,15)])
  
  oceanValues <- data.frame(openToExperience, conscientiousness, extraversion, agreeable, neuroticism)
  
  return(oceanValues)
}

getPersonalityVector <- function(questionnaireValues) {
  openToExperience <- mean(questionnaireValues[c(4,10,14)])
  conscientiousness <- mean(questionnaireValues[c(1,8,12)])
  extraversion <- mean(questionnaireValues[c(2,6,9)])
  agreeable <- mean(questionnaireValues[c(3,7,13)])
  neuroticism <- mean(questionnaireValues[c(5,11,15)])
  
  oceanValues <- data.frame(openToExperience, conscientiousness, extraversion, agreeable, neuroticism)
  
  return(oceanValues)
}

indexToPersonality <- function(personalityIndex) {
  if (personalityIndex == 1) {
    return('O')
  } else if(personalityIndex == 2) {
    return('C')
  } else if(personalityIndex == 3) {
    return('E')
  } else if(personalityIndex == 4) {
    return('A')
  } else if(personalityIndex == 5) {
    return('N')
  } else {
    return('noPersonality')
  }
}

getClearPersonalities <- function(personalityVectors) {
  personalityIndexes <- apply(personalityVectors, 1, function(x) {h <- which(x == max(x)); sample(h, 1)})

  return(sapply(personalityIndexes, indexToPersonality))
}

getAmbiguousPersonalities <- function(personalityVectors) {
#  personalityIndexes <- apply(personalityVectors, 1, function(x) {h <- which(x == max(x)); sample(h, 1)})
  personalityIndexes <- apply(personalityVectors, 1, function(x) {h <- which(x == max(x))})
  
  #return(sapply(personalityIndexes, indexToPersonality))
  return(rapply(personalityIndexes, function(x) {sapply(x, indexToPersonality)}, how = "list"))
}