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

getEvaluatedValue <- function(data, attributes, selectionMatrix, func) {
  values <- apply(selectionMatrix, 1, function(x) func(data[x]))
  
  return(data.frame(attributes, values))
}

getAttribute <- function(evaluatedValues, func) {
  unlist(lapply(evaluatedValues, function(x) {x[[1]][which(x[[2]] == func(x[[2]]))][1]}))
}