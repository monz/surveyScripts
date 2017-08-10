# import libraries
library(hashmap)

mapping <- read.csv("subject_of_study_mapping.csv", colClasses = "character", na.strings = "NA")
mapData <- hashmap(mapping$subject_of_study, mapping$mapped_subject_of_study)

map <- function(subject) {
  mapped <- mapData[[subject]]
  return(ifelse(mapped == "NA", NA, mapped))
}