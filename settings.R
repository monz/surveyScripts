# import external libraries
library(dplyr)

# common data
breaksOpinionChanged = c(-3,-1,0,2)
breaksOpinionChanged2 = c(-3,0,2)
breaksNfc = c(0, 3.84, 5.68, Inf) # former nfc mean +- 1sd
labelsNfc = c("niedrig", "mittel", "hoch")
# breaksNfc = c(0, 5.7, Inf)
# labelsNfc = c("niedrig", "hoch")

isInvertedNfc <- c(TRUE, FALSE, FALSE, TRUE) # points out which values must get inverted
isInvertedBigFive <- logical(length = 15)
isInvertedBigFive[c(3,6,8,15)] <- TRUE
attributesBigFive <- c("O","C","E","A","N")
selectionMatrixBigFive <- matrix(c(4,1,2,3,5, 10,8,6,7,11, 14,12,9,13,15), ncol = 3)

isInvertedBigFiveAdapted <- logical(length = 16)
isInvertedBigFiveAdapted[c(3,6,8,15)] <- TRUE
selectionMatrixBigFiveAdapted <- matrix(c(4,1,2,3,5, 10,8,6,7,11, 14,12,9,13,15, 16,NA,NA,NA,NA), ncol = 4)