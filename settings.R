# common data
breaksOpinionChanged = c(-3,-1,0,2)
breaksOpinionChanged2 = c(-3,0,2)
breaksNfc = c(0, 4.25, 5.5, Inf) # inter quartile distance
labelsNfc = c("niedrig", "mittel", "hoch")

isInvertedNfc <- c(TRUE, FALSE, FALSE, TRUE) # points out which values must get inverted
isInvertedBigFive <- logical(length = 15)
isInvertedBigFive[c(3,6,8,15)] <- TRUE
attributesBigFive <- c("O","C","E","A","N")
selectionMatrixBigFive <- matrix(c(4,1,2,3,5, 10,8,6,7,11, 14,12,9,13,15), ncol = 3)

isInvertedBigFiveAdapted <- logical(length = 16)
isInvertedBigFiveAdapted[c(3,6,8,15)] <- TRUE
selectionMatrixBigFiveAdapted <- matrix(c(4,1,2,3,5, 10,8,6,7,11, 14,12,9,13,15, 16,NA,NA,NA,NA), ncol = 4)

timeToFinishThreshold <- 120 #(fastest 10% on welcomePage(4sec) + questionnaire(68sec) + message(19sec) + nfc(25sec))
timeToFinishUpperThreshold <- 636 #(95th percentile)
timeOnTextThreshold <- 25