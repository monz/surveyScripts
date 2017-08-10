anes <- read.csv("anes_pilot_2016.csv")

anesNfc <- select(anes, contains("nfc23"),contains("nfc32")) # see trumplowinforamtionvotersfinal.pdf footnote 24, page 14
anesNfc[anesNfc == 9] <- NA
anesNfc <- mutate(anesNfc, nfc = rowSums(anesNfc, na.rm = TRUE), nfcR = cut(nfc, breaks = c(0,6,8,Inf), labels = c("low", "med", "hig")))

print(table(anesNfc$nfcR))
print(prop.table(table(anesNfc$nfcR)))