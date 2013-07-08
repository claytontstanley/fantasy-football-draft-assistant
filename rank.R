PATH = getPathToThisFile()
library(stringr)

histFrm = read.csv(str_c(PATH, "/hist.csv"))

histFrm$score = with(histFrm, rushYds/10 + rushTds*6 + recYds/10 + recTds*6 + passYds/25 + passTds*4 + kickFgs*3)
histFrm = histFrm[with(histFrm, order(-score)),]
histFrm$rank = 1:length(histFrm$score)

histFrm

write.csv(histFrm, str_c(PATH, "/score.csv"), row.names=F)

