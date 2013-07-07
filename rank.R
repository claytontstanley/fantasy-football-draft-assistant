PATH = getPathToThisFile()
library(stringr)

histFrm = read.csv(str_c(PATH, "/hist.csv"))

histFrm$score = with(histFrm, rushYds/10 + rushTds*6 + recYds/10 + recTds*6 + passYds/25 + passTds*4 + kickFgs*3)

write.csv(str_c(PATH, "/score.csv"))

