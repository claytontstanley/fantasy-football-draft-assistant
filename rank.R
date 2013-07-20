PATH = getPathToThisFile()
library(stringr)
library(plyr)

generateScore = function() {
	
	histFrm = read.csv(str_c(PATH, "/hist-real.csv"))

	histFrm$score = with(histFrm, rushYds/10 + rushTds*6 + recYds/10 + recTds*6 + passYds/25 + passTds*4 + fgs0.20*3 + fgs20.30*3 + fgs30.40*4 + fgs40.50*5 + fgs50.inf*6)
	histFrm = histFrm[with(histFrm, order(-score)),]
	histFrm$rank = 1:length(histFrm$score)

	histFrm

	write.csv(histFrm, str_c(PATH, "/score.csv"), row.names=F)
}

generateHist = function() {
	rbFrm = read.csv(str_c(PATH, "/hist-rbs.csv"))
	rbHistFrm = with(rbFrm, data.frame(name=Name, type="rb", rushYds=Yds, rushTds=TD, recYds=Yds.1, recTds=TD.1))
	qbFrm = read.csv(str_c(PATH, "/hist-qbs.csv"))
	qbHistFrm = with(qbFrm, data.frame(name=Name, type="qb", rushYds=Rush, rushTds=TD.1, passYds=Yds, passTds=TD))
	kFrm = read.csv(str_c(PATH, "/hist-ks.csv"))
	kHistFrm = with(kFrm, data.frame(name=Name, type="k", fgs0.20=X0.19, fgs20.30=X20.29, fgs30.40=X30.39, fgs40.50=X40.49, fgs50.inf=X50.))
	wrFrm = read.csv(str_c(PATH, "/hist-wrs.csv"))
	wrHistFrm = with(wrFrm, data.frame(name=Name, type='wr', recYds=Yds, recTds=TD+TD.1+TD.2))
	teFrm = read.csv(str_c(PATH, "/hist-tes.csv"))
	teHistFrm = with(teFrm, data.frame(name=Name, type='te', recYds=Yds, recTds=TD+TD.1))
    histFrm = rbind.fill(rbHistFrm, qbHistFrm, kHistFrm, wrHistFrm, teHistFrm)
    # converts all factors to numeric; got this linenoise from here: http://stackoverflow.com/questions/8596466/r-change-all-columns-of-type-factor-to-numeric
    histFrm$name = as.character(histFrm$name)
    histFrm$type = as.character(histFrm$type)
    histFrm = modifyList(histFrm, lapply(histFrm[, sapply(histFrm, is.factor)], function(x) as.numeric(as.character(x))))
    histFrm[is.na(histFrm)] = 0
	write.csv(histFrm, str_c(PATH, "/hist-real.csv"), row.names=F)
}
	
generateHist()
generateScore()
