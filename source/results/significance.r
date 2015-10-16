##########################################################################################################
##########################################################################################################

require("PMCMR")			# Statistic Tests

source("../exp/stats.r")

notbars = dget("Alternativa/NoTbars-CV-all-measures.RData")
tbars = dget("Alternativa/Tbars-CV-all-measures.RData")

##########################################################################################################
##########################################################################################################

pairedTest = function(data, name, conf){

	perf = do.call("rbind", lapply(data, function(elem){
		return(elem$accruacy)
	}))

	alg = c("c45", "nb", "nn", "rf", "svm", "mlp", "fuzzy.w", "fuzzy.chi")
	mat = matrix(FALSE, ncol(perf), ncol(perf));

	# for(i in 1:(ncol(perf)-1)){
	# 	for(j in (i+1):ncol(perf)){
	mat = matrix(FALSE, ncol(perf), ncol(perf));

	for(i in 1:8){
		for(j in 1:8){
			if(i!=j){
				mat[i,j] = wilcoxon(perf[,i], perf[,j], conf)
			}
		}
	}

	colnames(mat) = alg
	rownames(mat) = alg

	filename = paste(name, "-", toString(conf) ,".csv", sep="")
	write.csv(x = mat, file = filename, sep=",")
}

##########################################################################################################
##########################################################################################################

friedTest = function(tbars, notbars, conf){
	
	perf1 = do.call("rbind", lapply(tbars, function(elem){
		return(elem$accruacy)
	}))

	#friedman data1
	obj1 = friedman.nemenyi(perf1);

	perf2 = do.call("rbind", lapply(notbars, function(elem){
		return(elem$accruacy)
	}))

	#friedman data2
	obj2 = friedman.nemenyi(perf2)

	#friedman together
	obj3 = friedman.nemenyi(rbind(perf1, perf2))
}


##########################################################################################################
##########################################################################################################

pairedTest(notbars, "notbars", 0.90)
pairedTest(notbars, "notbars", 0.95)
pairedTest(notbars, "notbars", 0.99)

pairedTest(tbars, "tbars", 0.90)
pairedTest(tbars, "tbars", 0.95)
pairedTest(tbars, "tbars", 0.99)

##########################################################################################################
##########################################################################################################
