##########################################################################################################
##########################################################################################################

pairedTest = function(data, name, conf){

	perf = do.call("rbind", lapply(data, function(elem){
		return(elem$accruacy)
	}))

	alg = c("c45", "nb", "nn", "rf", "svm", "mlp", "fuzzy.w", "fuzzy.chi")
	mat = matrix(FALSE, ncol(perf), ncol(perf));

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
