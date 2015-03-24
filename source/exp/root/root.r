##########################################################################################################
##########################################################################################################
# Running 30 times ...
root = function(data, dirs, filename) {
	
	#Reading/Creating folds from data sampling
	path = paste(dirs$folds.dir,"/", filename, sep="");
	if(!file.exists(path)){
		dir.create(path)
	}

	# Running 30 times, classifiers (mapply)
	cat("/")
	exec = lapply(1:EPOCHS, function(j){
		res = pattern(data, path, filename, j);
		cat("=");
		return(res);
	});
	cat("/\n");

	#Saving results
	dump("exec", file=paste(dirs$out.dir, "/complete-trace.RData", sep=""));

	aux = do.call("rbind", lapply(CLASSIFIERS, function(cls) {
		ret = get.mean.sd(exec, cls)
		return(ret);
	}));

	df = data.frame(aux);
	colnames(df) = c("acc.mean", "acc.sd", "err.mean", "err.sd", "prec.mean", "prec.sd", 
		"recall.mean", "recall.sd", "fsc.mean", "fsc.sds");
	df$classifier = CLASSIFIERS;

	return(df);
}

##########################################################################################################
##########################################################################################################

# Calling classifiers ...
pattern = function(data, path, filename, i) {

	# dumped.file = paste(path, "/", "fold-", i, ".RData", sep="");
	
	# 9 - 1 permutation | load previous division
	# if(!file.exists(dumped.file)){
	folds = cfold(data);
		# dump("folds", dumped.file);
	# }else{
		# folds = dget(dumped.file);
	# }
	
	# Running algorithms
	exec = lapply(CLASSIFIERS, function(cls) {
		ret = cross.validation(folds, cls);
		return(ret);
	});

	#nome nas colunas (classificador, erro, precision, recall, fscore)
	accr = do.call("rbind", exec)
	colnames(accr) = c("accruacy","error", "precision", "recall", "fscore");
	df = data.frame(accr);
	df$classifier = CLASSIFIERS;
	
	return(df);
}


##########################################################################################################
##########################################################################################################
