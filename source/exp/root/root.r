##########################################################################################################
##########################################################################################################
# Running 30 times ...
root = function(data, dirs) {
	
	# Running 30 times, classifiers (mapply)
	cat("/")
	exec = lapply(1:EPOCHS, function(j){
		res = pattern(data, j);
		cat("=");
		return(res);
	});
	cat("/\n");

	#Saving results
	if(TBARS){
		dump("exec", file = paste(dirs$out.dir, "/Tbars-", METHOD, "-complete-trace.RData",sep=""));
	}else{
		dump("exec", file = paste(dirs$out.dir, "/NoTbars-", METHOD, "-complete-trace.RData",sep=""));
	}

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
pattern = function(data, i) {

	folds = NULL;
	if(METHOD == "CV"){
		folds = cfold(data);
	}else if(METHOD == "LOO"){
		folds = oneout(data);
	}

	# Running algorithms
	exec = lapply(CLASSIFIERS, function(cls) {
	
		ret = NULL;
		if(METHOD == "CV"){
			ret = cross.validation(folds, cls);
		}
		else if(METHOD == "LOO"){
			ret = leave.one.out(folds, cls);
		}
		return(ret);
	});

	#nome nas colunas (classificador, erro, precision, recall, fscore)
	accr = do.call("rbind", exec)
	colnames(accr) = c("accruacy","error", "precision", "recall", "fscore");
	df = data.frame(accr);
	df = round(df,3);
	df$classifier = CLASSIFIERS;
	
	return(df);
}


##########################################################################################################
##########################################################################################################
