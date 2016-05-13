##########################################################################################################
##########################################################################################################
# Running 30 times ...
root = function(data, dirs, tbars) {
	
	# Running 30 times, classifiers 
	cat("/")
	exec = lapply(1:EPOCHS, function(epoch){
		res = pattern(data)
		cat("=")
		return(res)
	})
	cat("/\n")

	#Saving results
	if(tbars){
		dump("exec", file = paste(dirs$out.dir, "/Tbars-", METHOD, "-complete-trace.RData",sep=""))
	}else{
		dump("exec", file = paste(dirs$out.dir, "/NoTbars-", METHOD, "-complete-trace.RData",sep=""))
	}

	all.meas = lapply(exec, function(elem) {return(elem$acc)})
	all.pred = lapply(exec, function(elem) {return(elem$pred)})

	aux = do.call("rbind", lapply(CLASSIFIERS, function(cls) {
		ret = get.mean.sd(all.meas, cls)
		return(ret)
	}))

	df = data.frame(aux)
	colnames(df) = c("acc.mean", "acc.sd", "err.mean", "err.sd", "prec.mean", "prec.sd", 
		"recall.mean", "recall.sd", "fsc.mean", "fsc.sds")
	df$classifier = CLASSIFIERS

	obj = NULL
	obj$df = df
	obj$allmeasures = all.meas
	obj$allpredictions = all.pred

	return(obj)
}

##########################################################################################################
##########################################################################################################

# Calling classifiers ...
pattern = function(data) {

	folds = cfold(data)
	
	# Running algorithms
	exec = lapply(CLASSIFIERS, function(cls){
		ret = cross.validation(folds, cls)
		return(ret)
	})

	# measures
	accr = do.call("rbind", lapply(exec, function(elem) {return(elem$measures)}))
	colnames(accr) = c("accruacy", "error", "precision", "recall", "fscore")
	df = data.frame(accr)
	df = round(df,3)
	df$classifier = CLASSIFIERS
	
	#predictions
	predictions = do.call("cbind", lapply(exec, function(elem) {return(elem$predictions)}))
	predictions = cbind(predictions, data$Class)
	
	predictions[which(predictions == 1)] = 0
	predictions[which(predictions == 3)] = 7
	predictions[which(predictions == 2)] = 14
	
	df2 = data.frame(predictions)
	colnames(df2) = c(CLASSIFIERS, "Class")

	obj = NULL
	obj$acc = df
	obj$pred = df2

	return(obj)
}


##########################################################################################################
##########################################################################################################
