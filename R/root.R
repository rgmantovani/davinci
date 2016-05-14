##########################################################################################################
##########################################################################################################
# Running 30 times ...
root = function(data, filename) {
	
	# Running 30 times, classifiers 
	cat("/")
	exec = lapply(1:EPOCHS, function(epoch){
		obj = pattern(data)
		save(obj, file = paste0("output/", filename, "/", epoch, "_execution.RData"))
		cat("=")
		return(obj)
	})
	cat("/\n")

	
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
	df$dataset = filename

	obj = list(df = df, allmeasures = all.meas, allpredictions = all.pred)
	save(obj, file = paste0("output/", filename, "/final_results.RData"))
	return(obj)
}

##########################################################################################################
##########################################################################################################

# Calling classifiers ...
pattern = function(data) {

	folds = cfold(data)
	
	# Running algorithms
	exec = lapply(CLASSIFIERS, function(cls){
		ret = crossValidation(folds, cls)
		return(ret)
	})

	# Extract Measures
	accr = do.call("rbind", 
		lapply(exec, function(elem) {
			return(elem$measures)
		})
	)
	colnames(accr) = c("accruacy", "error", "precision", "recall", "fscore")

	df = data.frame(accr)
	df = round(df,3)
	df$classifier = CLASSIFIERS
	
	# Extract Predictions
	predictions = do.call("cbind", 
		lapply(exec, function(elem) {
			return(elem$predictions)
		})
	)
	predictions = cbind(predictions, data$Class)
	
	predictions[which(predictions == 1)] = 0
	predictions[which(predictions == 3)] = 7
	predictions[which(predictions == 2)] = 14
	
	df2 = data.frame(predictions)
	colnames(df2) = c(CLASSIFIERS, "Class")

	obj = list(acc = df, pred = df2)
	return(obj)
}


##########################################################################################################
##########################################################################################################
