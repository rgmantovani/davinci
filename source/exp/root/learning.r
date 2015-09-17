##########################################################################################################
##########################################################################################################

cross.validation = function(folds, cls){

	#Running FOLD times
	aux = lapply(1:FOLDS, function(i){

        #treinar svm com Train, e validar com teste
        data.train = folds$train[[i]];
        data.test = folds$test[[i]];
	
		obj = do.call(cls, list(data.train, data.test));
 		
        test.acc  = acc.simple (obj$pred, data.test$Class);
    	meas.acc = acc.multi.measures(obj$pred, data.test$Class);

		result = NULL;

		result$ret = c(test.acc, meas.acc);
		result$ret = round(result$ret, 4);
		result$pred = obj$pred;
		return(result);

    }); 

	#concatenating measures
	meas = do.call("rbind", lapply(aux, function(elem) {return(elem$ret)}));

	v = unlist(lapply(aux, function(elem) {return(elem$pred)})); 
	pred = v[order(as.numeric(names(v)))]

	#concatenating predictions
	ret = NULL;
	ret$measures = colMeans(meas);
	ret$predictions = pred;
	return(ret);

}

##########################################################################################################
##########################################################################################################