##########################################################################################################
##########################################################################################################

cross.validation = function(temp, cls){

	aux = do.call("rbind", lapply(1:FOLDS, function(i){

        #treinar svm com Train, e validar com teste
        data.train = temp$train[[i]];
        data.test = temp$test[[i]];
	
		obj = do.call(cls, list(data.train, data.test));
 		
        test.acc  = acc.simple (obj$pred, data.test$Class);
    	meas.acc = acc.multi.measures(obj$pred, data.test$Class);
  
		ret = c(test.acc, meas.acc);
		ret = round(ret, 4);
		return(ret);

    })); 

	aux = colMeans(aux);
	return(aux);

}

##########################################################################################################
##########################################################################################################

leave.one.out = function(temp, cls){

	aux = do.call("rbind", lapply(1:length(temp$train), function(i) {

 		data.train = temp$train[[i]];
        data.test = temp$test[[i]];
		
		obj = do.call(cls, list(data.train, data.test));
		
		ret = cbind(obj$pred, data.test$Class);
		return(ret);
	
	}));

	pred = aux[,1];
	test = aux[,2];

	test.acc = acc.simple(pred, test);
   	meas.acc = acc.multi.measures(pred, test);
  
	ret = c(test.acc, meas.acc);
	ret = round(ret, 4);
	return(ret);

}

##########################################################################################################
##########################################################################################################