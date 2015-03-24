##########################################################################################################
##########################################################################################################

get.mean.sd = function(exec, cls){

	ret = do.call("rbind", lapply(exec, function(element){
		id = which((element)$classifier == cls)
		return(element[id,]);
	}));

	n = 1:(ncol(ret)-1);
	means = colMeans(ret[,n])
	sdsd = colSds(as.matrix(ret[,n]))
	
	ret = c(means,sdsd)[ order( c(seq_along(means), seq_along(sdsd)))];
	return(ret);
}


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