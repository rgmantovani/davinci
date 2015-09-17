##########################################################################################################
##########################################################################################################

# Metodos do FSelector Package
# http://cran.r-project.org/web/packages/FSelector/FSelector.pdf

##########################################################################################################
##########################################################################################################

# get.mean.sd = function(exec, cls){

	# id = 8;
	# ret = do.call("rbind", lapply(aux, function(element){
	# 	return(element[,id]);
	# }));

	# n = 1:(ncol(ret)-1);
	# means = colMeans(ret[,n])
	# sdsd = colSds(as.matrix(ret[,n]))
	
	# ret = c(means,sdsd)[ order( c(seq_along(means), seq_along(sdsd)))];
	# return(ret);
# }

##########################################################################################################
##########################################################################################################

feature.selection = function(data) {
	
	f = as.formula("Class ~ .");
	temp = data;
	temp$Class = as.numeric(as.character(temp$Class));

	cat("/");
	aux = lapply(1:EPOCHS, function(i){

		ig = information.gain(f, data);
		gr = gain.ratio(f, data);
		gr[6,1] = 0;
		
		su = symmetrical.uncertainty(f, data);
		chi.sq = chi.squared(f, data);
		lc = linear.correlation(f, temp)
		rc = rank.correlation(f, temp)
		rfi = random.forest.importance(f, data)
		rl = relief(f, data)

		ret = cbind(ig, gr, su, chi.sq, lc, rc, rfi, rl);
		colnames(ret) = c("info.gain", "gain.ratio", "symm.uncert", "chi.sq", "lin.corr",
		"rank.corr", "RF.import", "relief");
		cat("=");
		return(ret);

	});
	cat("\\n");

	return(aux);

# csf.exit =  FSelector::cfs(f, data)
# [1] "L"   "a"   "PAP" "ph" 

# cns = consistency(f, data)
# [1] "L"   "a"   "PAP" "ph" 

}

##########################################################################################################
##########################################################################################################