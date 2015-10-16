##########################################################################################################
##########################################################################################################

creating.folders =  function(){

	# Creating trace dir - populations stored
	dirs = NULL;

	# Creating output dir (in database level)
	out.dir = paste(DIR, "/results", sep="")
	if(!file.exists(out.dir)){
		cat(" - Creating output dir.\n");
		dir.create(out.dir);
	}

	dirs$datasets.dir = paste(DIR, DATABASE, sep="");
	dirs$out.dir = out.dir;

	return(dirs);
}

##########################################################################################################
##########################################################################################################

output.files = function(dir, tbars){

	files = NULL;

	if(tbars){
		files$all.meas = paste(dir, "/MLP_KNN-Tbars-", METHOD, "-all-measures.RData", sep="");
		files$all.pred = paste(dir ,"/MLP_KNN-Tbars-", METHOD, "-all-predictions.RData", sep="");
		files$rdata = paste(dir ,"/MLP_KNN-Tbars-", METHOD, "-results.RData", sep="");
		files$csv = paste(dir ,"/MLP_KNN-Tbars-", METHOD, "-results.csv", sep="");
	}else{
		files$all.meas = paste(dir, "/MLP_KNN-NoTbars-", METHOD, "-all-measures.RData", sep="");
		files$all.pred = paste(dir, "/MLP_KNN-NoTbars-", METHOD, "-all-predictions.RData", sep="");
		files$rdata = paste(dir, "/MLP_KNN-NoTbars-", METHOD, "-results.RData", sep="");
		files$csv = paste(dir, "/MLP_KNN-NoTbars-", METHOD, "-results.csv", sep="");
	}

	return(files);
}

##########################################################################################################
##########################################################################################################
