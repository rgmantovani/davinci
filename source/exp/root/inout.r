##########################################################################################################
##########################################################################################################

creating.folders =  function(){

	# Creating trace dir - populations stored
	dirs = NULL;

	# # Creating output dir (in database level)
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

output.files = function(dir){

	files = NULL;

	if(TBARS){
		files$rdata = paste(dir, "/Tbars-", METHOD, "-results.RData", sep="");
		files$csv = paste(dir, "/Tbars-", METHOD, "-results.csv", sep="");
	}else{
		files$rdata = paste(dir, "/NoTbars-", METHOD, "-results.RData", sep="");
		files$csv = paste(dir, "/NoTbars-", METHOD, "-results.csv", sep="");
	}

	return(files);
}

##########################################################################################################
##########################################################################################################
