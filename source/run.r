##########################################################################################################
# R Code
# Base experiment 
# R. G.  Mantovani, 2014
# Start the experiment 
##########################################################################################################

setup = function() {

	files = list.files("exp/", recursive=TRUE, full.name=TRUE);	
	for(i in files){
		source(i);
	}
}

##########################################################################################################
##########################################################################################################

creating.folders =  function(){

	# Creating trace dir - populations stored
	dirs = NULL;

	# Creating folds dir - folds partitions
	folds.dir = paste(DIR, "/folds", sep="");
	if(!file.exists(folds.dir)){
		cat(" - Creating folds dir (storing dataset partitions). \n");
		dir.create(folds.dir);
	}

	# # Creating output dir (in database level)
	out.dir = paste(DIR, "/results", sep="")
	if(!file.exists(out.dir)){
		cat(" - Creating output dir.\n");
		dir.create(out.dir);
	}

	datasets.dir = paste(DIR, DATABASE, sep="");

	dirs$folds.dir = folds.dir;
	dirs$datasets.dir = datasets.dir;
	dirs$out.dir = out.dir;

	return(dirs);
}

##########################################################################################################
##########################################################################################################

# Running experiment 
run = function() {

	dirs = creating.folders();
	file = "Alternativa.arff";
	filename =  gsub("*\\.(\\w*)", "\\", file);

	cat("@File:",filename, "\n");
	data = read.arff(paste( dirs$datasets.dir, file, sep=""))

	#Dataset with no Tbars 
	# data = data[,c(1,2,3,4,5,7)]; # comentar para usar o Tbars

	exit = root(data, dirs, filename);
	dump("exit", paste(dirs$out.dir, "/results.RData", sep=""));
	write.table(x=exit, file=paste(dirs$out.dir, "/results.csv", sep=""));

}

##########################################################################################################
##########################################################################################################

main = function(){

	setup();	# loading files
	run();		# running experiment

}

##########################################################################################################
##########################################################################################################

main();

##########################################################################################################
##########################################################################################################
