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

# Running experiment 
run = function() {

	#Creating directories
	dirs = creating.folders();

	#TODO: make automatic to run in several files 
	file = "Alternativa.arff";
	# file = "14day.arff"
	filename =  gsub("*\\.(\\w*)", "\\", file);

	cat("@File:",filename, "\n");
	cat("@Method:", METHOD, "\n");

	data = read.arff(paste( dirs$datasets.dir, file, sep=""))


	# Running for TBARS = TRUE and TBARS = FALSE
	for(tbars in TBARS){

		# - Selecting Attributes
		if(!tbars){
			cat(" @TBARS attribute: not used ... \n");
			data = data[,c(1,2,3,4,5,7)]; # comentar para usar o Tbars
		} else{
			cat(" @TBARS attribute: used ... \n");
		}

		exit = root(data, dirs, tbars);

		#output files
		files = output.files(dirs$out.dir, tbars)	

		df = exit$df;
		dump("df",files$rdata);
		write.table(x=exit$df, file=files$csv);

		allmeasures = exit$allmeasures;
		dump("allmeasures", file=files$all.meas);
		allpredictions = exit$allpredictions
		dump("allpredictions", file=files$all.pred);

	}


	
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
