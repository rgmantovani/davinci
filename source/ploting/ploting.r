##########################################################################################################
##########################################################################################################

require("reshape2");
require("ggplot2");

##########################################################################################################
##########################################################################################################

hits.plot = function(data){

	METRICS = c("STLG" , "STAT", "INF", "LANDM", 
		"MODEL", "TIME", "COMP", "ALL", "FSELEC");

	ALGORITHMS = c("J48", "MLP", "NB", "3NN", "RF", "SVM", "ENS");

	aux = lapply(1:9, function(i){
		cn = paste(ALGORITHMS, METRICS[i], sep=".")
		cn = c(cn, "Real");
		df = data[[i]]$data;
		colnames(df) = cn;
		ret = abs(df[,1:7] - df[,8]);

		hits.plot.aux(ret, METRICS[i], 3);

		return(ret);
	});

	full = do.call("cbind", aux);
	hits.plot.aux(full, "Full", 9);
}

##########################################################################################################
##########################################################################################################

hits.plot.aux = function(full, filename, h){

	df = melt(full);
	colnames(df) = c("Dataset", "Algorithm", "Errors");

	setEPS();
	filename = paste("hits-", filename, ".eps", sep="");
	postscript(filename, height=h, width=10);
	g = ggplot(df, aes(x=Dataset, y=Algorithm, fill=Errors))
	g = g + geom_tile() + scale_fill_gradient(low="darkgray", high="black", guide=FALSE)
	print(g)
	dev.off();

}

##########################################################################################################
##########################################################################################################