##########################################################################################################
##########################################################################################################

require("reshape2");
require("ggplot2");


# data = dget("data/Alternativa/Tbars-CV-all-predictions.RData")
# temp = data[[1]]

##########################################################################################################
##########################################################################################################

hits.plot = function(data, prefix){

	# Data frame colnames
	ALGORITHMS = c("J48", "NB", "3NN", "RF", "SVM", "MLP", "FUZZY.W", "FUZZY.CHI");

	aux = lapply(1:length(data), function(k){

		df = data[[k]];
		ret = abs(df[,1:8] - df[,9]);
		for(i in 1:8){
			ret[which( ret[,i] != 0), i] = 1;
		}

		colnames(ret) = ALGORITHMS;
		filename = paste(prefix, "-", k , sep="")
		hits.plot.aux(ret, filename, 3);

		return(ret);
	});

	#plotar o somatorio dos erros
}


##########################################################################################################
##########################################################################################################

hits.plot.aux = function(ret, filename, h){

	ret$example = 1:nrow(ret);
	df = melt(ret, id.vars=c(9));
	colnames(df) = c("Example", "Algorithm", "Errors");

	setEPS();
	filename = paste("hits-", filename, ".eps", sep="");
	postscript(filename, height=h, width=10);
	g = NULL;
	g = ggplot(df, aes(x=Example, y=Algorithm, fill=Errors))
	g = g + geom_tile() + scale_fill_gradient(low="darkgray", high="black", guide=FALSE)
	print(g)
	dev.off();
}


##########################################################################################################
##########################################################################################################

data = dget("data/Alternativa/Tbars-CV-all-predictions.RData")
prefix = "Tbars";
hits.plot(data, prefix);

data2 = dget("data/Alternativa/NoTbars-CV-all-predictions.RData")
prefix2 = "No-TBars";
hits.plot(data, prefix2);




##########################################################################################################
##########################################################################################################