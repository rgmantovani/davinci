##########################################################################################################
##########################################################################################################

require("reshape2");
require("ggplot2");


# aux = unlist(lapply(1:nrow(data), function(i){
# 	if(all(data[i,] != 0)){
# 		return(i);
# 	}
# }));

##########################################################################################################
##########################################################################################################
errors.plot = function(data, prefix){

	# Data frame colnames
	ALGORITHMS = c("J48", "NB", "3NN", "RF", "SVM", "MLP", "FUZZY.W", "FUZZY.CHI");

	aux = lapply(1:length(data), function(k){

		df = data[[k]];
		ret = df;

		for(i in 1:8){

			# se for igual, recebe 0 (white)
			ret[which(df[,i] == df[,9]), i] = 0;

			# se for 0, mas previu 7
			ret[which(df[,i] == 7 & df[,9] == 0), i] = 1; #(seagreen2)

			#se for 0, mas previu 14						
			ret[which(df[,i] == 14 & df[,9] == 0), i] = 2; # (springgreen4)

			#se for 7, mas previu 0
			ret[which(df[,i] == 0 & df[,9] == 7), i] = 3; # (plum3)

			#se for 7, mas previu 14
			ret[which(df[,i] == 14 & df[,9] == 7), i] = 4; # (purple4)

			#se for 14, mas previu 0
			ret[which(df[,i] == 0 & df[,9] == 14), i] = 5; # (blue1)

			#se for 14, mas previu 7
			ret[which(df[,i] == 7 & df[,9] == 14), i] = 6; #(navyblue)
		}

		ret = ret[,1:8]

		colnames(ret) = ALGORITHMS;
		filename = paste(prefix, "-Single-Errors-", k , sep="")
		errors.plot.aux(ret, filename, 3);
		return(ret);
	
	});
}


##########################################################################################################
##########################################################################################################

errors.plot.aux = function(ret, filename, h){

	ret$example = 1:nrow(ret);
	df = melt(ret, id.vars=c(9));
	colnames(df) = c("Example", "Algorithm", "Error");

	setEPS();
	filename = paste("hits-", filename, ".eps", sep="");
	postscript(filename, height=h, width=10);
	g = NULL;
	g = ggplot(df, aes(x=Example, y=Algorithm, fill=factor(Error)));
	g = g + geom_tile();

  	colours = c("white", "seagreen2", "springgreen4", "plum3", "purple4", "blue1", "navyblue"); 
  	g = g + scale_fill_manual(values=colours, name ="Type of Error",
   		labels=c(" " ,"0 predicted as 7","0 predicted as 14", "7 predicted as 0", "7 predicted as 14",
   			"14 predicted as 0", "14 predicted as 7"));
	print(g)
	dev.off();
}



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
	temp = Reduce("+", aux);
	dump("temp", file=paste(prefix, "-accumulatedErrors.RData", sep=""))
	hits.plot.aux.full(temp, prefix, 3);

}


##########################################################################################################
##########################################################################################################

hits.plot.aux.full = function(ret, filename, h){

	ret$example = 1:nrow(ret);
	df = melt(ret, id.vars=c(9));
	colnames(df) = c("Example", "Algorithm", "Errors");

	setEPS();
	filename = paste("accumulatedErrors-", filename, ".eps", sep="");
	postscript(filename, height=h, width=10);
	g = NULL;
	g = ggplot(df, aes(x=Example, y=Algorithm, fill=Errors))
	g = g + geom_tile() + scale_fill_gradient(low="white", high="red")
	print(g)
	dev.off();
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
# hits.plot(data, prefix);
errors.plot(data, prefix);


data2 = dget("data/Alternativa/NoTbars-CV-all-predictions.RData")
prefix2 = "No-TBars";
# hits.plot(data2, prefix2);
errors.plot(data2, prefix2);




##########################################################################################################
##########################################################################################################