##########################################################################################################
##########################################################################################################

KnnPlot = function(temp, prefix = NULL){

	rr = array(unlist(temp), dim = c(nrow(temp[[1]]), ncol(temp[[1]]), length(temp)))
	desvios = apply(rr, c(1,2), sd) 
	colnames(desvios) = c("sd.Accuracy", "sd.Error", "sd.Precision", "sd.Recall", "sd.F-Score")

	medias = Reduce("+", temp) / length(temp)
	medias$hidden = seq(from=1, to=31, by = 2)
	colnames(medias) = c("Accuracy", "Error", "Precision", "Recall", "F-Score", "Hidden")
	
	df = melt(medias, id.vars=c(2,6))
	df$SD = c(desvios[,1], desvios[,3], desvios[,4], desvios[,5])
	colnames(df) = c("Error", "Hidden_Units", "Measure", "Value", "SD")

	setEPS();
	filename = paste(prefix, "-KNN.eps", sep="")
	postscript(filename, height=5, width=10);

	g = NULL;
	g = ggplot(df, aes(x=Hidden_Units, y=Value, fill=Measure, colour=Measure, 
		linetype=Measure, shape=Measure))
	g = g + geom_errorbar(aes(ymin=Value-SD, ymax=Value+SD), width=.1)
  g = g +  geom_line() + geom_point()
 	g = g + scale_x_continuous("Hidden_Units", limits = c(1, 31))
	g = g + ylab("Measure values") + xlab("Number of Nearest Neighbors");
	g = g + ggtitle("k-NN performance");
 	print(g);

	dev.off();

}

##########################################################################################################
##########################################################################################################


MlpPlot = function(temp, prefix = NULL){

	rr = array(unlist(temp), dim = c(nrow(temp[[1]]), ncol(temp[[1]]), length(temp)))
	desvios = apply(rr, c(1,2), sd) 
	colnames(desvios) = c("sd.Accuracy", "sd.Error", "sd.Precision", "sd.Recall", "sd.F-Score")

	medias = Reduce("+", temp) / length(temp)
	medias$hidden = 2:(nrow(medias)+1)
	colnames(medias) = c("Accuracy", "Error", "Precision", "Recall", "F-Score", "Hidden")
	
	df = melt(medias, id.vars=c(2,6))
	df$SD = c(desvios[,1], desvios[,3], desvios[,4], desvios[,5])
	colnames(df) = c("Error", "Hidden_Units", "Measure", "Value", "SD")

	setEPS();
	filename = paste(prefix, "-MLP.eps", sep="")
	postscript(filename, height=5, width=10);

	g = NULL;
	g = ggplot(df, aes(x=Hidden_Units, y=Value, fill=Measure, colour=Measure, 
		linetype=Measure, shape=Measure))
	g = g + geom_errorbar(aes(ymin=Value-SD, ymax=Value+SD), width=.1)
  g = g +  geom_line() + geom_point()
 	g = g + scale_x_continuous(limits = c(2, 50))
 	
	g = g + ylab("Measure values") + xlab("Number of hidden units");
	g = g + ggtitle("MLP performance");
 	print(g);
	dev.off();

}


##########################################################################################################
##########################################################################################################

allErrorsPlot = function(data, prefix){

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

hitsPlot = function(data, prefix = NULL){

	# Data frame colnames
	ALGORITHMS = c("J48", "NB", "3NN", "RF", "SVM", "MLP", "FUZZY.W", "FUZZY.CHI");

	aux = lapply(1:length(data), function(k){

		df = data[[k]];
		ret = abs(df[,1:8] - df[,9]);
		for(i in 1:8){
			ret[which( ret[,i] != 0), i] = 1;
		}

		colnames(ret) = ALGORITHMS;
		# filename = paste(prefix, "-", k , sep="")
		# hits.plot.aux(ret, filename, 3);
		return(ret);
	});

	#plotar o somatorio dos erros
	temp = Reduce("+", aux);
	# dump("temp", file=paste(prefix, "-accumulatedErrors.RData", sep=""))
	hits.plot.aux.full(temp, prefix, 3);

}


##########################################################################################################
##########################################################################################################

hits.plot.aux.full = function(ret, filename, h){

	ret$example = 1:nrow(ret);
	df = melt(ret, id.vars=c(9));
	colnames(df) = c("Example", "Algorithm", "Errors");

	setEPS();
	filename = paste(filename, "-accumulatedErrors.eps", sep="");
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