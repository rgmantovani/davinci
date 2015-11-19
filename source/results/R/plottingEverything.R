##########################################################################################################
##########################################################################################################

plottingEverything = function(data){

	# subsample predictions
  pred.tbars = lapply(data$pred.tbars, function(elem){
    return(elem[,c(1:8, ncol(elem))])
  })

  pred.no.tbars = lapply(data$pred.no.tbars, function(elem){
    return(elem[,c(1:8, ncol(elem))])
  })

  # Summed error by type plot
	ret1 = sumError(pred.tbars)
	typeOfErrorsPlot(ret1, prefix = "Tbars")
	ret2 = sumError(pred.no.tbars)
	typeOfErrorsPlot(ret2, prefix = "NoTbars")
		
  # Error plots (for each execution)
  # allErrorsPlot(pred.tbars, prefix = "Tbars");
  # allErrorsPlot(pred.no.tbars, prefix = "NoTbars");

  # Summed Error plot
  hitsPlot(pred.tbars, prefix = "Tbars")
  hitsPlot(pred.no.tbars, prefix = "NoTbars")


  mlp.tbars = lapply(data$meas.tbars, function(elem){
    return(elem[9:57, 1:5])
  })

  mlp.no.tbars = lapply(data$meas.no.tbars, function(elem){
    return(elem[9:57, 1:5])
  })

  MlpPlot(mlp.tbars, prefix = "Tbars")
  MlpPlot(mlp.no.tbars, prefix = "NoTbars")

  knn.tbars = lapply(data$meas.tbars, function(elem){
    return(elem[58:73, 1:5])
  })

  knn.no.tbars = lapply(data$meas.no.tbars, function(elem){
    return(elem[58:73, 1:5])
  })

  KnnPlot(knn.tbars, prefix = "Tbars")
  KnnPlot(knn.no.tbars, prefix = "NoTbars")

}

##########################################################################################################
##########################################################################################################

