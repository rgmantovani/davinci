##########################################################################################################
##########################################################################################################

doingStatisticalComparisons = function(data) {

	# subsample measures
  tbars = lapply(data$meas.tbars, function(elem){
    return(elem[1:8,])
  })

  notbars = lapply(data$meas.no.tbars, function(elem){
    return(elem[1:8,])
  })

	pairedTest(notbars, "notbars", 0.90)
	pairedTest(notbars, "notbars", 0.95)
	pairedTest(notbars, "notbars", 0.99)

	pairedTest(tbars, "tbars", 0.90)
	pairedTest(tbars, "tbars", 0.95)
	pairedTest(tbars, "tbars", 0.99)

}

##########################################################################################################
##########################################################################################################