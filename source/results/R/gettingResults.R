##########################################################################################################
##########################################################################################################

gettingResults = function() {

  trace.tbars    = dget("data/new-results/Tbars-CV-complete-trace.RData")  
  trace.no.tbars = dget("data/new-results/NoTbars-CV-complete-trace.RData")
  meas.tbars     = dget("data/new-results/MLP_KNN-Tbars-CV-all-measures.RData")
  meas.no.tbars  = dget("data/new-results/MLP_KNN-NoTbars-CV-all-measures.RData")
  pred.tbars     = dget("data/new-results/MLP_KNN-Tbars-CV-all-predictions.RData")
  pred.no.tbars  = dget("data/new-results/MLP_KNN-NoTbars-CV-all-predictions.RData")

  obj = list(trace.tbars = trace.tbars, trace.no.tbars = trace.no.tbars, meas.tbars = meas.tbars,
      meas.no.tbars = meas.no.tbars, pred.tbars = pred.tbars, pred.no.tbars = pred.no.tbars)

  return(obj)
}

##########################################################################################################
##########################################################################################################
