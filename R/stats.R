################################################################################################
################################################################################################

#Two sided paired t-test
ttest = function(alg1, alg2, conf=0.99) {

  #applying test
  obj = t.test(alg1, alg2, paired=TRUE, alternative="two.sided", conf.level= conf)
  p.value = obj$p.value
  alpha = 1 - conf

  return(p.value < alpha)
}

################################################################################################
################################################################################################

wilcoxon = function(alg1, alg2, conf=0.99) {

  obj = wilcox.test(alg1, alg2, paired= TRUE)
  p.value = obj$p.value
  alpha = 1 - conf

  return(p.value < alpha)
}


################################################################################################
################################################################################################

# Computing the effect size using Cohen’s statistic.
cohen.stat = function(alg1, alg2) {

  d =  abs(mean(alg1) - mean(alg2)) / (sqrt((var(alg1)+var(alg2))/2)) 
  return(d)
}

################################################################################################
################################################################################################

friedman.nemenyi = function(data, conf=0.95) {

  alpha = 1 - conf
  rankData = c()
  for (i in 1:nrow(data)) {
    rankData = rbind(rankData, rank(data[i, ]))
  }

  rk = colMeans(rankData)
  fried = friedman.test(as.matrix(data))
  	
  ret = matrix(FALSE, ncol(data), ncol(data))
  diag(ret) = NA
  ret[lower.tri(ret)] = NA

  if(fried$p.value < alpha) {
    nem = posthoc.friedman.nemenyi.test(as.matrix(data))

    # See where it is differences
    for(i in 1:nrow(nem$p.value)) {
      for(j in 1:ncol(nem$p.value)) {
        elem = nem$p.value[i,j]
 	if(!is.na(elem) & elem < alpha) {
 	  ret[j,i+1] = TRUE
 	}
      }
    }
  }
 	
  obj = NULL
  #retornar uma matriz com TRUE/FALSE de onde tem diferenças
  obj$differences = ret
  #retornar o ranking das medidas tambem
  obj$ranking = rk

  return(obj)
}

################################################################################################
################################################################################################
