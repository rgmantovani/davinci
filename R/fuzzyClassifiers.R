################################################################################################
################################################################################################

# • [@]Algorithm - "FRBCS.W": fuzzy rule-based classification systems with weight factor based
# on Ishibuchi’s method to handle classification tasks. See FRBCS.W

# • Control params:
# 		list(num.labels, type.mf, type.tnorm, type.implication.func, name)

# control <- list(num.labels = 3, type.mf = "TRIANGLE", type.tnorm = "MIN",
# type.implication.func = "ZADEH", name = "sim-0")

################################################################################################
################################################################################################

cl.fuzzy.w = function(train, test) {

  train$Class = as.integer(train$Class)

  #FIX ME: not runing for datasets with just one attribute
  if(ncol(test) == 2) {
    temp = as.data.frame(test[,-ncol(test)])
    colnames(temp) = colnames(test)[1]
    aux = data.frame(train[, -ncol(train)])
    colnames(aux) = colnames(train)[1]
    rg.data = matrix(apply(aux, 2, range), nrow = 2)
  } else {
    temp = test[,-ncol(test)]
    rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2)
  }
	
  model = frbs.learn(train, rg.data, method.type = "FRBCS.W")
  pred = predict(model, temp)

  pred = as.factor(as.character(pred))
  names(pred) = rownames(test)
	
  obj = list(pred = pred, model = model)
  return(obj)
}

################################################################################################
################################################################################################

# • [@]Algorithm - "FRBCS.CHI": fuzzy rule-based classification systems based on Chi’s method
# to handle classification tasks. See FRBCS.CHI

# Chi-classifier
cl.fuzzy.chi = function(train, test) {

  train$Class = as.integer(train$Class)

  #FIX ME: not runing for datasets with just one attribute
  if(ncol(test) == 2) {
    temp = as.data.frame(test[,-ncol(test)])
    colnames(temp) = colnames(test)[1]
    aux = data.frame(train[, -ncol(train)])
    colnames(aux) = colnames(train)[1]
    rg.data = matrix(apply(aux, 2, range), nrow = 2)
  } else {
   temp = test[,-ncol(test)]
   rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2)
  }

  model = frbs.learn(train, rg.data, method.type = "FRBCS.CHI")
  pred = predict(model, test[,-ncol(test)])

  pred = as.factor(as.character(pred))
  names(pred) = rownames(test)
	
  obj = list(pred = pred, model = model)
  return(obj)
}

################################################################################################
################################################################################################
