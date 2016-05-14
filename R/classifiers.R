################################################################################################
# Classifiers
################################################################################################

cl.c45 = function(train, test) {
	
  if(ncol(test) == 2) {
    temp = as.data.frame(test[,-ncol(test)])
    colnames(temp) = colnames(test)[1]
  } else {
    temp = test[,-ncol(test)]
  }
	
  model = RWeka::J48(Class ~ ., train)
  pred = predict(model, temp, type="class")
  names(pred) = row.names(test)
	
  obj = list(pred = pred, model = model)
  return(obj)
}

################################################################################################
################################################################################################

cl.mlp = function(train, test, h=20) {

  if(ncol(test) == 2) {
    temp = as.data.frame(test[,-ncol(test)])
    colnames(temp) = colnames(test)[1]
    test = temp
  } else {
    temp = test[,-ncol(test)]
  }

  MLP = RWeka::make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
  model = MLP(Class ~ ., train, control = RWeka::Weka_control(H=h))
  pred = predict(model, temp, type="class")
  names(pred) = row.names(test)

  obj = list(pred = pred, model = model)
  return(obj)
}

################################################################################################
################################################################################################

cl.nb = function(train, test) {

  if(ncol(test) == 2) {
    temp = as.data.frame(test[,-ncol(test)])
    colnames(temp) = colnames(test)[1]
    test = temp
  } else {
    temp = test[,-ncol(test)]
  }
		
  model = naiveBayes(Class ~ ., train)
  pred = predict(model, temp , type="class")
  names(pred) = row.names(test)

  obj = list(pred = pred, model = model)
  return(obj)
}

################################################################################################
################################################################################################

cl.nn = function(train, test, k=3) {

  if(ncol(test) == 2) {
    temp = as.data.frame(test[,-ncol(test)])
    colnames(temp) = colnames(test)[1]
    test = temp
  } else {
    temp = test[,-ncol(test)]
  }

  model = kknn(Class ~., train, temp, k=k)
  pred = model$fitted.values
  names(pred) = rownames(test)

  obj = list(pred = pred, model = model)
  return(obj)
}

################################################################################################
################################################################################################

cl.rf = function(train, test, type="response") {

  if(ncol(test) == 2) {
    temp = as.data.frame(test[,-ncol(test)])
    colnames(temp) = colnames(test)[1]
    test = temp
  } else {
    temp = test[,-ncol(test)]
  }

  model = randomForest(Class ~ ., train)
  pred = predict(model, temp, type=type)

  if(type != "prob") {
    names(pred) = row.names(test)
  }

  obj = list(pred = pred, model = model)
  return(obj)
}

################################################################################################
################################################################################################

cl.svm = function(train, test) {

  if(ncol(test) == 2) {
    temp = as.data.frame(test[,-ncol(test)])
    colnames(temp) = colnames(test)[1]
    test = temp
  } else {
    temp = test[,-ncol(test)]
  }

  model = svm(Class ~ ., train, kernel="radial")
  pred = predict(model, temp)
  names(pred) = row.names(test)

  obj = list(pred = pred, model = model)
  return(obj)
}

################################################################################################
################################################################################################
