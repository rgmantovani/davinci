################################################################################################
################################################################################################
# Classifiers

cl.c45 = function(train, test) {
	
	obj = NULL
	model = RWeka::J48(Class ~ ., train)
	pred = predict(model, test[,-ncol(test)], type="class")
	names(pred) = row.names(test)
	
	obj$pred = pred
	obj$model = model

	return(obj)
}

################################################################################################
################################################################################################

cl.mlp = function(train, test, h=2) {

	obj = NULL
	MLP = RWeka::make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
	model = MLP(Class ~ ., train, control = RWeka::Weka_control(H=h))
	pred = predict(model, test[,-ncol(test)], type="class")
	names(pred) = row.names(test)

	obj$pred = pred
	obj$model = model
	return(obj)
}

################################################################################################
################################################################################################

cl.nb = function(train, test) {
		
	obj = NULL
	model = naiveBayes(Class ~ ., train)
	pred = predict(model, test[,-ncol(test)], type="class")
	names(pred) = row.names(test)

	obj$pred = pred
	obj$model = model
	return(obj)
}

################################################################################################
################################################################################################

cl.nn = function(train, test, k=3) {

	obj = NULL
	model = kknn(Class ~., train, test[,-ncol(test)], k=k)
	pred = model$fitted.values
	names(pred) = rownames(test)

	obj$pred = pred
	obj$model = model
	return(obj)
}

################################################################################################
################################################################################################

cl.rf = function(train, test, type="response") {

	obj = NULL
	model = randomForest(Class ~ ., train)
	pred = predict(model, test[,-ncol(test)], type=type)

	if(type != "prob"){
		names(pred) = row.names(test)
	}

	obj$pred = pred
	obj$model = model
	return(obj)
}

################################################################################################
################################################################################################

cl.svm = function(train, test) {

	obj = NULL
	model = svm(Class ~ ., train, kernel="radial")

	pred = predict(model, test[,-ncol(test)])
	names(pred) = row.names(test)

	obj$pred = pred
	obj$model = model
	return(obj)

}

################################################################################################
################################################################################################