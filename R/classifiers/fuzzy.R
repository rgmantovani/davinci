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

	obj = NULL
	lv.train = levels(train$Class)
	train$Class = as.integer(train$Class)

	rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2)
	
	model = frbs.learn(train, rg.data, method.type = "FRBCS.W")
	pred = predict(model, test[,-ncol(test)])

	pred[which(pred == 1)] = 0
	pred[which(pred == 2)] = 14	
	pred[which(pred == 3)] = 7
	
	pred = as.factor(as.character(pred))
	names(pred) = rownames(test)
	
	obj$pred = pred
	obj$model = model

	return(obj)
}

################################################################################################
################################################################################################

# • [@]Algorithm - "FRBCS.CHI": fuzzy rule-based classification systems based on Chi’s method
# to handle classification tasks. See FRBCS.CHI

# Chi-classifier
cl.fuzzy.chi = function(train, test) {

	obj = NULL
	train$Class = as.integer(train$Class)
	rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2)

	model = frbs.learn(train, rg.data, method.type = "FRBCS.CHI")
	pred = predict(model, test[,-ncol(test)])

	pred[which(pred == 1)] = 0
	pred[which(pred == 3)] = 7
	pred[which(pred == 2)] = 14
	
	pred = as.factor(as.character(pred))
	names(pred) = rownames(test)
	
	obj$pred = pred
	obj$model = model

	return(obj)
}

################################################################################################
################################################################################################
