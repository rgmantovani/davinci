################################################################################################
################################################################################################

# Classification Fuzzy Algorithms:

# Description of the control Parameters

# 	• num.labels: a positive integer to determine the number of labels (linguistic terms). The default value is 7.

# 	• type.mf: the following type of the membership function. The default value is GAUSSIAN. For more detail, see fuzzifier.
# 		– TRIANGLE: it refers triangular shape.
# 		– TRAPEZOID: it refers trapezoid shape.
# 		– * GAUSSIAN: it refers gaussian shape.
# 		– SIGMOID: it refers sigmoid.
# 		– BELL: it refers generalized bell.

# • type.defuz: the type of the defuzzification method as follows. The default value is WAM. For more detail, see defuzzifier.
		# – * WAM: the weighted average method.
		# – FIRST.MAX: the first maxima.
		# – LAST.MAX: the last maxima.
		# – MEAN.MAX: the mean maxima.
		# – COG: the modified center of gravity (COG).

# • type.tnorm: the type of conjunction operator (t-norm). The following are options of t-norm available. 
# For more detail, please have a look at inference. The default value is MIN.
		# – * MIN means standard type (minimum).
		# – HAMACHER means Hamacher product.
		# – YAGER means Yager class (with tao = 1).
		# – PRODUCT means product.
		# – BOUNDED mean bounded product.

# • type.snorm: the type of disjunction operator (s-norm). The following are options of s-norm available. 
# For more detail, please have a look at inference. The default value is MAX.
		# – * MAX means standard type (maximum).
		# – HAMACHER means Hamacher sum.
		# – YAGER means Yager class (with tao = 1).
		# – SUM means sum.
		# – BOUNDED mean bounded sum.

# • type.implication.func: the type of implication function. The following
# are options of implication function available: DIENES_RESHER, LUKASIEWICZ,
# ZADEH, GOGUEN, GODEL, SHARP, MIZUMOTO, DUBOIS_PRADE, and MIN. For
# more detail, please have a look at WM. The default value is ZADEH.


################################################################################################
################################################################################################

# • [@]Algorithm - "FRBCS.W": fuzzy rule-based classification systems with weight factor based
# on Ishibuchi’s method to handle classification tasks. See FRBCS.W;

# • Control params:
# 		list(num.labels, type.mf, type.tnorm, type.implication.func, name)

# control <- list(num.labels = 3, type.mf = "TRIANGLE", type.tnorm = "MIN",
# type.implication.func = "ZADEH", name = "sim-0")

cl.fuzzy.w = function(train, test){

	obj = NULL;
	lv.train = levels(train$Class);
	train$Class = as.integer(train$Class);

	rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2);
	
	model = frbs.learn(train, rg.data, method.type = "FRBCS.W")#, control)
	pred = predict(model, test[,-ncol(test)]);

	pred[which(pred == 1)] = 0;
	pred[which(pred == 2)] = 14;	
	pred[which(pred == 3)] = 7;
	
	pred = as.factor(as.character(pred));
	names(pred) = rownames(test);
	
	obj$pred = pred;
	obj$model = model;

	return(obj);
}

################################################################################################
################################################################################################

# • [@]Algorithm - "FRBCS.CHI": fuzzy rule-based classification systems based on Chi’s method
# to handle classification tasks. See FRBCS.CHI;

# • Control params:
# 		list(num.labels, type.mf, type.tnorm, type.implication.func, name)

# control <- list(num.labels = 7, type.mf = "TRIANGLE", type.tnorm = "MIN",
# type.implication.func = "ZADEH", name = "sim-0")

# Chi-classifier
cl.fuzzy.chi = function(train, test){

	obj = NULL;
	# train$Class = as.integer(train$Class);
	train$Class = as.integer(train$Class);
	rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2)

	model = frbs.learn(train, rg.data, method.type = "FRBCS.CHI")#, control)
	pred = predict(model, test[,-ncol(test)]);

	pred[which(pred == 1)] = 0;
	pred[which(pred == 3)] = 7;
	pred[which(pred == 2)] = 14;
	
	pred = as.factor(as.character(pred));
	names(pred) = rownames(test);
	
	obj$pred = pred;
	obj$model = model;

	return(obj);
}


################################################################################################
################################################################################################

# • [@] Algorithm - "GFS.GCCL": Ishibuchi’s method based on genetic cooperative-competitive
# learning to handle classification tasks. See GFS.GCCL;

# 	• Control params:
# 		list(popu.size, num.class, num.labels, persen_cross, max.gen, persen_mutant, name)

# control <- list(popu.size = 5, num.class = 3, num.labels = 5, persen_cross = 0.9,
# max.gen = 2, persen_mutant = 0.3,
# name="sim-0")

# cl.fuzzy.gccl = function(train, test){

# 	obj = NULL;
# 	train$Class = as.integer(train$Class);
# 	rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2)

# 	model = frbs.learn(train, rg.data, method.type = "GFS.GCCL")#, control)
# 	pred = predict(model, test[,-ncol(test)]);

# 	pred[which(pred == 1)] = 0;
# 	pred[which(pred == 2)] = 7;
# 	pred[which(pred == 3)] = 14;
	
# 	obj$pred = as.factor(pred);
# 	obj$model = model;

# 	return(obj);
# }

################################################################################################
################################################################################################

# ******* NOT WORKING

#	[@] Algorithm -  "SLAVE": structural learning algorithm on vague environment to handle
# classification tasks. See SLAVE;
# 	• SLAVE:
# 		list(num.class, num.labels, persen_cross, max.iter, max.gen, persen_mutant, k.lower, k.upper, epsilon, name)

# cl.fuzzy.slave = function(train, test){

# 	obj = NULL;
# 	train$Class = as.integer(train$Class);
# 	rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2)

# 	control <- list(num.class = 3, num.labels = 5,
# 		persen_cross = 0.9, max.iter = 5, max.gen = 3, persen_mutant = 0.3,
# 		k.lower = 0.25, k.upper = 0.75, epsilon = 0.1, name="sim-0")

# 	model = frbs.learn(train, range.data.input, method.type = "SLAVE", control)
# 	pred = predict(model, test[,-ncol(test)]);

# 	pred[which(pred == 1)] = 0;
# 	pred[which(pred == 2)] = 7;
# 	pred[which(pred == 3)] = 14;
	
# 	obj$pred = as.factor(pred);
# 	obj$model = model;

# 	return(obj);
# }

################################################################################################
################################################################################################

# • "FH.GBML": Ishibuchi’s method based on hybridization of genetic cooperative-competitive
# learning and Pittsburgh to handle classification tasks. See FH.GBML;

# 	• FH.GBML:R
# 		list(popu.size, max.num.rule, num.class, persen_cross, max.gen, persen_mutant, p.dcare, p.gccl, name)

# cl.fuzzy.gbml = function(train, test){

# 	obj = NULL;
# 	train$Class = as.integer(train$Class);
# 	rg.data = matrix(apply(train[, -ncol(train)], 2, range), nrow = 2)

# 	control <- list(popu.size = 5, max.num.rule = 5, num.class = 3,
# 		persen_cross = 0.9, max.gen = 2, persen_mutant = 0.3, p.dcare = 0.5,
# 		p.gccl = 1, name="sim-0")

# 	model = frbs.learn(train, rg.data, method.type = "FH.GBML", control)
# 	pred = predict(model, test[,-ncol(test)]);

# 	pred[which(pred == 1)] = 0;
# 	pred[which(pred == 2)] = 7;
# 	pred[which(pred == 3)] = 14;
	
# 	obj$pred = as.factor(pred);
# 	obj$model = model;

# 	return(obj);
# }

################################################################################################
################################################################################################
