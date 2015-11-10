##########################################################################################################
##########################################################################################################
# R Code
# Config File 

# Packages and Global Variables
##########################################################################################################
##########################################################################################################

# Packages
require("CORElearn");
require("e1071");
require("FNN");
require("foreign");
require("infotheo");
require("kknn");
require("randomForest");
require("R.utils");
require("matrixStats");
require("frbs");
require("PMCMR");			# Statistic Tests


##########################################################################################################
##########################################################################################################

# list of classifiers
ALL = c("cl.c45", "cl.nb", "cl.nn", "cl.rf", "cl.svm", "cl.mlp", "cl.fuzzy.w", "cl.fuzzy.chi")
KNNS = paste("cl.nn.", seq(1,31, by=2), sep="")
MLPS = paste("cl.mlp.", rep(2:50), sep="")
CLASSIFIERS = c(ALL, MLPS, KNNS)


TBARS = c(TRUE, FALSE);

#metodology
METHOD = "CV";

# dataset folder
DATABASE = "/database/";

# current directory
DIR = getwd();

# epochs to run
EPOCHS = 30;

# number of folds
FOLDS = 10; 

##########################################################################################################
##########################################################################################################
