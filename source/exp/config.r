##########################################################################################################
##########################################################################################################
# R Code
# Config File 
# L. P. Garcia, R. G. Mantovani, A. C.P.L.F. Carvalho 2013
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
require("doParallel");
require("caret");
require("matrixStats");
require("frbs");

##########################################################################################################
##########################################################################################################

# list of classifiers
# CLASSIFIERS = c("cl.c45", "cl.nb", "cl.nn", "cl.rf", "cl.svm", "cl.mlp");

# CLASSIFIERS = c("cl.nn.1", "cl.nn.3" ,"cl.nn.5" ,"cl.nn.7" ,"cl.nn.9", "cl.nn.11" ,"cl.nn.13" ,"cl.nn.15");

CLASSIFIERS = c("cl.fuzzy.w", "cl.fuzzy.chi")#, "cl.fuzzy.gccl");


#using TBARS
# TBARS = TRUE;
TBARS=FALSE;

#metodology
METHOD = "CV";
# METHOD = "LOO";

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
