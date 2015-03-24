#!/bin/bash

#Executing base level

#	-parameters:
#		LOGFILTER: TRUE or FALSE


#R CMD BATCH --no-save --no-restore '--args LOGFILTER="TRUE"' run.r out.log

#no parameter
R CMD BATCH --no-save --no-restore run.r out.log

