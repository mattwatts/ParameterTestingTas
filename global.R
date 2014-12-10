# Author: Matt Watts
# Date: 10 Dec 2014
# Purpose: ParameterTestingTas web app global.R

library(shiny)
library(PBSmapping)
library(maptools)
library(sp)

sMarxanDir <- getwd()

# find how many runs from input.dat
inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
iParam <- which(regexpr("NUMREPS",inputdat)==1)
iNUMREPS <<- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])

itestinput <<- 0
ioutputmap <<- 0
ioutputtable <<- 0

