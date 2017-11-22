# Tuure Parviainen
# Wed Nov 22 14:08:55 2017
# title: Chapter 4. Clustering and classification
rm(list=ls())

.libPaths( c( .libPaths(), "C:/Program Files/R/R-3.4.1/library")

sourceDir <- getSrcDirectory(function(dummy) {dummy})
setwd(sourceDir)
packages_to_load <- c("dplyr", "MASS", "tidyverse", "corrplot")
lapply(packages_to_load, require, character.only = TRUE)

# load the data
data("Boston")

# explore the dataset
str(Boston)
summary(Boston)

# plot matrix of the variables
pairs(Boston)
script.dir <- dirname(sys.frame(1)$ofile)

