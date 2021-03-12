# Description organize_dataset.R file####
# Date : 2020-11-15
# This file regroup functions usefull to organize input data for the EM algorithm
# It is composed of 3 functions :
# - convertCateVectToBinMatrix
# - binarizeCateMatrix
# - splitDatasetIntoCatAndConti
#All functions could be tested with unit tests using testthat package
#For more details see README

# packages ####
#library(bayess)
#library(mvtnorm)
#library(mclust)


# function convertCateVectToBinMatrix ####
#' function convertCateVectToBinMatrix #
#' Takes a vector representing one Categorical variable and transform it in a Matrix with number of columns
#' corresponding to number of possible values the categorical variable can take.
#' All the column of the matrix are fill with 0 or 1 depending on if row (observation) take this modality (1) or not (0)
#' @param cateVect Vector with categorical variable
#' @return binMatrix a Matrix with each column corresponding to one of categorical variable's possible values
#' @example A vector encoding categorical variable eye color of 5 persons.
#' We have three modalities for this categorical variable :
#' B = blue clored eye
#' G = green colored eyed
#' Br = Brown colored eyed
#' exCateVect = c(B,G,B,G,Br)
#' convertCateVectToBinMatrix(exCateVect) return a Matrix of 5 rows and 3 columns equals to this :
#' [,1]:(1,0,1,0,0)
#' [,2]:(0,1,0,1,0)
#' [,3]:(0,0,0,1)
#' @export
convertCateVectToBinMatrix <- function (cateVect) {
  uniqueValVect <- unique(cateVect)
  binMatrix <- matrix(0, nrow=length(cateVect), ncol=length(uniqueValVect))
  for (i in 1:length(cateVect)) {
    binMatrix[i, which(uniqueValVect == cateVect[i])] <- 1
  }
  return(binMatrix)
}
# function binarizeCateMatrix #####
#' function binarizeCateMatrix
#' Convert a matrix of categorical variables into matrix of ones and zeros representing
#' modalities taken by observations (rows).
#' @param cateMatrix Matrix of categorical variables
#' (only matrix and vector ! not list)
#' @return Matrix filled with only ones and zeros.
#' @example If an observation take a specific modality
#' for example blue eye for the eye color categorical variable. Observation will take 1
#' for the column blue eye. See convertCateVectToBinMatrix to understand deeper.
#' @export
binarizeCateMatrix = function(cateMatrix) {
  binCateMatrix = Reduce(cbind, lapply(cateMatrix, convertCateVectToBinMatrix))
  return(binCateMatrix)
}
# function splitDatasetIntoCatAndConti ####
#' function splitDatasetIntoCatAndConti
#' @param data list or matrix of continuous and categorical data
#' @return two matrix, one with continuous data and another with categorical varaibles grouped.
#' @example With iris dataset, splitDatasetIntoCatAndConti(iris) will retrun a matrix continous
#' composed of columns [,1:4] and matrix categorical with only columns [,5].
#' @export
splitDatasetIntoCatAndConti <- function(data) {
  categoricalMat <- data[lapply(data, class) == "factor"]
  categoricalMat <- cbind(categoricalMat, data[lapply(data, class) == "character"])
  continuousMat <- data[lapply(data, class) == "numeric"]
  return(list(continuousMat = continuousMat, categoricalMat = categoricalMat))
}
