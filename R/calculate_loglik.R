# Description calculate_loglik.R file####
# Date : 2020-12-13
# This file regroup functions usefull to calculate loglik
# All functions could be tested with unit tests using testthat package
# For more details see README

# function CalculateLoglik ####
#'function CalculateLoglik
#' @param xCate Matrix of categorical variables for input datas
#' @param xConti Matrix of continuous variables for input datas
#' @param KnbClasse number of classes K
#' @param prop Marix of proportions for each classes
#' @param mu Matrix of mus for continuous variables
#' @param sigma Matrix of Variance Covariance matrix for continuous variables
#' @param alpha Probabilities for each categorical variables
#' @return log likelihood
#' @export
CalculateLoglik = function(xConti, xCate, KnbClasse, prop, mu, sigma, alpha, ITER){
  n = nrow(xCate)
  SommeSurIndividus = 0
  for(i in 1:n){
    SommeSurClasses = 0
    for(k in 1:KnbClasse){

      ProduitSurCateg = 1

      for(j in 1:length(alpha[1,])){
        ProduitSurCateg = ProduitSurCateg * alpha[k,j]^xCate[i,j]
      }
      # => une fois qu'on a les produits sur les catégories
      # on multiplie itérativement par fk(x)
      # mu (ajouter une dim si on veut la stocker en fct du nombre d'itération)
      # sigma (pareil que mu)
      #print(paste('dmvnorm'))
      currX = xConti[i,]
      currMu = mu[ITER,k,]
      currSigma = sigma[ITER,k,,]
      fkConti= dmvnorm(currX,mean = currMu,sigma = currSigma)

      PrdCateConti = ProduitSurCateg * fkConti

      SommeSurClasses = SommeSurClasses + (prop[ITER,k] * PrdCateConti)
    }
    SommeSurIndividus = SommeSurIndividus + log(SommeSurClasses)
  }
  return(SommeSurIndividus)
}

# Loglik of  data####
#' Loglik
#' Takes continous observation and categorical data, return a conditional pobablilie based in the
#' completed likehood esperance.
#' @param continousData continous data
#' @param nbClass number of class
#' @param prop proportions
#' @param mu vectors of mu
#' @param sigma sigma matrixs
#' @param ITER iteration step
#' @return tik matrix
#' @export
loglikCalul <- function(continousData,
                        categoricalData,
                        nbClass,
                        prop,
                        mu,
                        sigma,
                        alpha,
                        ITER){
  # Test si n est correct
  if(nrow(continousData)== nrow(categoricalData)){
    n <- nrow(continousData)
    tmp <- 0
    for(i in 1:n){
      for(k in 1:nbClass){
        tmp <- log(prop[ITER,k]*prod(alpha[k,]^categoricalData[i,]))
        +log(prop[ITER,k]*dmvnorm(continousData[i,],
                                  mean=mu[ITER,k,],
                                  sigma=sigma[ITER,k,,]))
      }
    }
    return(tmp)
  }else{
    stop('continous and categorical data has not the same length')
  }
}


calcLoglik <- function(continousData,
                         categoricalData,
                         nbClass,
                         prop,
                         mu,
                         sigma,
                         alpha){
  loglik <- 0
  for(k in 1:nbClass)
    loglik1 <- loglik + sum(log(prop[k,]*(alpa[c,]^continousData)))
    loglik <- loglik1 + sum(log(prop[k,])+log(apply(data[, 1:4], 1, function(xi)
    dmvnorm(xi,
            mean = mu,
            sigma = sigma )
    )))
  return(loglik)
}

# tests ####
# split ...
# source(file = "organize_dataset.R")
# source(file = "initialisation.R")
# res = splitDatasetIntoCatAndConti(iris)
# cat = res$categoricalMat
# resCate = binarizeCateMatrix(cat)
# # initialise
# init = initializeModel(res$continuousMat, resCate, 3, 10)
#
# # test loglik
# loglik = CalculateLoglik(resCate, res$continuousMat, 3, init$prop, init$mu, init$sigma, init$alpha, ITER=1)
# print(loglik)

