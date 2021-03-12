# Description calculate_loglik.R file####
# Date : 2020-12-13
# This file regroup functions usefull to calculate loglik
# All functions could be tested with unit tests using testthat package
# For more details see README

# function MaximisationStep ####
#'function MaximisationStep
#' @param xCate Matrix of categorical variables for input datas
#' @param xConti Matrix of continuous variables for input datas
#' @param KnbClasse number of classes K
#' @param prop Marix of proportions for each classes
#' @param mu Matrix of mus for continuous variables
#' @param sigma Matrix of Variance Covariance matrix for continuous variables
#' @param alpha Probabilities for each categorical variables 
#' @return log likelihood
#' @export
MaximisationStep = function(tk, xBinCate, xConti, KnbClasse, prop, mu, sigma, alpha, ITER){
  # M step ####
  n = nrow(xBinCate)
  # calcul nk
  nk = rep(NA, KnbClasse)
  for(k in 1:KnbClasse){
    nk[k] = sum(tk[,k])
  }
  #actualisation de prop
  # formule = pk() = nk/n
  p <- array(NA, KnbClasse)
  for(k in 1:KnbClasse){
    p[k]<- nk[k]/n
  }
  #actualisation de mus
  m <- array(NA, dim = c(KnbClasse, ncol(xConti)))
  for(k in 1:KnbClasse){
    m[k,] = (1 / nk) * colSums( tk[,k] * xConti )
  }
  #actualisation de sigma
  s <- array(NA, dim=c(KnbClasse, ncol(xConti), ncol(xConti)))
  for (k in 1:KnbClasse) {
    sig <- colSums(sqrt(tk[,k])*(xConti-m[k,]))%*%t(colSums(sqrt(tk[,k])*(xConti-m[k,])))/nk
    sig <- (sig+t(sig))/2
    s[k,,] = sig 
      }
  #actualisation de alpha
  # formule : alpha jhk = 1/nk * somme sur n des tik(xi) * x ijh
  a <- matrix(NA, KnbClasse, ncol(xBinCate))
  for(k in 1:KnbClasse){
    for(j in 1:ncol(xBinCate)){
      sumTkxi = sum( tk[,k] * xBinCate )
      a[k,j] = ( 1 / nk[k] ) * sumTkxi
    }
  }

  return(list(prop = p,
              mu = m,
              sigma = s,
              alpha = a))
}


