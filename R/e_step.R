# calcul of probabilities

# Conditionnal probability of  data####
#' conditionalProbCont
#' Takes continous observation and categorical data, return a conditional pobablilie based in the
#' completed likehood esperance.
#' @param continousData continous data
#' @param nbClass number of class
#' @param prop proportions
#' @param mu mu vectors
#' @param sigma sigma latrix
#' @param ITER iteration step
#' @return tik matrix
#' @export
conditionalProb <-
  function(continousData,
           categoricalData,
           nbClass,
           prop,
           mu,
           sigma,
           alpha,
           ITER) {
    if (nrow(continousData) == nrow(categoricalData)) {
      n <- nrow(categoricalData)
      tik <- matrix(NA, n, nbClass)
      for (i in 1:n) {
        for (k in 1:nbClass){
          tik[i,k] <- prop[ITER,k]*prod(alpha[k,]^categoricalData[i,])*dmvnorm(continousData[i,],
                                                                             mean = mu[ITER,k,],
                                                                             sigma = sigma[ITER,k,,])
        }
      }
      tik <- tik/rowSums(tik)
      return(tik)
    }else{
      stop('continous and categorical data has not the same length')
    }
  }


conditionalProb2 <- function(continousData,
                             categoricalData,
                             nbClass,
                             prop,
                             mu,
                             sigma,
                             alpha
                             )
{
  tik <- sapply(1:nbClass,
              function(c) prod(alpha[c,]^categoricalData)
              *dmvnorm(continousData, mean=mu, sigma=sigma))
  tik <- t(prop*t(tik))/rowSums(t(prop*t(tik)))
  return(tik)
}

# Expectation step function
# ExpectationStep ####
#' ExpectationStep
#'  This function takes both continous and categorical observations and performs
#'  the expectation step of EM-algorithm.
#'  @param continousData continous data
#'  @param categoricalData categorical data
#'  @param nbClass number of class
#'  @param prop proportions
#'  @param mu mu vectors
#'  @param sigma sigma matrixs
#'  @param alpha alpha table
#'  @param ITER iteration step
#'  @return list of tik matrix of continuous and categorical observation
#'
#'  @export
ExpectationStep <-function(continousData, categoricalData, nbClass, prop, mu, sigma, alpha, ITER){
  continousData <- continousData
  categoricalData <- categoricalData
  nbClass <- nbClass
  prop <- prop
  mu <- mu
  alpha <- alpha
  ITER <- ITER

  tik <- conditionalProb(continousData, categoricalData, nbClass, prop, mu, sigma, alpha, ITER)

  return(tik)
}

