# Function initialize
#' Function initializeModel
#' Initialize model containers objects (arrays, matrix and vector) that
#'  contains proportion of each class, vector of means(mu), matrix of covariance
#'  and probabilities alpha of multinomial model.
#'@param continuousData continuous data result of split data function
#'@param categoricalData categorical data result of plit data function
#'@param nbClass number of class
#'@param ITERMAX maximum iteration
#'
#'@return list object containing proportion vectors, means vectors, sigmas matrix
#' alpha matrix
#'@export
initializeModel <- function(continuousData, categoricalData, nbClass, ITERMAX) {
  require(bayess)

  if(is.factor(categoricalData)){
    print(paste("Error categoricalData is a factor vector"))
    return(NULL)
  }
  # data information
  n <- nrow(continuousData);  p <- ncol(continuousData)

  #[Object creation] of gaussian model
  prop <- matrix(NA, ITERMAX+1, nbClass)
  mu <- array(NA, dim = c(ITERMAX+1, nbClass, p))
  sigma <- array(NA, dim = c(ITERMAX+1, nbClass, p, p))

  # Creation of object that contain alpha an proportion in
  # multinomial model
  nbCatLevel <- NCOL(categoricalData)
  #alpha <- array(NA, data = c(ITERMAX, nbClass, nbCatLevel))
  alpha <- matrix(NA, nbClass, nbCatLevel)

  ## Object initialisation
  # gaussian model
  prop[1,] <- rdirichlet(1, par=rep(1,nbClass))
  mu[1,,] <- as.matrix(continuousData[sample(1:n,nbClass),])
  for (k in 1:nbClass){
    sigma[1,k,,] <- cov(matrix(rnorm(p*p), ncol = p))
  }

  # Multinomial
  alpha[,] <- rdirichlet(nbClass, par=rep(1,nbCatLevel))

  #loglik
  loglik <- array(NA, ITERMAX+1)

  return(list(prop = prop,
              mu = mu,
              sigma = sigma,
              alpha = alpha,
              loglik = loglik
              ))
}
