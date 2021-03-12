
# EM algorithm function
#' runEMAlgoritm
#' This function perform the whole EM algorithm
#' @param data data
#' @param nbClass number of class
#' @param ITERMAX number of iteration
#'
#' @return list
#'
#' @export
#' @exemple
#' > data <-iris
#' > runEMAlgorithm(data,3,10)
#' > res <- runEMAlgoritm(data,3,10)
#' > plot(res$loglik,type='l',main=paste('max loglik :',max(res$loglik)),cex.main=0.8)
#' > plot(data,col=res$class)
#' > table(data[,5],res$class)
#'             1  2  3
#' setosa      0  0 50
#' versicolor  2 48  0
#'
runEMAlgorithm <- function(data, nbClass, ITERMAX, mode = "random"){
  # Split of data
  # In this step data are split in 2 dataset continous and categorical
  dataset <- splitDatasetIntoCatAndConti(data)

  categoricalData <- binarizeCateMatrix(dataset$categoricalMat)
  continousData <- dataset$continuousMat
  # Initialization
  # Creation of objects that will contain parameters of models
    ## initialization of random model
    init <- initializeModel(continuousData=continousData,
                            categoricalData=categoricalData,
                            nbClass=nbClass,
                            ITERMAX = ITERMAX,
                            mode = mode)
    prop <- init$prop
    mu <- init$mu
    sigma <- init$sigma
    alpha <- init$alpha
    loglik <- init$loglik


  # Beggin of loops
  ITER <- 1
  LIMITE <- 1

  loglik[1] <- loglikCalul(continousData,
                               categoricalData,
                               nbClass,
                               prop,
                               mu,
                               sigma,
                               alpha,
                               1)



  while(ITER <= ITERMAX){
    # Perform Expectaion step
    # tik <- ExpectationStep(continousData,
    #                        categoricalData,
    #                        nbClass,
    #                        prop,
    #                        mu,
    #                        sigma,
    #                        alpha,
    #                        ITER)

    tik <- conditionalProb(continousData,
                           categoricalData,
                           nbClass,
                           prop,
                           mu,
                           sigma,
                           alpha,
                           ITER)



    # Perform Maximization step
    # estimation of paramater prop, mu, sigma and alpha
    # mStepRes <- MaximisationStep(tik,
    #                  categoricalData,
    #                  continousData,
    #                  nbClass,
    #                  prop,
    #                  mu,
    #                  sigma,
    #                  alpha,
    #                  ITER)
    # prop[ITER+1,] <- mStepRes$prop
    # mu[ITER+1,,] <- mStepRes$mu
    # sigma[ITER+1,,,] <- mStepRes$sigma
    # alpha <- mStepRes$alpha

    nk <- round(colSums(tik))

    prop[ITER+1,] <- nk/sum(nk)
    mu[ITER + 1, , ] <-
      t(sapply(1:nbClass, function(c)
        (1 / nk[c]) * colSums(tik[, c] * continousData)))

    for(i in 1:ncol(continousData))
      for (j in 1:ncol(continousData))
        for (k in 1:nbClass)
          sigma[ITER + 1, k, i, j] <-
      1 / nk[k] * sum(tik[, k] * (continousData[, i] - mu[ITER + 1, k, i]) * tik[, k] *
                        (continousData[, j] - mu[ITER + 1, k, j]))

    for(k in 1:nbClass)
      for (j in 1:ncol(categoricalData))
        alpha[k, j] <- (1 / nk[k]) * sum(tik[, k] * categoricalData)



    # Calcul of loglik of the next interation
    loglik[ITER+1] <- loglikCalul(continousData,
                                      categoricalData,
                                      nbClass,
                                      prop,
                                      mu,
                                      sigma,
                                      alpha,
                                      ITER+1)


    ITER <- ITER +1
    LIMITE <- loglik[ITER+1]-loglik[ITER]
    # if(LIMITE <=1e-6){
    #   return(
    #     # TODO: best way to raise warning
    #     c(
    #       print(paste("WARNING: we stoped the algorithm because limit is under or equal to: )", 1e-6))
    #     ),
    #     list())
    #   break
    # }
  }
  z=max.col(tik)

  return(list(prop=prop,
              mu = mu,
              sigma= sigma,
              alpha = alpha,
              loglik = loglik,
              class = z))
}
