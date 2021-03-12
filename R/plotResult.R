#' plotResult
#' Plot result and orthers parametres such as mu, prop and loglik evolution
#' @param data data 
#' @param res result of clustering
#'
#' @export
#'
plotResult <- function(data, res){
  
  plot(data, lower.panel = NULL, col = res$class)
  # Set plot layout
  layout(matrix(c(1,1,2,2,3,3), 3, 2, byrow = TRUE))

  # Loglik
  plot(res$loglik,type='l',main=paste('max loglik :',max(res$loglik)),cex.main=0.8)
  
  # Prop
  for (k in 1:ncol(res$prop)){
    if (k==1) plot(res$prop[,k],ylim=c(0,1),type='l',main="evolution des prop")
    else lines(res$prop[,k])
  }
  
  # mu
  for (j in 1:dim(res$mu)[3]){
    for (k in 1:dim(res$mu)[2]){
      if (j*k==1) plot(res$mu[,k,j],ylim=c(min(res$mu),max(res$mu)),type='l',main="evolution des mu")
      else lines(res$mu[,k,j])
    }
  }
  
}

