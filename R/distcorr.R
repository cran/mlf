#' Distance Correlation
#'
#'Provides pairwise correlation via distance covariance normalized by distance standard deviation. Allows for non-linear dependencies.
#'
#'@param x,y numeric vectors of data values
#'@references Székely GJ, Rizzo ML, Bakirov NK. Measuring and testing dependence by correlation of distances. Ann Stat. 2007. 35(6):2769-2794.
#'@examples # Sample data
#'@examples a <- rnorm(25, 80, 35)
#'@examples b <- rnorm(25, 100, 50)
#'
#'@examples mlf::distcorr(a, b)
#'@export

distcorr <- function(x,y){

  dcenter <- function(x){
    centered <- x
    for(i in 1:base::dim(x)[1]){
      for(j in 1:base::dim(x)[2]){
        centered[i,j] <- x[i,j] - base::mean(x[i,]) - base::mean(x[,j]) + base::mean(x)
      }
    }
    return(centered)
  }

  distcov <- function(x,y){
    N <- base::length(x)
    distX <- base::as.matrix(stats::dist(x))
    distY <- base::as.matrix(stats::dist(y))
    centeredX <- dcenter(distX)
    centeredY <- dcenter(distY)
    calc <- base::sum(centeredX * centeredY)
    return(base::sqrt(calc/(N^2)))
  }

  distvar <- function(x){
    return(distcov(x,x))
  }

  cov <- distcov(x,y)
  sd <- base::sqrt(distvar(x)*distvar(y))
  return(cov/sd)
}
