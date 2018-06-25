#' Mutual Information
#'
#'Estimates Kullback-Leibler divergence of joint distribution and the product of two respective marginal distributions. Roughly speaking, the amount of information one variable provides about another.
#'
#'@param x,y numeric or discrete data vectors
#'@examples # Sample data
#'@examples a <- rnorm(25, 80, 35)
#'@examples b <- rnorm(25, 100, 50)
#'
#'@examples mlf::mi(a, b)
#'@export

mi<- function(x,y){

  if(is.data.frame(y)){
    y <- as.matrix(y)
    y <- as.numeric(y)
  }
  if(is.data.frame(x)){
    x <- as.matrix(x)
    x <- as.numeric(x)
  }

  jointDist_ <- function(x,y){
    N <- length(x)
    u <- unique(append(x,y))
    joint_ <- c()
    for(i in u){
      for(j in u){
        f <- x[paste(x,y) == paste(i,j)]
        joint_ <- append(joint_, length(f)/N)
      }
    }
    return(joint_)
  }

  marginalProduct_ <- function(x,y){
    N <- length(x)
    u <- unique(append(x,y))
    marginal_ <- c()
    for(i in u){
      for(j in u){
        fX <- length(x[x == i]) / N
        fY <- length(y[y == j]) / N
        marginal_ <- append(marginal_, fX * fY)
      }
    }
    return(marginal_)
  }

  joint_ <- jointDist_(x,y)
  marginal_ <- marginalProduct_(x,y)
  Hjm <- - sum(joint_[marginal_ > 0] * log(marginal_[marginal_ > 0],2))
  Hj <- - sum(joint_[joint_ > 0] * log(joint_[joint_ > 0],2))
  return(abs(Hjm - Hj))
}
