#' Maximal Information Criterion
#'
#'Information-theoretic approach for detecting non-linear pairwise dependencies. Employs heuristic discretization to achieve highest normalized mutual information.
#'
#'@param x,y numeric or discrete data vectors
#'@references Reshef DN, Reshef YA, Finucane HK, Grossman SR, McVean G, Turnbaugh PJ, Lander ES, Mitzenmacher M, Sabeti PC. Detecting novel associations in large data sets. Science. 2011. 334(6062):1518-1524.
#'@examples # Sample data
#'@examples a <- rnorm(25, 80, 35)
#'@examples b <- rnorm(25, 100, 50)
#'
#'@examples mlf::mic(a, b)
#'@export

mic <- function(x,y){

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

  marginalProduct_<- function(x,y){
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

  mutualInfo_ <- function(x,y){
    joint_ <- jointDist_(x,y)
    marginal_ <- marginalProduct_(x,y)
    Hjm <- - sum(joint_[marginal_ > 0] * log(marginal_[marginal_ > 0],2))
    Hj <- - sum(joint_[joint_ > 0] * log(joint_[joint_ > 0],2))
    return(Hjm - Hj)
  }

  MI_ <- c()
  if(is.data.frame(y)){
    y <- as.matrix(y)
    y <- as.numeric(y)
  }
  if(is.data.frame(x)){
    x <- as.matrix(x)
    x <- as.numeric(x)
  }
  N <- length(x)
  maxBins <- ceiling(N ** 0.6)
  for(i in 2:maxBins) {
    for (j in 2:maxBins){
      if(i * j > maxBins){
        next
      }
      Xbins <- i; Ybins <- j
      binnedX <-cut(x, breaks=Xbins, labels = 1:Xbins)
      binnedY <-cut(y, breaks=Ybins, labels = 1:Ybins)
      MI_estimate <- mutualInfo_(binnedX,binnedY)
      MI_normalized <- MI_estimate / log(min(Xbins,Ybins),2)
      MI_ <- append(MI_, MI_normalized)
    }
  }
  return(round(max(MI_),4))
}
