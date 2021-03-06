#' Kullback-Leibler Divergence
#'
#'Provides estimated difference between individual entropy and cross-entropy of two probability distributions.
#'
#'@param x,y numeric or discrete data vectors
#'@param bins specify number of bins
#'@examples # Sample numeric vector
#'@examples a <- rnorm(25, 80, 35)
#'@examples b <- rnorm(25, 90, 35)
#'@examples mlf::kld(a, b, bins = 2)
#'
#'@examples # Sample discrete vector
#'@examples a <- as.factor(c(1,1,2,2))
#'@examples b <- as.factor(c(1,1,1,2))
#'@examples mlf::kld(a, b)
#'@export

kld<- function(x,y,bins){

  if(is.data.frame(y)){
    y <- as.matrix(y)
    y <- as.numeric(y)
  }
  if(is.data.frame(x)){
    x <- as.matrix(x)
    x <- as.numeric(x)
  }

  ind<-ifelse(is.character(x) == TRUE || is.factor(x) == TRUE || is.character(y) == TRUE || is.factor(y) == TRUE,1,0)

  if(missing(bins)){
    bins<-"bins"
    ind2<-1
  } else {
    ind2<-0
  }


  if(ind == 0 && ind2 == 1){
    error<-"Please specify number of bins or provide discrete vector."
    stop(error)
  }

  if(ind == 1 && ind2 == 0 && bins > nlevels(factor(x))){
    error<-"Sorry, too many bins."
    stop(error)
  }

  if(ind == 0 && ind2 == 0 && bins > length(unique(x))){
    error<-"Sorry, too many bins."
    stop(error)
  }

  if(ind == 1 && ind2 == 0){
    error<-"Discrete vector detected, please remove bins argument."
    stop(error)
  }


  if(ind == 1 && ind2 == 1){
    ent <- function(x){
      pr <- base::prop.table(base::table(x))
      H <- base::sum(pr * base::log(pr,2))
      return(-H)
    }

    relent <- function(x,y){
      prX <- base::prop.table(base::table(x))
      prY <- base::prop.table(base::table(y))
      H <- base::sum(prX * base::log(prY,2))
      return(-H)
    }

    kl<-relent(x,y)-ent(x)
  }

  if(ind == 0 && ind2 == 0){



    ent <- function(x){
      x2<-base::cut(x,breaks=bins,labels=1:bins)
      pr <- base::prop.table(base::table(x2))
      H <- base::sum(pr * base::log(pr,2))
      return(-H)
    }

    relent <- function(x,y){
      x2<-base::cut(x,breaks=bins,labels=1:bins)
      y2<-base::cut(y,breaks=bins,labels=1:bins)
      prX <- base::prop.table(base::table(x2))
      prY <- base::prop.table(base::table(y2))
      H <- base::sum(prX * base::log(prY,2))
      return(-H)
    }

    kl<-relent(x,y)-ent(x)
  }

  return(kl)
}

