#' Entropy
#'
#'Estimates uncertainty in univariate probability distribution.
#'
#'@param x numeric or discrete data vector
#'@param bins specify number of bins if numeric or integer data class.
#'@examples # Sample numeric vector
#'@examples a <- rnorm(25, 80, 35)
#'@examples mlf::entropy(a, bins = 2)
#'
#'@examples # Sample discrete vector
#'@examples b <- as.factor(c(1,1,1,2))
#'@examples mlf::entropy(b)
#'@export

entropy<-function(x, bins){

  if(base::is.data.frame(x)){
    x <- base::as.matrix(x)
    x <- base::as.numeric(x)
  }

  ind<-base::ifelse(base::is.character(x) == TRUE || base::is.factor(x) == TRUE,1,0)

  if(base::missing(bins)){
    bins<-"bins"
    ind2<-1
  } else {
    ind2<-0
  }

  if(ind == 0 && ind2 == 1){
    error<-"Please specify number of bins or provide discrete vector."
    stop(error)
  }

  if(ind == 1 && ind2 == 0 && bins > nlevels(base::factor(x))){
    error<-"Sorry, too many bins."
    stop(error)
  }

  if(ind == 0 && ind2 == 0 && bins > base::length(base::unique(x))){
    error<-"Sorry, too many bins."
    stop(error)
  }

  if(ind == 1 && ind2 == 0){
    error<-"Discrete vector detected, please remove bins argument."
    stop(error)
  }

  if(ind == 1 && ind2 == 1){
    prp <- base::prop.table(base::table(x))
    H <- base::sum(prp*base::log(prp,2))
  }

  if(ind == 0 && ind2 == 0){
    x2<-base::cut(x,breaks=bins,labels=1:bins)
    prp <- base::prop.table(base::table(x2))
    H <- base::sum(prp*base::log(prp,2))
  }

  return(-H)
}

