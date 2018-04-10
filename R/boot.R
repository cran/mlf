#' Bootstrap Confidence Intervals via Resampling
#'
#'Provides nonparametric confidence intervals via percentile-based resampling for given \code{mlf} function.
#'
#'@param x,y numeric vectors of data values
#'@param func specify \code{mlf} function
#'@param reps (optional) number of resamples. Defaults to 500
#'@param conf.int (optional) numeric value indicating level of confidence. Defaults to \code{0.90}.
#'@examples # Sample data
#'@examples a <- rnorm(25, 80, 35)
#'@examples b <- rnorm(25, 100, 50)
#'
#'@examples mlf::mic(a, b)
#'@examples mlf::boot(a, b, mic)
#'@export

boot<- function(x,y,func,reps,conf.int){

  if(missing(conf.int)){
    conf.int<-.9
  }

  if(missing(reps)){
    reps<-500
  }

  if(is.data.frame(y)){
    y <- as.matrix(y)
    y <- as.numeric(y)
  }
  if(is.data.frame(x)){
    x <- as.matrix(x)
    x <- as.numeric(x)
  }

  est <- c()
  orig <- base::data.frame(x,y)
  N <- base::dim(orig)[1]
  pb<-utils::txtProgressBar(min =0,max=reps,style = 3)
  for(i in 1:reps){
    utils::setTxtProgressBar(pb,i)
    S <- orig[base::sample(1:N, N, replace = TRUE),]
    est <- base::append(est, func(S$x, S$y))
  }
  base::close(pb)
  LL <- (1-conf.int)/2
  UL <- 1 - LL
  interval <- stats::quantile(est, c(UL, LL))
  return(round(2*(func(x,y)) - base::as.numeric(interval[1:2]),3))
}
