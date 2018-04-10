#' Permutation Test
#'
#'Provides nonparametric statistical significance via sample randomization.
#'
#'@param x,y numeric vectors of data values
#'@param func specify \code{mlf} function: (\code{distcorr} or \code{mic}).
#'@param reps (optional) number of resamples. Defaults to 500.
#'@examples # Sample data
#'@examples a <- rnorm(25, 80, 35)
#'@examples b <- rnorm(25, 100, 50)
#'
#'@examples mlf::mic(a, b)
#'@examples mlf::perm(a, b, mic)
#'@export

perm<-function(x,y,func,reps){

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

  estimates <- c()
  observed <- func(x,y)
  N <- length(x)
  pb<-utils::txtProgressBar(min =0,max=reps,style = 3)
  for(i in 1:reps){
    utils::setTxtProgressBar(pb,i)
    y_i <- sample(y, length(y), replace = T)
    estimates <- append(estimates, func(x, y_i))
  }
  base::close(pb)
  p_value <- mean(estimates >= observed)
  return(p_value)
}
