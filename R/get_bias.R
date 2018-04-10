#' Bias
#'
#'Estimates squared bias by decomposing model prediction error.
#'
#'@param truth test data vector or baseline accuracy to test against.
#'@param estimate predicted vector
#'@examples # Sample data
#'@examples test <- rnorm(25, 80, 35)
#'@examples predicted <- rnorm(25, 80, 50)
#'
#'@examples mlf::get_bias(test, predicted)
#'@export

get_bias <- function(truth, estimate) {

  if(base::is.data.frame(truth)){
    truth <- base::as.matrix(truth)
    truth <- base::as.numeric(truth)
  }
  if(base::is.data.frame(estimate)){
    estimate <- base::as.matrix(estimate)
    estimate <- base::as.numeric(estimate)
  }

  return((base::mean(estimate) - base::mean(truth))^2)

}
