#' Mean Squared Error
#'
#'Estimates mean squared error from model predictions.
#'
#'@param truth test data vector or baseline accuracy to test against.
#'@param estimate predicted vector
#'@examples # Sample data
#'@examples test <- rnorm(25, 80, 35)
#'@examples predicted <- rnorm(25, 80, 50)
#'
#'@examples mlf::get_mse(test, predicted)
#'@export

get_mse <- function(truth, estimate) {

  if(base::is.data.frame(truth)){
    truth <- base::as.matrix(truth)
    truth <- base::as.numeric(truth)
  }
  if(base::is.data.frame(estimate)){
    estimate <- base::as.matrix(estimate)
    estimate <- base::as.numeric(estimate)
  }

  return(base::mean((estimate - truth) ^ 2))

}
