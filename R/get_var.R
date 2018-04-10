#' Variance
#'
#'Estimates squared variance by decomposing model prediction error.
#'
#'@param estimate predicted vector
#'@examples # Sample data
#'@examples test <- rnorm(25, 80, 35)
#'@examples predicted <- rnorm(25, 80, 50)
#'
#'@examples mlf::get_var(predicted)
#'@export



get_var <- function(estimate) {

  if(base::is.data.frame(estimate)){
    estimate <- base::as.matrix(estimate)
    estimate <- base::as.numeric(estimate)
  }
  return(base::mean((estimate - mean(estimate)) ^ 2))

}
