#' Bias-Variance Trade-Off
#'
#'Provides estimated error decomposition from model predictions (mse, bias, variance).
#'
#'@param truth test data vector or baseline accuractruth to test against.
#'@param estimate predicted vector
#'@examples # Sample data
#'@examples test <- rnorm(25, 80, 35)
#'@examples predicted <- rnorm(25, 80, 50)
#'
#'@examples mlf::bvto(test, predicted)
#'@export

bvto<-function(truth, estimate){

  if(is.data.frame(truth)){
    truth <- as.matrix(truth)
    truth <- as.numeric(truth)
  }
  if(is.data.frame(estimate)){
    estimate <- as.matrix(estimate)
    estimate <- as.numeric(estimate)
  }

  bias_<-((base::mean(estimate) - base::mean(truth))^2)
  var_<-base::mean((estimate - base::mean(estimate))^2)
  mse_<-base::mean((estimate - truth)^2)

  output<-c(mse_,var_,bias_)
  names(output)<-c("MSE","Var","Bias")
  return(output)

}
