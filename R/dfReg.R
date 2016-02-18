#'This is the main function in the dfReg package,
#'
#'This function takes in a fitted model objects, determines the type of the object, and processes it accordingly
#'
#'@param regression_model The inputed model to be processed
#'@param guess_object_type If type not supported can try to guess from a similar object. Currently knows c("lm", "glm", "merModLmerTest", "lmerMod")
#'
#'@return a data.frame object with predictor names and estimates
#'
#' @examples
#' test_data = data.frame(predictor = 1:10, outcome = rnorm(10))
#' test_model = lm(outcome ~ predictor, data = test_data)
#' dfReg(test_model)
#' @export

dfReg <- function(regression_model, guess_model_type = NULL, ...) {
  object_type = object_type_call(regression_model, ...)
  supported_types = c("lm", "glm", "merModLmerTest", "lmerMod")
  if(!object_type %in% supported_types){
    if(!is.null(guess_model_type)) object_type = guess_model_type
  } else{
    warning("Model type not supported, trying 'lm'. You can suggest other types by specifying 'guess_model_type'")
  }

  regression_model

}
