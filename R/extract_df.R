#'This is the main function in the dfReg package,
#'
#'This function takes in a fitted model objects, determines the type of the object, and processes it accordingly
#'
#'@param regression_model The inputed model to be processed
#'@param guess_object_type If type not supported can try to guess from a similar object. Currently knows c("lm", "glm", "merModLmerTest", "lmerMod")
#'@param include.confint Include the confidence interval estimates. Default FALSE.
#'@return a data.frame object with predictor names and estimates
#'
#' @examples
#' test_data = data.frame(predictor = 1:10, outcome = rnorm(10))
#' test_model = lm(outcome ~ predictor, data = test_data)
#' extract_df(test_model)
#' @export

extract_df <- function(regression_model, include.intercept = T, include.confint = F,
                       guess_model_type = NULL, ...) {
  object_type = get_object_type(regression_model, ...)
  supported_types = c("lm", "glm", "merModLmerTest", "lmerMod")
  if(!object_type %in% supported_types){
    if(!is.null(guess_model_type)) {
      object_type = guess_model_type
      } else{
        warning("Model type not supported, trying 'lm'. You can suggest other types by specifying
                'guess_model_type'")
        object_type = "lm"
      }
  }

  extract_df_switch(regression_model, object_type, include.intercept, include.confint, ...)
}
