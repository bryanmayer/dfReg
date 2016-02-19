#'This function directs to the correct processing function
#'
#'
#'@param regression_model The inputed model to be processed
#'@param object_type the string argument to be evaluated
#'
#'@return a data.frame object with predictor names and estimates


extract_df_switch = function(regression_model, object_type, ...){
  switch(object_type,
         lm = extract_df_lm(regression_model, ...),
         glm = extract_df_lm(regression_model, ...),
         merModLmerTest = extract_df_lmerTest(regression_model),
         lmerMod = extract_df_lmer(regression_model),
         stop("object_type not found. Did you specify a supported type with guess_object_type?")
  )
  }

#'This function processed lm or glm regression
#'
#'
#'@param regression_model The inputed model to be processed
#'@param include_intercept Include the intercept estimate. Default TRUE.
#'
#'@return a data.frame object with predictor names and estimates
#'
#'
#' @examples
#' test_data = data.frame(predictor = 1:10, outcome = rnorm(10))
#' test_model = lm(outcome ~ predictor, data = test_data)
#' extract_df_lm(test_model)
#' extract_df_lm(test_model, include.intercept = F)
#'
#'@export

extract_df_lm = function(regression_model, include_intercept = T){
  coef_output = coef(summary(regression_model))
  coef_names = row.names(coef_output)
  output = as.data.frame(coef_output)
  output$predictor = coef_names
  names(output) = stringr::str_replace_all(tolower(names(output)), pattern= "[^[:alnum:]]", repl="")

  if(!include_intercept) output = subset(output, predictor != "(Intercept)")

  row.names(output) <- NULL

  select(output, predictor, everything()) %>% rename(p_value = prt)
}

#'This function processed lm or glm regression
#'
#'
#'@param regression_model The inputed model to be processed
#'@param include_intercept Include the intercept estimate. Default TRUE.
#'
#'@return a data.frame object with predictor names and estimates
#'
#'
#' @examples
#' test_data = data.frame(predictor = 1:10, outcome = rnorm(10))
#' test_model = lm(outcome ~ predictor, data = test_data)
#' extract_df_lm(test_model)
#' extract_df_lm(test_model, include.intercept = F)
#'
#'@export

extract_df_lmerTest = function(regression_model, include_intercept = T){

}

