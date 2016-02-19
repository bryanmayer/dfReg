#'This function directs to the correct processing function
#'
#'
#'@param regression_model The inputed model to be processed
#'@param object_type the string argument to be evaluated
#'
#'@return a data.frame object with predictor names and estimates


extract_df_switch = function(regression_model, object_type,
                             include.intercept, include.confint, ...){
  switch(object_type,
         lm = extract_df_lm(regression_model, include.intercept, include.confint, ...),
         glm = extract_df_lm(regression_model, include.intercept, include.confint, ...),
         merModLmerTest = extract_df_lmerTest(regression_model, include.intercept, include.confint),
         lmerMod = extract_df_lmer(regression_model, include.intercept, include.confint),
         stop("object_type not found. Did you specify a supported type with guess_object_type?")
  )
  }

#'This function processed lm or glm regression
#'
#'
#'@param lm_model The inputed lm or glm model to be processed
#'@param include.intercept Include the intercept estimate. Default TRUE.
#'@param include.confint Include the confidence interval estimates. Default FALSE.
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

extract_df_lm = function(lm_model, include.intercept = T, include.confint = F){
  coef_output = coef(summary(lm_model))
  coef_names = row.names(coef_output)
  output = as.data.frame(coef_output)
  output$predictor = coef_names
  names(output) = stringr::str_replace_all(tolower(names(output)), pattern= "[^[:alnum:]]", repl="")

  if(include.confint){
    confints = suppressMessages(confint(lm_model)) %>% as.data.frame()
    names(confints) = c("lower_95ci", "upper_95ci")
    confints$predictor = row.names(confints)
    output = left_join(output, confints, by = "predictor")
  }

  if(!include.intercept) output = subset(output, predictor != "(Intercept)")

  row.names(output) <- NULL

  select(output, predictor, everything()) %>% rename(p_value = prt)
}

#'This function processed lmer model when lmerTest package is loaded
#'
#'
#'@param lmerTest_model A model generated using lmerTest
#'@param include.intercept Include the intercept estimate. Default TRUE.
#'@param include.confint Include the confidence interval estimates. Default FALSE.
#'
#'@return a data.frame object with predictor names and estimates
#'
#'
#' @examples
#' library(lmerTest)
#' test_data = data.frame(
#'   predictor1 = 1:100,
#'   predictor2 = rep(LETTERS[1:2], each = 50),
#'   outcome = rnorm(100),
#'   group = sample(LETTERS[1:3], 100, replace = T)
#'   )
#' test_model = lmer(outcome ~ predictor1 + predictor2 + (1|group), data = test_data)
#' extract_df_lmerTest(test_model)
#' extract_df_lmerTest(test_model, include.intercept = F)
#'
#'@export

extract_df_lmerTest = function(lmerTest_model, include.intercept = T, include.confint = F){
  coef_output = coef(lmerTest::summary(lmerTest_model)) #this kept calling the wrong summary function
  coef_names = row.names(coef_output)
  output = as.data.frame(coef_output)
  output$predictor = coef_names
  names(output) = stringr::str_replace_all(tolower(names(output)), pattern= "[^[:alnum:]]", repl="")

  if(include.confint){
    confints = suppressMessages(confint(lmerTest_model)) %>% as.data.frame()
    names(confints) = c("lower_95ci", "upper_95ci")
    confints$predictor = row.names(confints)
    output = left_join(output, confints, by = "predictor")
  }

  if(!include.intercept) output = subset(output, predictor != "(Intercept)")

  row.names(output) <- NULL

  select(output, predictor, everything()) %>% rename(p_value = prt)
}
