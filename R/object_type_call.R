#'This is the main function in the dfReg package,
#'
#'This function takes a model object and return its type
#'
#'@param regression_model The inputted model to be processed
#'
#'@return a string of type
#'

object_type_call = function(regression_model, position = 1) as.character(is(regression_model)[position])
