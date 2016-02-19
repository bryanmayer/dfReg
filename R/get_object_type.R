#'This function takes a model object and return its type
#'
#'@param regression_model The inputted model to be processed
#'
#'@return a string of type
#'

get_object_type = function(regression_model, position = 1) as.character(is(regression_model)[position])
