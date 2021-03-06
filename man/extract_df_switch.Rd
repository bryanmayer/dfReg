% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/extract_df_regressiontype.R
\name{extract_df_switch}
\alias{extract_df_switch}
\title{This function directs to the correct processing function}
\usage{
extract_df_switch(regression_model, object_type, include.intercept,
  include.confint, ...)
}
\arguments{
\item{regression_model}{The inputed model to be processed}

\item{object_type}{the string argument to be evaluated}
}
\value{
a data.frame object with predictor names and estimates
}
\description{
This function directs to the correct processing function
}

