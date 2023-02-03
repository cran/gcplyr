% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{smooth_data}
\alias{smooth_data}
\title{Smooth data}
\usage{
smooth_data(
  ...,
  x = NULL,
  y = NULL,
  sm_method,
  subset_by = NULL,
  return_fitobject = FALSE
)
}
\arguments{
\item{...}{Arguments passed to \code{stats::loess}, \code{mgcv::gam},
\code{moving_average}, or \code{moving_median}. Typically
includes tuning parameter(s), which in some cases are required.
See Details for more information.}

\item{x}{An (optional) vector of predictor values to smooth along (e.g. time)}

\item{y}{A vector of response values to be smoothed (e.g. density). If NULL,
\code{formula} and \code{data} *must* be provided via \code{...}}

\item{sm_method}{Argument specifying which smoothing method should
be used to smooth data. Options include 
"moving-average", "moving-median", "loess", and "gam"}

\item{subset_by}{An optional vector as long as \code{y}. 
\code{y} will be split by the unique values of this vector 
and the derivative for each group will be calculated 
independently of the others.

This provides an internally-implemented approach similar
to \code{dplyr::group_by} and \code{dplyr::mutate}}

\item{return_fitobject}{Boolean indicating whether entire object returned
by fitting function should be returned. If FALSE,
just fitted values are returned.}
}
\value{
If return_fitobject == FALSE:

        A vector, the same length as \code{y}, with the now-smoothed y values
        
        If return_fitobject == TRUE:
        
        A list the same length as unique(subset_by) where each element is
        an object of the same class as returned by the smoothing method
        (typically a named list-like object)
        
        Varies by method, but always with a first element named 'fitted'
        containing the smoothed values of the response variable, and a 
        second element named 'residuals' containing the residuals of the
        fitted values and the input values
}
\description{
This function calls other functions to smooth growth curve data
}
\details{
For \code{moving_average} and \code{moving_median}, 
           passing \code{window_width} or \code{window_width_n} via 
           \code{...} is required. \code{window_width} sets the width
           of the moving window in units of \code{x}, while 
           \code{window_width_n} sets the width in units of number
           of data points. Larger values for either will produce more 
           "smoothed" data.
           
           For \code{loess}, the \code{span} argument sets the fraction of
           data points that should be included in each calculation. It's
           typically best to specify, since the default of 0.75 is often
           too large for growth curves data. Larger values of \code{span} 
           will produce more more "smoothed" data
           
           For \code{gam}, both arguments to \code{gam} and \code{s} can
           be provided via \code{...}. Most frequently, the \code{k} 
           argument to \code{s} sets the number of "knots" the
           spline-fitting can use. Smaller values will be more "smoothed".
           
           When using \code{sm_method = "gam"}, advanced users may also modify 
           other parameters of \code{s()}, including the smoothing basis 
           \code{bs}. These bases can be thin plate (\code{bs = "tp"}, 
           the default), cubic regressions (\code{bs = "cr"}), or many other 
           options (see \code{?mcgv::s}). I recommend leaving the default 
           thin plate regressions, whose main drawback is that they are 
           computationally intensive to calculate. For growth curves data, 
           this is unlikely to be relevant.
           
           As an alternative to passing \code{y}, for more advanced needs 
           with \code{loess} or \code{gam}, \code{formula} and \code{data} 
           can be passed to \code{smooth_data} via the \code{...} argument 
           (in lieu of \code{y}).
         
           In this case, the formula should specify the response (e.g. density) 
           and predictors. For \code{gam} smoothing, the formula should
           typically be of the format: y ~ s(x), which uses 
           \code{mgcv::s} to smooth the data. The data argument should be a 
           \code{data.frame} containing the variables in the formula.
           In such cases, \code{subset_by} can still be specified as a vector
           as long as \code{nrow(data)}
}