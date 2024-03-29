% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{auc}
\alias{auc}
\title{Calculate area under the curve}
\usage{
auc(
  x,
  y,
  xlim = NULL,
  blank = 0,
  subset = NULL,
  na.rm = TRUE,
  neg.rm = FALSE,
  warn_xlim_out_of_range = TRUE,
  warn_negative_y = TRUE
)
}
\arguments{
\item{x}{Numeric vector of x values}

\item{y}{Numeric vector of y values}

\item{xlim}{Vector, of length 2, delimiting the x range over which the
area under the curve should be calculated (where NA can be
provided for the area to be calculated from the start or to
the end of the data)}

\item{blank}{Value to be subtracted from \code{y} values before calculating
area under the curve}

\item{subset}{A vector of logical values indicating which x and y values
should be included (TRUE) or excluded (FALSE).}

\item{na.rm}{a logical indicating whether missing values should be removed}

\item{neg.rm}{a logical indicating whether \code{y} values below zero should 
be treated as zeros. If \code{FALSE}, area under the curve
for negative \code{y} values will be calculated normally,
effectively subtracting from the returned value.}

\item{warn_xlim_out_of_range}{logical whether warning should be issued when 
xlim is lower than the lowest x value or higher
than the highest x value.}

\item{warn_negative_y}{logical whether warning should be issued when 
\code{neg.rm == FALSE} but some y values are below 0.}
}
\value{
A scalar for the total area under the curve
}
\description{
This function takes a vector of \code{x} and \code{y} values
and returns a scalar for the area under the curve, calculated using 
the trapezoid rule
}
