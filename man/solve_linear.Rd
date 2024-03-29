% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{solve_linear}
\alias{solve_linear}
\title{Return missing information about a line}
\usage{
solve_linear(
  x1,
  y1,
  x2 = NULL,
  y2 = NULL,
  x3 = NULL,
  y3 = NULL,
  m = NULL,
  named = TRUE
)
}
\arguments{
\item{x1, y1}{A point on the line}

\item{x2, y2}{An additional point on the line}

\item{x3, y3}{An additional point on the line}

\item{m}{The slope of the line}

\item{named}{logical indicating whether the returned value(s)
should be named according to what they are (m, x2, y2,
x3, or y3)}
}
\value{
A named vector with the missing information from the line:

        If \code{m} and \code{x2} are provided, \code{y2} will be returned
        
        If \code{m} and \code{y2} are provided, \code{x2} will be returned
        
        If \code{x2} and \code{y2} are provided, but neither \code{x3} nor 
        \code{y3} are provided, \code{m} will be returned
        
        If \code{x2} and \code{y2} are provided and one of \code{x3} or 
        \code{y3} are provided, the other (\code{y3} or \code{x3}) will be 
        returned
}
\description{
Takes a set of inputs that is sufficient information to infer a line
and then returns information not provided (either the slope, an x point
on the line, or a y point on the line)
}
\details{
Note that there is no requirement that 
         \code{x1} < \code{x2} < \code{x3}: the points can be in any order 
         along the line.
         
         \code{solve_linear} works with vectors of all inputs to solve
         multiple lines at once, where the \code{i}th element of each 
         argument corresponds to the \code{i}th output. Note that all
         lines must be missing the same information. Input vectors
         will be recycled as necessary.
}
