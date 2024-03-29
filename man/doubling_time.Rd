% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{doubling_time}
\alias{doubling_time}
\title{Calculate doubling time equivalent of per-capita growth rate}
\usage{
doubling_time(y, x_scale = 1)
}
\arguments{
\item{y}{Vector of per-capita derivative data to calculate the 
equivalent doubling time of}

\item{x_scale}{Numeric to scale per-capita derivative values by

Set x_scale to the ratio of the the units of 
y to the desired units. E.g. if y is in per-second, but the 
desired doubling time is in minutes, \code{x_scale = 60} 
(since there are 60 seconds in 1 minute).}
}
\value{
A vector of values for the doubling time equivalent to the
        per-capita growth rate supplied for \code{y}
}
\description{
Provided a vector of per-capita growth rates, this function returns 
the vector of equivalent doubling times
}
