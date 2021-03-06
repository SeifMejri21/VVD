% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IMF_number.R
\name{IMF_number}
\alias{IMF_number}
\title{Finds appropriate number of IMFs for a given time series}
\usage{
IMF_number(ts)
}
\arguments{
\item{ts}{time series to decompose in xts format}
}
\value{
the appropriate number of IMF s
}
\description{
Finds appropriate number of IMFs for a given time series
}
\examples{
library(fpp)
data(a10)
VVD::IMF_number(a10)

}
