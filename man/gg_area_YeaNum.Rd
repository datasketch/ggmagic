% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_area_YeaNum.R
\name{gg_area_YeaNum}
\alias{gg_area_YeaNum}
\title{Area Chart Year Numeric}
\usage{
gg_area_YeaNum(data, ...)
}
\arguments{
\item{data}{A data.frame}

\item{...}{
  Arguments passed on to \code{\link[dsvizopts:chart_viz_options]{dsvizopts::dsviz_default_opts}}
  \describe{
    \item{\code{}}{}
  }}

\item{orientation}{Doesn't do anything for this type of chart.}

\item{order}{doesn't do anything}
}
\description{
This chart does not allow for chaning orientation
}
\section{ctypes}{

Dat-Num, Yea-Num
}

\examples{
gg_area_YeaNum(sample_data("Cat-Num", nrow = 10))
}
