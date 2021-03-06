% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_line_DatNum.R
\name{gg_line_DatNum}
\alias{gg_line_DatNum}
\title{Line Chart Date Numeric}
\usage{
gg_line_DatNum(data, ...)
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
gg_line_DatNum(sample_data("Dat-Num", nrow = 10))
}
