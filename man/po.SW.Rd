% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/po_fun_test.R
\name{po.SW}
\alias{po.SW}
\title{Perform Stochastic Weight Portfolio Optimization}
\usage{
po.SW(x0, b, sample)
}
\arguments{
\item{x0}{A numeric matrix of asset returns.}

\item{b}{Number of assets to select in each sample.}

\item{sample}{Number of random samples to generate.}
}
\value{
A numeric vector of optimized portfolio weights.
}
\description{
This function performs stochastic weight portfolio optimization.
}
