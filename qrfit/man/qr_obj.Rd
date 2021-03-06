% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qrobj.R
\name{qr_obj}
\alias{qr_obj}
\title{The objective function of conditional quantile regression model}
\usage{
qr_obj(beta, y, X, tau)
}
\arguments{
\item{beta}{The (p * 1) coefficient vector of the regression model.}

\item{y}{The (n * 1) dependent response variable vector of the regression model.}

\item{X}{The (n * p) independent cause variable matrix of the regression model.}

\item{tau}{The constant level chosen according to which quantile needs to be estimated.}
}
\value{
The value of the objective function of conditional quantile regression model.
}
\description{
The objective function of conditional quantile regression model
}
