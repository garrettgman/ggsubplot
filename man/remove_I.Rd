\name{remove_I}
\alias{remove_I}
\title{replace I() with identity}
\usage{
  remove_I(expr)
}
\arguments{
  \item{expr}{an expression}
}
\description{
  remove_I searches through an expression for the
  \code{\link{I}} function and replaces it with
  \code{\link{identity}}. remove_I is used when an
  aesthetic has been surrounded with I() to prevent
  groupwise calculation.
}

