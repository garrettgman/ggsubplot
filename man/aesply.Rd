\name{aesply}
\alias{aesply}
\title{Evaluate aesthetics by group}
\usage{
  aesply(data, .var, aesthetics)
}
\arguments{
  \item{data}{a data frame}

  \item{.vars}{a vector of variables to split data by. Each
  element should be the name of a variable in data saved as
  a character string.}

  \item{aesthetics}{an object of class uneval, usually the
  output of \code{\link[ggplot2]{aes}}}
}
\value{
  a data frame
}
\description{
  aesply splits a data frame into pieces, evaluates a list
  of aesthetics within each piece, and combines the results
  into a new data frame. Each aesthetic in the list must
  return either a single value per piece or a single value
  per row in the piece.
}
\keyword{internal}

