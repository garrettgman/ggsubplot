\name{rescale_2pi}
\alias{rescale_2pi}
\title{rescale vectors to [0, 2 * pi]}
\usage{
  rescale_2pi(xvars, xlim = NULL, zero = FALSE)
}
\arguments{
  \item{xvars}{a list of vectors}

  \item{xlim}{NULL (default) or a numeric vector of length
  two that specifies the range of values to scale to [0, 2
  * pi]}

  \item{zero}{logical. Should zero be added to the range
  before rescaling?}
}
\value{
  a list of vectors
}
\description{
  rescale_2pi rescales every vector in a list of vectors to
  the range [0, 2 * pi] (e.g, for working with radians).
  rescale_2pi rescales the vectors as a group (instead of
  rescaling each vector independently). This is a helpful
  feature for rescaling related variables (such as xmin and
  xmax) without nullifying the difference between the two.
}
\details{
  If a vector is a character or factor vector, rescale_11
  attempts to coerce it to numeric before scaling. The
  scale is determined by finding the range of values
  contained in the list of vectors and mapping it to [0, 2
  * pi].

  If the full range of values to be scaled is not present
  in the vectors, users can specify the range to be scaled
  to [0, 2 * pi] with the xlim argument. Values in the
  vectors will be rescaled as if they according to this
  range.
}
\seealso{
  \code{\link{rescale_01}}, \code{\link{rescale_11}}
}

