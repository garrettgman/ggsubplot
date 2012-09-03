\name{geom_star}
\alias{geom_star}
\title{Star glyphs}
\usage{
  geom_star(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, r.zero = TRUE, ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link[ggplot2]{aes}}. Only needs to be set at
  the layer level if you are overriding the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults}

  \item{stat}{The statistical transformation to use for
  this layer.}

  \item{position}{The position adjustment to use for
  overlapping points in this layer}

  \item{na.rm}{If FALSE (the default), removes missing
  values with a warning. If TRUE, silently removes missing
  variables.}

  \item{r.zero}{logical. Should the origin of the star
  correspond to r = 0? If FALSE, origin corresponds to
  lowest value of r.}

  \item{...}{other arguments passed on to
  \code{\link[ggplot2]{layer}}. This can include aesthetics
  whose values you want to set, not map. See
  \code{\link[ggplot2]{layer}} for more details.}
}
\description{
  geom_star draws the type of glyph commonly called a star
  plot, radar plot, or polar plot.
}
\section{Aesthetics
geom_coxcomb understands the following aesthetics}{
  x, y, colour, fill, size, linetype, weight, and alpha.
}

