\name{geom_freqstar}
\alias{geom_freqstar}
\title{Frequency Star glyphs}
\usage{
  geom_freqstar(mapping = NULL, data = NULL, stat = "bin",
  position = "identity", na.rm = FALSE, ...)
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

  \item{...}{other arguments passed on to
  \code{\link[ggplot2]{layer}}. This can include aesthetics
  whose values you want to set, not map. See
  \code{\link[ggplot2]{layer}} for more details.}
}
\description{
  geom_freqstar draws the type of glyph commonly called a
  star plot, radar plot, or polar plot. geom_freqstar bins
  the data provided as the angle variable and then maps
  frequency statistics to the r aesthetic. Compare with
  \code{\link{geom_star}}
}
\section{Aesthetics
geom_coxcomb understands the following aesthetics}{
  x, y, colour, fill, size, linetype, weight, and alpha.
}

