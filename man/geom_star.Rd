\name{geom_star}
\alias{geom_star}
\title{Star glyphs}
\usage{
  geom_star(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", na.rm = FALSE, r.zero = TRUE,
    ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link[ggplot2]{aes}}. Only needs to be set at
  the layer level if you are overriding the plot defaults.
  x and y will determine where the center of the star
  should appear. r and angle will determine the polar
  coordinate system for the star.}

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
\examples{
\dontrun{## A single star
one_nasa <- nasa[nasa$id == "1-1", ]
ggplot(one_nasa) +
geom_star(aes(x = 0, y = 0, r = surftemp, angle = date,
fill = mean(temperature)), r.zero = FALSE)

## Stars in an embedded plot
ggplot(nasa) +
map_americas +
geom_subplot(aes(long, lat, group = id,
subplot = geom_star(aes(x = 0, y = 0, r = surftemp,
angle = date, fill = mean(surftemp)), r.zero = FALSE))) +
coord_map()
}
}

