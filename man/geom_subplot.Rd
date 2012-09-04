\name{geom_subplot}
\alias{geom_subplot}
\title{Create a layer of embedded subplots}
\usage{
  geom_subplot(mapping, width = rel(0.95), height =
  rel(0.95), data = waiver(), x_scale = identity, y_scale =
  identity, position = "identity", reference = NULL,
  ply.aes = TRUE, .ref = FALSE)
}
\arguments{
  \item{mapping}{An aesthetic mapping, usually constructed
  with \code{\link[ggplot2]{aes}}. This mapping determines
  where in the major x and y axes each glyph will be
  position. Only x, y, group, and subplot aesthetics will
  be used. All other aesthetics will be ignored - consider
  placing them in the subplot's mapping instead.}

  \item{width}{numeric or rel object. The width of each
  glyph. If width is numeric, the glyph will be drawn with
  a width equal to width units on the x axis. If width is
  of class \code{\link{rel}}, glyph will attempt to assign
  an inuitive width based on the number of total glyphs and
  their placement within the plot. The width can be scaled
  relative to this intuitive width by changing the value of
  the rel object.}

  \item{height}{numeric or rel object. The height of each
  glyph. Height behaves the same way as width, but applies
  to the y dimension.}

  \item{data}{The dataframe the layer should map to.
  geom_subplot inherits the global dataframe defined in
  \code{\link[ggplot2]{ggplot}}.}

  \item{x_scale}{function. The scaling to use for the x
  axis within each glyph. If x_scale equals
  \code{\link{identity}}(default), the x limits within each
  glyph will correspond to the range of x across all
  glyphs. This aids comparison because each glyph will use
  the same scale. If x_scale equals \code{\link{free}},
  each glyph will use its own x scale. The limits of this
  scale will be set to the range of x values in that
  glyph.}

  \item{y_scale}{function. y_scale behaves the same as
  x_scale but controls the scales for the y axis within
  each glyph.}

  \item{position}{character. A string that specifies which
  position adjustment should be used. geom_subplot only
  recognizes "identity" and "merge".}

  \item{reference}{function. Function used to create
  reference objects for glyphs. If NULL, no reference
  objects are used. Reference objects are plotted on a
  layer beneath the glyphs. They provide a consistent frame
  of reference to aid comparisons between the glyphs.
  Functions that create reference objects include
  \code{\link{ref_box}}, \code{\link{ref_hline}}, and
  \code{\link{ref_vline}}.}

  \item{ply.aes}{logical. If TRUE (default) aesthetics are
  calculated separately for each group, as with
  \code{\link{ply_aes}}. If FALSE aesthetics are calculated
  based on entire data set for the layer.}

  \item{.ref}{internal argument used for plotting reference
  objects.}
}
\value{
  an object of class sp_layer
}
\description{
  Create a layer of embedded subplots
}
\examples{
\dontrun{ggplot(nasa) +
map_americas +
geom_subplot(aes(long, lat, group = id,
subplot = geom_point(aes(surftemp, temperature), size = 1/4))) +
coord_map()
}
}

