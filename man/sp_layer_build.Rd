\name{sp_layer_build}
\alias{sp_layer_build}
\title{Build a sp_layer for rendering}
\usage{
  sp_layer_build(layer, plot)
}
\arguments{
  \item{layer}{an object of class sp_layer}
}
\description{
  sp_layer_build takes a layer of subplots (class
  sp_layer), and performs all steps necessary to produce an
  object that can be rendered. This function outputs two
  pieces: a list of data frames (one for each layer), and a
  panel object, which contains all information about axis
  limits, breaks, etc.
}
\details{
  If the sp_layer is accompanied by regular layers,
  sp_layer_build will be used in conjunction with
  \code{\link{ggsubplot_build}} to build the plot for
  rendering.
}
\seealso{
  print.ggsubplot and \code{\link{ggsubplot_build}} for
  functions that contain the complete set of steps for
  generating a ggsubplot plot
}
\keyword{internal}

