\name{sp_layer}
\alias{sp_layer}
\title{Create a sp_layer object}
\usage{
  sp_layer(layer)
}
\arguments{
  \item{layer}{a proto object that can be used as a layer
  by the \code{\link[ggplot2]{ggplot2}} package (i.e,
  ggplot() + layer should return a graph).}
}
\description{
  sp_layer gives a ggplot2 layer object the S4 class
  sp_layer, see \code{\link{sp_layer-class}}. ggplot layer
  objects are usually non-specific
  \code{\link[proto]{proto}} class objects. A layer should
  contain an embed variable before being given the class
  'sp_layer.' See the function bodies of
  \code{\link{geom_subplot}} and
  \code{\link{geom_subplot2d}} for examples.
}

