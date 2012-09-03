\name{ply_aes}
\alias{ply_aes}
\title{Compute aesthetics groupwise}
\usage{
  ply_aes(layer, .vars = NULL)
}
\arguments{
  \item{layer}{a ggplot2 layer or sp_layer object. This
  layer's aesthetics will be computed groupwise, but the
  layer will remain the same in every other respect.}

  \item{.vars}{variable names to group by (optional),
  stored as a character string}
}
\description{
  ply_aes causes the aesthetics of a layer to be computed
  groupwise. ply_aes implements the split-apply-combine
  strategy of data analysis in a graphical framework. It
  first splits a layer's data frame into subgroups, then
  evaluates the layers mappings separately within each
  group, and finally combines the results into a single
  data frame which is used to build the plot for rendering.
}
\details{
  Users may specify which groupings to use through the
  .vars argument. If this argument is left NULL, ply_aes
  will search for and use a group aes, a glyphing or
  gridding criteria (in a sp_layer), a facetting criteria,
  or any combination of these that it finds.
}

