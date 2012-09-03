\name{unpanel}
\alias{unpanel}
\title{Format data from a facet plot to use in a ggsubplot plot}
\usage{
  unpanel(df)
}
\arguments{
  \item{df}{A data frame. Should be the output of a
  facetted plot built with
  \code{\link[ggplot2]{ggplot_build}}}
}
\value{
  A modified data frame. See Details.
}
\description{
  unpanel replaces the PANEL variable of a data frame with
  a GLYPH variable. It adjusts the data frame's group
  variable to retain the grouping information provided by
  the PANEL variable.
}

