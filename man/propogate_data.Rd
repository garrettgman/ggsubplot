\name{propogate_data}
\alias{propogate_data}
\title{Ensure each layer contains a data set}
\usage{
  propogate_data(layers, plot_data)
}
\arguments{
  \item{layers}{ggplot2 layer objects}

  \item{plot_data}{the global data set for a ggplot2 plot}
}
\description{
  propogate_data checks each layer for a data set. If none
  is found it assigns a copy of the plot level data set to
  the layer. propogate_data avoids the side effects of
  ggplot2:::map_layout, which performs a similar function.
}

