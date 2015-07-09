#' Build a sp_layer for rendering
#' 
#' sp_layer_build takes a layer of subplots (class sp_layer), and performs all 
#' steps necessary to produce an object that can be rendered. This function 
#' outputs two pieces: a list of data frames (one for each layer), and a panel 
#' object, which contains all information about axis limits, breaks, etc.
#' 
#' If the sp_layer is accompanied by regular layers, sp_layer_build will be used 
#' in conjunction with \code{\link{ggsubplot_build}} to build the plot for 
#' rendering.
#' 
#' @keywords internal
#' @param layer an object of class sp_layer
#' @seealso print.ggsubplot and \code{\link{ggsubplot_build}} for 
#' functions that contain the complete set of steps for generating a ggsubplot 
#' plot
#' @export
sp_layer_build <- function(layer, plot) {
  if (!("embed" %in% ls(layer))) {
    stop("layer does not have embedded subplots")
  }

  f <- get("plot_clone", envir = asNamespace("ggplot2"))
  minimal <- f(plot)
  minimal$data <- ggplot2::waiver()
  minimal$scales$scales[which_x(minimal$scales$scales)] <- NULL
  minimal$scales$scales[which_y(minimal$scales$scales)] <- NULL
  layer <- layer_clone(layer)
  layer$data <- layer$assign_subplots(layer$data, plot$plot_env)

  minimal$layers <- list(layer)
  minor <- ggplot2::ggplot_build(minimal + ggplot2::facet_wrap("SUBPLOT")) 

  ### combine subplots (minor) into single plot
  # data
  data <- unpanel(minor$data[[1]])
  data <- layer$combine_subplots(data)
  data$PANEL <- 1L
	
  # panel
  xspan <- range(unlist(data[names(data) %in% .x_aes]), na.rm = TRUE)
  yspan <- range(unlist(data[names(data) %in% .y_aes]), na.rm = TRUE)
  minimal$layers <- list(geom_point(aes(xspan, yspan), 
    data = data.frame(xspan, yspan)))
  panel <- ggplot2::ggplot_build(minimal)$panel

  # scales
  scales <- minor$plot$scales$scales
  scales[[which_x(scales)]] <- panel$x_scales[[1]]
  scales[[which_y(scales)]] <- panel$y_scales[[1]]
	
  # axis labels
  if (!is.null(layer$embed$major.aes)) {
    labels <- ggplot2::labs(layer$embed$major.aes)
    minor$plot$options$labels[c("x", "y")] <- labels[c("x", "y")]
  }
		
  # make build
  minor$data <- list(data)
  minor$panel <- panel
  minor$plot$facet <- ggplot2::facet_null()
  minor$plot$scales$scales <- scales
  
  f <- get("set_last_plot", envir = asNamespace("ggplot2"))
  f(plot)
  
  minor
}

#' Format data from a facet plot to use in a ggsubplot plot
#' 
#' unpanel replaces the PANEL variable of a data frame with a GLYPH variable. It 
#' adjusts the data frame's group variable to retain the grouping information 
#' provided by the PANEL variable.
#' 
#' @param df A data frame. Should be the output of a facetted plot built with 
#' \code{\link[ggplot2]{ggplot_build}}
#' @return A modified data frame. See Details.
#' @noRd
unpanel <- function(df) {
  if (!is.null(df$group)) {
    df$group <- interaction(df$group, df$PANEL)
  } 
  df$SUBPLOT <- as.numeric(as.character(df$PANEL))
  df$PANEL <- NULL
  df
}

which_x <- function(scales) {
  vars <-  names_scales(scales)
  which(vars == "x")
}

which_y <- function(scales) {
  vars <- names_scales(scales)
  which(vars == "y")
}

names_scales <- function(scales) {
  unlist(lapply(scales, function(s) s$aesthetics[[1]]))
