#' Build a ggsubplot object for rendering
#' 
#' ggsubplot_build takes a ggsubplot object and performs all steps 
#' necessary to produce an object that can be rendered. This function outputs 
#' two pieces: a list of data frames (one for each layer), and a panel object, 
#' which contain all information about axis limits, breaks, etc.
#' 
#' @keywords internal
#' @param layer an object of class sp_layer
#' @seealso print.ggsubplot and \code{\link{sp_layer_build}} for functions that 
#' contain the complete set of steps for generating a ggsubplot plot
#' @export
ggsubplot_build <- function(plot1){
  if (length(plot1$layers) == 0) stop("No layers in plot", call.=FALSE)
  if (!identical(plot1$facet, ggplot2::facet_null())) {
  	stop("ggsubplots do not support facetting", call. = FALSE)
  }

  f <- get("plot_clone", envir = asNamespace("ggplot2"))
  plot <- f(plot1)
  layers <- plot$layers
  layers <- propogate_data(layers, plot$data)
	
  # separate into sp_layers and normal layers
  spls <- unlist(lapply(layers, is.sp_layer))
  if (all(!spls)) return(ggplot2::ggplot_build(plot))
  if (all(spls) && sum(spls) == 1) {
    return(sp_layer_build(layers[[spls]], plot))
  }
  splayers <- layers[spls]
  plot$layers <- layers[!spls]
  spl.order <- seq_along(layers)[spls]
  nl.order <- seq_along(layers)[!spls]
	
  # build normal layers
  normal <- NULL
  if (length(plot$layers) > 0) {
	normal <- ggplot2::ggplot_build(plot)
  }
	
  # build glyph layers (embedded plots)
  embedded <- list()
  for (i in seq_along(splayers)) {
    embedded[[i]] <- sp_layer_build(splayers[[i]], plot)
  }
	
	
  ### combine the builds
	
  # plot
  build <- embedded[[1]]
	
  # data
  # take care to order
  edata <- lapply(embedded, function(bd) bd$data[[1]])
  data <- list()
  data[spl.order] <- edata
  data[nl.order] <- normal$data
	
  # panel
  xspan <- range(unlist(lapply(data, function(df) df[names(df) %in% .x_aes])), 
    na.rm = TRUE)
  yspan <- range(unlist(lapply(data, function(df) df[names(df) %in% .y_aes])), 
    na.rm = TRUE)
  minimal <- plot
  minimal$data <- ggplot2::waiver()
  minimal$layers <- list(geom_point(aes(xspan, yspan), 
    data = data.frame(xspan, yspan)))
  panel <- ggplot2::ggplot_build(minimal)$panel
	
  # scales 
  # collect all unique scales
  scales <- build$plot$scales$scales
  scales[[which_x(scales)]] <- panel$x_scales[[1]]
  scales[[which_y(scales)]] <- panel$y_scales[[1]]
  # git rid of untrained scales
  scales[which_untrained(scales)] <- NULL
  
  scale.names <- names_scales(scales)
  for (i in seq_along(embedded[-1])) {
    escales <- embedded[[i + 1]]$plot$scales$scales
	unique <- !(names_scales(escales) %in% scale.names)
	scales <- c(scales, escales[unique])
	scale.names <- names_scales(scales)
  }
  nscales <- normal$plot$scales$scales
  unique <- !(names_scales(nscales) %in% scale.names)
  scales <- c(scales, nscales[unique])
	
  # layers
  # take care to order
  spl.layers <- build$plot$layers
  for (i in seq_along(embedded[-1])) {
    spl.layers <- c(spl.layers, embedded[[i + 1]]$plot$layers)
  }
  layers[spl.order] <- spl.layers
  layers[nl.order] <- normal$plot$layers
  
  # labels 
  # collect all unique labels
  labels <- build$plot$option$labels
  for (i in seq_along(embedded[-1])) {
  	new.labels <- embedded[[i+1]]$plot$options$labels
  	unique <- !(names(new.labels) %in% names(labels))
  	labels <- c(labels, new.labels[unique])
  }
  norm.labels <- normal$plot$options$labels
  unique <- !(names(norm.labels) %in% names(labels))
  labels <- c(labels, norm.labels[unique])
	
  # make build
  build$data <- data
  build$panel <- panel
  build$plot$scales$scales <- scales
  build$plot$layers <- layers
  build$plot$options$labels <- labels

  f <- get("set_last_plot", envir = asNamespace("ggplot2"))
  f(plot1)
  
  build
}

#' Ensure each layer contains a data set
#' 
#' propogate_data checks each layer for a data set. If none is found it assigns 
#' a copy of the plot level data set to the layer. propogate_data avoids the 
#' side effects of ggplot2:::map_layout, which performs a similar function.
#' @param layers ggplot2 layer objects
#' @param plot_data the global data set for a ggplot2 plot
propogate_data <- function(layers, plot_data) {
	ensure_data <- function(layer){
		if (inherits(layer$data, "waiver")) {
			layer$data <- plot_data
		}
		layer
	}
	lapply(layers, ensure_data)
}


#' Ensure each layer contains all aesthetic mappings that affect it.
#' 
#' propogate_aes checks for aesthetics defined at the global level of a plot 
#' that affect a layer. Propogate_aes moves such aesthetics into the layer's 
#' mapping. 
#' @param layer ggplot2 layer objects
#' @param plot_mapping the global data set for a ggplot2 plot
propogate_aes <- function(layer, plot_mapping) {
  mapping <- c(layer$mapping, 
    plot_mapping[setdiff(names(plot_mapping), names(layer$mapping))])
  class(mapping) <- "uneval"
  layer$mapping <- mapping
  layer
}


#' find scales that have no range yet
#' 
#' @param scales A list of ggplot2 scales
which_untrained <- function(scales) {
  no_range <- function(scale) is.null(scale$range$range)
  unlist(lapply(scales, no_range))
}
