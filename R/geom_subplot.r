#' Create a layer of embedded subplots
#' 
#' @param mapping An aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. This mapping determines where in the major x and 
#' y axes each glyph will be position. Only x, y, group, and subplot aesthetics 
#' will be used. All other aesthetics will be ignored - consider placing them in 
#' the subplot's mapping instead.
#' @param width numeric or rel object. The width of each glyph. If width is 
#' numeric, the glyph will be drawn with a width equal to width units on the x 
#' axis. If width is of class \code{\link{rel}}, glyph will 
#' attempt to assign an inuitive width based on the number of total glyphs and 
#' their placement within the plot. The width can be scaled relative to this 
#' intuitive width by changing the value of the rel object.
#' @param height numeric or rel object. The height of each glyph. Height behaves 
#' the same way as width, but applies to the y dimension.
#' @param data The dataframe the layer should map to. geom_subplot inherits the 
#' global dataframe defined in \code{\link[ggplot2]{ggplot}}.
#' @param x_scale function. The scaling to use for the x axis within each glyph. 
#' If x_scale equals \code{\link{identity}}(default), the x limits within each 
#' glyph will correspond to the range of x across all glyphs. This aids 
#' comparison because each glyph will use the same scale. If x_scale equals 
#' \code{\link{free}}, each glyph will use its own x scale. The limits of this 
#' scale will be set to the range of x values in that glyph.
#' @param y_scale function. y_scale behaves the same as x_scale but controls the 
#' scales for the y axis within each glyph.
#' @param position character. A string that specifies which position adjustment 
#' should be used. geom_subplot only recognizes "identity" and "merge". 
#' @param reference function. Function used to create reference objects for 
#' glyphs. If NULL, no reference objects are used. Reference objects are plotted 
#' on a layer beneath the glyphs. They provide a consistent frame of reference 
#' to aid comparisons between the glyphs. Functions that create reference 
#' objects include \code{\link{ref_box}}, \code{\link{ref_hline}}, 
#' and \code{\link{ref_vline}}.
#' @param ply.aes logical. If TRUE (default) aesthetics are calculated 
#' separately for each group, as with \code{\link{ply_aes}}. If FALSE aesthetics 
#' are calculated based on entire data set for the layer.
#' @param .ref internal argument used for plotting reference objects.
#' @return an object of class sp_layer
#' @examples
#' \dontrun{ggplot(nasa) + 
#' map_americas + 
#' geom_subplot(aes(long, lat, group = id, 
#'   subplot = geom_point(aes(surftemp, temperature), size = 1/4))) +
#' coord_map()
#' }
#' @export
geom_subplot <- function(mapping, width = rel(0.95), height = rel(0.95), 
  data = waiver(), x_scale = identity, y_scale = identity, 
  position = "identity", reference = NULL, ply.aes = TRUE, .ref = FALSE) {
  
  missing <- c(is.null(mapping$x), is.null(mapping$y), is.null(mapping$group), 
    is.null(mapping$subplot))
  if (any(missing)) {
    stop(paste("Missing required aesthetics in geom_subplot:", 
      paste(c("x", "y", "group", "subplot")[missing], collapse = ", ")))
  }
  
  if (position != "identity" & position != "merge") {
    stop("geom_subplot only supports position = 'identity' or 'merge'", 
      call. = FALSE)
  }
  
  if (position == "merge") {
    merge.overlaps <- TRUE
  } else {
    merge.overlaps <- FALSE
  }
  
  layer <- extract_layer(mapping$subplot, parent.frame())
  layer$data <- data
  mapping$subplot <- NULL
  layer$embed <- list(width = width, height = height, x_scale = x_scale, 
    y_scale = y_scale, merge.overlaps = merge.overlaps, major.aes = mapping)
  layer$assign_subplots <- assign_subplots
  layer$combine_subplots <- combine_subplots
  if (.ref) layer$combine_subplots <- combine_refs
  
  if (is.null(reference)) {
    if (ply.aes) {
      ply_aes(sp_layer(layer))
    } else {
      sp_layer(layer)
    }
  } else {
    ref.layer <- reference(layer, "subplot", mapping, width, height, position)
    if (ply.aes) {
      list(ref.layer, ply_aes(sp_layer(layer)))
    } else {
      list(ref.layer, sp_layer(layer))
    }
  }
}

extract_layer <- function(subplot_aes, env) {
  layer <- eval(subplot_aes, env)
  if ("sp_plot" %in% class(layer) | "ggsubplot" %in% class(layer)) {
    stop("Cannot place subplots inside subplots", call. = FALSE)
  }
  if ("ggplot" %in% class(layer)) {
    # propogate data and aesthetics
    layer$data <- NULL
    mapping <- layer$mapping
    layer <- propogate_aes(layer, mapping)
  }
  if (!("proto" %in% class(layer))) {
    stop("subplot aes should be a ggplot2 layer object", call. = FALSE)
  }
  layer
}
        
    


# Assigns subplot membership to rows
# 
# assign_subplots assigns each row in a layer's data set to a subplot during 
# \code{\link{sp_layer_build}}. assign_subplots sets final width and heights 
# when width and heights are passed as rel objects. It computes and the position 
# aesthetics for each subplot and stores them in the layer's embed variable to 
# be used by combine_subplots. assign_subplots also handles merging when 
# position = "merge" in a \code{\link{geom_subplot}} call.
assign_subplots <- function(., data, plot.env) {
  
  # major x and y
  data$SUBPLOT <- eval(embed$major.aes$group, data, plot.env) 
  data$SUBPLOT <- as.numeric(factor(data$SUBPLOT))
  embed$major.aes$group <- NULL
  globals <- aesply(data, "SUBPLOT", embed$major.aes)
  too.many <- c(length(unique(globals$x)) > length(unique(globals$SUBPLOT)), 
    length(unique(globals$y)) > length(unique(globals$SUBPLOT)))
  if (any(too.many)) {
    message(paste("Major", paste(c("x", "y")[too.many], collapse = " and "), 
      "return more than one value per subplot. Only using first."))
    globals <- unique(plyr::ddply(globals, "SUBPLOT", transform, x = x[1], 
      y = y[1]))
  }

  # parse width, height
  width <- embed$width
  height <- embed$height
  if (is.rel(width)) {
    .$embed$width <- width <- max(
      ggplot2::resolution(vet(globals$x), zero = FALSE) * unclass(width), 
      (diff(range(vet(globals$x), na.rm = TRUE)) + unclass(width)) / 
      length(unique(globals$x)) * unclass(width)
    )
  }
  if (is.rel(height)) {
    .$embed$height <- height <- max(
      ggplot2::resolution(vet(globals$y), zero = FALSE) * unclass(height), 
      (diff(range(vet(globals$y), na.rm = TRUE)) + unclass(height)) / 
      length(unique(globals$y)) * unclass(height)
    )
  }

  if (embed$merge) {
    # search for overlapping glyphs, combine
    data$.gid <- factor(data$SUBPLOT)
    merge.key <- merge_overlaps(globals, .$embed$width, .$embed$height)
    data$SUBPLOT <- merge.key[data$SUBPLOT]
    globals <- aesply(data, "SUBPLOT", embed$major.aes)
    .$mapping <- add_gid(.$mapping)
    
    too.many <- c(length(unique(globals$x)) > length(unique(globals$SUBPLOT)), 
      length(unique(globals$y)) > length(unique(globals$SUBPLOT)))
    if (any(too.many)) {
      message(paste("Major", paste(c("x", "y")[too.many], collapse = " and "), 
        "return more than one value per subplot. Only using first."))
      globals <- unique(plyr::ddply(globals, "SUBPLOT", transform, x = x[1], 
        y = y[1]))
    }
  }
  .$embed$globals <- globals
  data
}

# Calculate final positions in a sp_layer
# 
# combine_subplots calculates the final positions for every location in a 
# sp_layer. sp_layer_build first builds each subplot separately as if it were a 
# facet. If plotted, these subplots would overlap with each other. 
# combine_subplots relocates each subplot based on the global positions stored 
# in the layer's embed variable.
combine_subplots <- function(., data) {  
  data <- plyr::join(data, globalize(embed$globals), by = "SUBPLOT")
  
  xvar <- get_xs(data)
  yvar <- get_ys(data)
  
  # scale if necessary
  if (!identical(embed$x_scale, identity) || 
    !identical(embed$y_scale, identity)) {
    data <- plyr::ddply(data, "SUBPLOT", function(df) {
      df[xvar] <- embed$x_scale(df[xvar])
      df[yvar] <- embed$y_scale(df[yvar])
      df
    })
  }
  
  # update x and y related variables
  # don't scale individually or xmin and xmax's will end up on top of 
  # one another
  data[xvar] <- vet(data$X) + rescale_11(data[xvar]) * embed$width/2
  data[yvar] <- vet(data$Y) + rescale_11(data[yvar]) * embed$height/2
  
  data$X <- NULL
  data$Y <- NULL
  data
}  
  

# combine_refs relocates reference objects within a layer. It works exactly like 
# combine_subplots but does not rescale the x and y variables for the reference 
# object.
combine_refs <- function(., data) {  
  data <- plyr::join(data, globalize(embed$globals), by = "SUBPLOT")
		
  xvar <- get_xs(data)
  yvar <- get_ys(data)
		
	# scale if necessary
  if (!identical(embed$x_scale, identity) || 
    !identical(embed$y_scale, identity)) {
    data <- plyr::ddply(data, "SUBPLOT", function(df) {
      df[xvar] <- embed$x_scale(df[xvar])
	  df[yvar] <- embed$y_scale(df[yvar])
	  df
	})
  }

  # update x and y related variables
  # don't scale individually or xmin and xmax's will end up on top of 
  # one another
  data[xvar] <- vet(data$X) + data[xvar] * embed$width/2
  data[yvar] <- vet(data$Y) + data[yvar] * embed$height/2
		
  data$X <- NULL
  data$Y <- NULL
  data
}


#' Ensure that an object is numeric
#' 
#' vet tests whether an object is a factor or character string. If so it 
#' attempts to coerce the variable to numeric.
#' 
#' @keywords internal
#' @param x an R object
#' @return a numeric object
#' @export
vet <- function(x) {
  if (is.character(x)) {
    x <- as.numeric(factor(x))
  }	
  if (is.factor(x)) {
  	x <- as.numeric(x)
  }
  x
}

#' rename global x and y variables in capitals
#' @keywords internal
#' @param obj a data.frame
#' @export
globalize <- function(obj){
	names(obj)[names(obj) == "x"] <- "X"
	names(obj)[names(obj) == "y"] <- "Y"
	obj
}

#' Include .gid in groupings
#' 
#' add_gid intelligently adds the .gid variable to the group slot of an uneval 
#' object. If the group slot is NULL, add_gid sets group = .gid. If the group 
#' slot already contains a mapping, add_gid adds .gid to this mapping with 
#' interaction().
#'
#' @keywords internal
#' @param aes_group the group value of an uneval object
#' @export
add_gid <- function(maps) {
  if (is.null(maps$group)) {
    maps$group <- as.name(".gid")
  } else {
    maps$group <- as.call(list(quote(interaction), as.name(".gid"), 
      maps$group))
  }
  maps
}