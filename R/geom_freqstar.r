#' Frequency Star glyphs
#' 
#' geom_freqstar draws the type of glyph commonly called a star plot, radar plot, 
#' or polar plot. geom_freqstar bins the data provided as the angle variable and 
#' then maps frequency statistics to the r aesthetic. Compare with 
#' \code{\link{geom_star}}
#' 
#' @param mapping The aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you 
#' are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override 
#' the plot defaults
#' @param stat The statistical transformation to use for this layer.
#' @param position The position adjustment to use for overlapping points in this 
#' layer
#' @param na.rm If FALSE (the default), removes missing values with a warning. 
#' If TRUE, silently removes missing variables.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This 
#' can include aesthetics whose values you want to set, not map. See 
#' \code{\link[ggplot2]{layer}} for more details.
#' 
#' @section Aesthetics
#' geom_coxcomb understands the following aesthetics: x, y, colour, fill, size, 
#' linetype, weight, and alpha.
#' 
#' @export
geom_freqstar <- function(mapping = NULL, data = NULL, stat = "bin", 
  position = "identity", na.rm = FALSE, ...) { 
  
  GeomFreqstar$new(mapping = mapping, data = data, stat = stat, 
    position = position, ...)
}


GeomFreqstar <- proto::proto(ggplot2:::Geom, {
  objname <- "freqstar"
  
  default_stat <- function(.) StatBin
  default_pos <- function(.) PositionIdentity
  default_aes <- function(.) aes(colour="grey20", fill=NA, size=0.5, linetype=1, 
    weight = 1, alpha = NA)
  

  # turn cartesian coordinates polar
  reparameterise <- function(., df, params) {  
    
    #remove default zeros at start and end of freqpolies
    trim_ends <- function(df) {
      if (nrow(df) > 4) return(df[-c(1, nrow(df)), ])
      df[-c(1, nrow(df)), ]
    }
    df <- ddply(df, c("group", "PANEL"), trim_ends)
    
    # scale x to be between 0 and 2*pi

    df$theta <- unlist(rescale_2pi(df["x"]))
    df$r <- unlist(rescale_01(df["y"], zero = TRUE))
    
    mark_center <- function(data) {
      origin <- data[1, ]
      origin$r <- 0
      origin$theta <- 0
      origin$plot <- FALSE
      data$plot <- TRUE
      rbind(data, origin)
    }
    
    df <- ddply(df, c("group", "PANEL"), mark_center)
    
    df$x <- df$r * cos(df$theta)
    df$y <- df$r * sin(df$theta)
    
    # ensure that (0,0) is plotted in center of graph
    center <- function(data) {
      origin <- data[data$r == 0 & data$theta == 0, ][1, ]
      xtreme <- max(abs(data$x - origin$x))
      ytreme <- max(abs(data$y - origin$y))
      fullspan <- data[c(1,1,1,1), ]
      fullspan$x <- c(origin$x - xtreme, origin$x - xtreme, 
                      origin$x + xtreme, origin$x + xtreme)
      fullspan$y <- c(origin$y - ytreme, origin$y + ytreme, 
                      origin$y - ytreme, origin$y + ytreme)
      fullspan$plot <- FALSE
      rbind(data, fullspan)
    }
    
    ddply(df, c("group", "PANEL"), center)
  }
  
  draw <- function(., data, scales, coordinates, ...) {
    data <- data[data$plot, ]
    data <- data[order(data$theta, data$r), ]
    ggname(.$my_name(), 
           gTree(children = gList(
             with(coord_munch(coordinates, data, scales), 
                  polygonGrob(x, y, default.units = "native",   				
                              gp = gpar(col = colour, fill = alpha(fill, alpha), 
                                        lwd = size * .pt, lty = linetype)
                  )
             )
           ))
    )
  }	
  
  default_stat <- function(.) StatBin
  
  default_aes <- function(.) {
    aes(weight = 1, colour = "grey20", fill = "NA", alpha = NA, 
        linetype = "solid", size = 0.5)
  }
  
  required_aes <- c("x")
  
  guide_geom <- function(.) "polygon"
  
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), 
                         lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, 
                          lineend="butt", lty = linetype))
    ))
  }
  
  new <- function(., mapping = NULL, data = NULL, stat = NULL, 
                  position = NULL, na.rm = FALSE, ...) {
    
    if (!"x" %in% names(mapping)) {
      if (!("angle" %in% names(mapping))) {
        stop("Missing required aesthetics for geom_freqstar: angle", 
          call. = FALSE)
      } else {
        names(mapping)[names(mapping) == "angle"] <- "x"
      }
    } else {
      if ("angle" %in% names(mapping)) {
        mapping$x <- NULL
        names(mapping)[names(mapping) == "angle"] <- "x"
      }
    }
    
    do.call("layer", list(mapping = mapping, data = data, stat = stat, 
      geom = ., position = position, na.rm = na.rm, ...))  
  }
  
})