#' Star glyphs
#' 
#' geom_star draws the type of glyph commonly called a star plot, radar plot, 
#' or polar plot.
#' 
#' @param mapping The aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you 
#' are overriding the plot defaults. x and y will determine where the center 
#' of the star should appear. r and angle will determine the polar coordinate 
#' system for the star.
#' @param data A layer specific dataset - only needed if you want to override 
#' the plot defaults
#' @param stat The statistical transformation to use for this layer.
#' @param position The position adjustment to use for overlapping points in this 
#' layer
#' @param na.rm If FALSE (the default), removes missing values with a warning. 
#' If TRUE, silently removes missing variables.
#' @param r.zero logical. Should the origin of the star correspond to r = 0? If 
#' FALSE, origin corresponds to lowest value of r.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This 
#' can include aesthetics whose values you want to set, not map. See 
#' \code{\link[ggplot2]{layer}} for more details.
#' 
#' @section Aesthetics
#' geom_coxcomb understands the following aesthetics: x, y, colour, fill, size, 
#' linetype, weight, and alpha.
#' @examples
#' \dontrun{## A single star
#' one_nasa <- nasa[nasa$id == "1-1", ]
#' ggplot(one_nasa) +
#' geom_star(aes(x = 0, y = 0, r = surftemp, angle = date, 
#' fill = mean(temperature)), r.zero = FALSE)
#' 
#' ## Stars in an embedded plot
#' ggplot(nasa) + 
#' map_americas +
#' geom_subplot(aes(long, lat, group = id,
#' subplot = geom_star(aes(x = 0, y = 0, r = surftemp, 
#' angle = date, fill = mean(surftemp)), r.zero = FALSE))) +
#' coord_map()
#' }
#' @export
geom_star <- function(mapping = NULL, data = NULL, stat = "identity", 
  position = "identity", na.rm = FALSE, r.zero = TRUE, ...) { 

    GeomStar$new(mapping = mapping, data = data, stat = stat, 
      position = position, r.zero = r.zero, ...)
}


GeomStar <- proto::proto(ggplot2:::Geom, {
  objname <- "star"
  # turn cartesian coordinates polar
  reparameterise <- function(., df, params) { 
    # scale x to be between 0 and 2*pi
    df$theta <- unlist(rescale_2pi(df["angle"]))
    df$r <- unlist(rescale_01(df["r"], zero = params$r.zero))
    
    include_origin <- function(data) {
      data <- data[order(data$theta, data$r), ]
      if (data$theta[1] > 0.01) {
        first <- data[1, ]
        first$theta <- 0
        first$r <- 0
        data <- rbind(first, data)
      }
      if (data$theta[length(data$theta)] < 6.27) {
        last <- data[length(data$theta), ]
        last$theta <- 6.28
        last$r <- 0
        data <- rbind(data, last)
      }
      # for centering
      origin <- data[1, ]
      origin$r <- 0
      origin$theta <- 0
      origin$plot <- FALSE
      data$plot <- TRUE
      
      # remove missing points from plot 
      # (otherwise each star will becaome separate polygons)
      data$plot[is.na(data$r) | is.na(data$angle)] <- FALSE
      
      rbind(data, origin)
    }
  
    df <- ddply(df, c("group", "PANEL"), include_origin)
    
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
  
  default_stat <- function(.) StatIdentity
  
  default_aes <- function(.) {
    aes(weight = 1, colour = "grey20", fill = "NA", alpha = NA, 
      linetype = "solid", size = 0.5, r.zero = TRUE)
  }
  
  required_aes <- c("x", "y")
  
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
    position = NULL, na.rm = FALSE,  ...) {
    
    # compute_aesthetics should retunr numeric x and y scales
    # to allow smooth use of the output of reparameterise
    if (!("angle" %in% names(mapping))) {
      if ("x" %in% names(mapping)) {
        names(mapping)[names(mapping) == "x"] <- "angle"
        mapping$x <- 0
      } else {
        stop("Missing required aesthetic for geom_star: angle", call. = F)
      }
    } else {
      if (!("x" %in% names(mapping))) {
        mapping$x <- 0
      }
    }
        
    if (!("r" %in% names(mapping))) {
      if ("y" %in% names(mapping)) {
        names(mapping)[names(mapping) == "y"] <- "r"
        mapping$y <- 0
      } else {
        stop(cat("Missing required aesthetic for geom_star: r\n",
          "do you want geom_freqstar?"), call. = F)
      }
    } else {
      if (!("y" %in% names(mapping))) {
        mapping$y <- 0
      }
    }
    
    do.call("layer", list(mapping = mapping, data = data, stat = stat, 
      geom = ., position = position, na.rm = na.rm, ...)) 
  }
  
})