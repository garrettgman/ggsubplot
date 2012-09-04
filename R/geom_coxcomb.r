#' Coxcomb glyphs
#' 
#' geom_coxcomb draws the type of glyph commonly called a coxcomb plot or polar 
#' area plot, popularized by Florence Nightingale.
#' 
#' @param mapping The aesthetic mapping, usually constructed with 
#' \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you 
#' are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override 
#' the plot defaults
#' @param stat The statistical transformation to use for this layer.
#' @param position The position adjustment to use for overlapping points in this 
#' layer
#' @param npoints the number of points to use when drawing the arcs with line 
#' segments. Defaults to 10.
#' @param na.rm If FALSE (the default), removes missing values with a warning. 
#' If TRUE, silently removes missing variables.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This 
#' can include aesthetics whose values you want to set, not map. See 
#' \code{\link[ggplot2]{layer}} for more details.
#' 
#' @section Aesthetics
#' geom_coxcomb understands the following aesthetics: x, y, colour, fill, size, 
#' linetype, weight, and alpha.
#' @examples
#' \dontrun{## A single coxcomb
#' ggplot(casualties) +
#' geom_coxcomb(aes(angle = month, fill = month))
#'
#' ## Coxcombs in an embedded plot
#' ggplot(casualties) + 
#' map_afghanistan +
#' geom_subplot2d(aes(lon, lat, 
#' subplot = geom_coxcomb(aes(angle = month, fill = month))),
#' bins = c(15, 12), ref = NULL) +
#' coord_map()
#' }
#' @export
geom_coxcomb <- function(mapping = NULL, data = NULL, stat = "bin", 
  position = "identity", npoints = 10, na.rm = FALSE, ...) { 
  
    GeomCoxcomb$new(mapping = mapping, data = data, stat = stat, 
      position = position, npoints = npoints, na.rm = na.rm, ...)
}

GeomCoxcomb <- proto::proto(ggplot2:::Geom, {
  objname <- "coxcomb"
  
  default_stat <- function(.) StatBin
  default_pos <- function(.) PositionIdentity
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, 
    weight = 1, alpha = NA)
  
  required_aes <- c("x")
  
  reparameterise <- function(., df, params) {

    # scale bin breaks to 0 and 2 *pi
    width <- ggplot2::resolution(unique(df$x), FALSE)
    xbreaks.min <- df$x - width/2
    xbreaks.max <- df$x + width/2
    xbreaks <- unlist(rescale_2pi(list(c(xbreaks.min, xbreaks.max))))
    n <- length(xbreaks.min)
    df$xmin <- zero_wrap(xbreaks[1:n])
    df$xmax <- xbreaks[(n + 1):(n + n)]
      
    # scale y to r to create equal areas
    adjust_y <- function(df) {
      if (length(df$y) == 1) {
        df$ymin <- 0
        df$ymax <- df$y
        return(df)
      }
      span <- df$xmax[1] - df$xmin[1]
      df$y[1] <- sqrt(2 / span * df$y[1])
      for ( i in 2:length(df$y)) {
        df$y[i] <- sqrt(2 / span * df$y[i - 1])
      }
      df$ymax <- cumsum(df$y)
      df$ymin <- c(0, df$ymax[-length(df$y)])
      df
    }
    df <- ddply(df, c("x", "PANEL"), adjust_y)

    df$section <- id(df[c("x", "group")], drop = TRUE)
    
    # create polygon points
    poly_curve <- function(df, npoints) {
      non.pos <- setdiff(names(df), c(.x_aes, .y_aes, "count", "ndensity", 
        "ncount", "density"))
      theta <- seq(df$xmin, df$xmax, length = npoints)
      theta <- c(theta, theta[length(theta):1])
      r <- c(rep(df$ymin, npoints), rep(df$ymax, npoints))
      x <- r*cos(theta)
      y <- r*sin(theta)
      df <- df[1, non.pos]
      row.names(df) <- NULL
      df <- cbind(df, x, y)
      rbind(df, df[1, ])
    }
    df <- ddply(df, c("section", "group", "PANEL"), poly_curve, params$npoints)
  # ensure that (0,0) is plotted in center of graph
  center <- function(data) {
    # origin always (0, 0)?
    xtreme <- max(abs(data$x))
    ytreme <- max(abs(data$y))
    fullspan <- data[c(1,1,1,1), ]
    fullspan$x <- c(-xtreme, -xtreme, xtreme, xtreme)
    fullspan$y <- c(-ytreme, ytreme, -ytreme, ytreme)
    fullspan$plot <- FALSE
    data$plot <- TRUE
    rbind(data, fullspan)
  }
    
  ddply(df, c("group", "PANEL"), center)
  }  
  draw <- draw_groups <- function(., data, scales, coordinates, ...) {
    data <- data[data$plot, ]
    polys <- dlply(data, c("section", "group", "PANEL"), function(df) {
      ggname("polygon", gTree(children=gList(
        with(coord_munch(coordinates, df, scales), 
             polygonGrob(x, y, default.units="native",
                         gp=gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt,
                                 lty=linetype))
        )
      )))
    })
    ggname("coxcomb", do.call("grobTree", polys))               
  }
  
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
    position = NULL, npoints = 10, na.rm = FALSE, ...) {
    
    missing <- !(c("angle") %in% names(mapping))
    if (any(missing)) {
      stop("Missing required aesthetic: angle", call. = FALSE)
    }
    names(mapping)[names(mapping) == "angle"] <- "x"
    if ("r" %in% names(mapping)) {
      names(mapping)[names(mapping) == "r"] <- "y"
    }
    mapping$section <- coxcomb_sections(mapping)
    
    lyr <- do.call("layer", list(mapping = mapping, data = data, stat = stat, 
      geom = ., position = position, na.rm = na.rm, ...))  
    lyr$geom_params$npoints <- npoints
    lyr
  }
  
})

#' coxcomb_sections ensures that continuous fill, alpha, and colour variables 
#' generate groups at build time as if they were discrete
#' 
#' @keywords internal
#' @param mapping an aesthetic mapping, usually created with 
#' \code{\link[ggplot2]{aes}}
#' @export
coxcomb_sections <- function(mapping) {
  sections <- mapping[c("alpha", "fill", "colour")]
  sections <- plyr::compact(sections)
  names(sections) <- NULL
  if (length(sections) == 0) return(NULL)
  as.call(c(quote(interaction), sections))
}

# changes 2 * pi into 0
zero_wrap <- function(vec){
  vec[vec > 6.2831] <- 0
  vec
}