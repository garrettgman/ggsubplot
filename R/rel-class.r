#' Make a rel class object
#' 
#' rel class objects are used to specify the width and height of glyphs in 
#' \code{\link{geom_subplot}} calls. The numeric component of the rel object specifies 
#' the proportion of the relative width or height to use for the final width or 
#' height. The relative width or height of a glyph is calculated at the time a 
#' plot is built for rendering. It depends on the number of glyphs in a plot and 
#' their placement within the plot.
#' 
#' @param x numeric the proportion of the relative width or height to use as 
#' the final width or height
#' @return an object of class rel
#' @export
rel <- function(x) {
  structure(x, class = "rel")
}

#' @S3method print rel
print.rel <- function(x, ...) print(noquote(paste(x, " *", sep = "")))

#' is x a rel object?
#' @param x an R object
#' @return logical
#' @export
is.rel <- function(x) inherits(x, "rel")