#' rescale vectors to [0,1]
#' 
#' rescale_01 rescales every vector in a list of vectors to the range [0, 1]. 
#' rescale_01 rescales the vectors as a group (instead of rescaling each vector 
#' independently). This is a helpful feature for rescaling related variables (such 
#' as xmin and xmax) without nullifying the difference between the two.
#' 
#' If a vector is a character or factor vector, rescale_01 attempts to coerce it 
#' to numeric before scaling. The scale is determined by finding the range of 
#' values contained in the list of vectors and mapping it ot [0, 1]. 
#' 
#' If the full range of values to be scaled is not present in the vectors, users 
#' can specify the range to be scaled to [0,1] with the xlim argument. Values in 
#' the vectors will be rescaled as if they according to this range.
#' 
#' @aliases free
#' @param xvars a list of vectors
#' @param xlim NULL (default) or a numeric vector of length two that specifies 
#' the range of values to scale to [0, 1]
#' @param zero logical. Should zero be added to the range before rescaling?
#' @return a list of vectors
#' @seealso \code{\link{rescale_11}}, \code{\link{rescale_2pi}}
#' @export rescale_01 free
rescale_01 <- free <- function(xvars, xlim=NULL, zero=FALSE) {
	xnames <- names(xvars)
	numberfy <- function(x) {
		if (is.character(x)) {
			x <- as.numeric(factor(x))
		}	
		if (is.factor(x)) {
			x <- as.numeric(x)
		}
    if (inherits(x, "POSIXt")) {
      x <- as.numeric(x)
    }
		x
	}
	xvars <- lapply(xvars, numberfy)
  
	if (is.null(xlim)) {
		rng <- range(unlist(xvars), na.rm = TRUE)
	} else {
		rng <- xlim
	}
  
  if (zero) {
    rng <- c(min(rng[1], 0), max(rng[2], 0))
  }
	
	scale <- function(x) {
		if ((rng[2] - rng[1]) == 0) {
			x - rng[1]
		} else {	
			(x - rng[1]) / (rng[2] - rng[1])
		}
	}
	xvars <- lapply(xvars, scale)
	names(xvars) <- xnames
	data.frame(xvars)
}

#' rescale vectors to [-1,1]
#' 
#' rescale_11 rescales every vector in a list of vectors to the range [-1, 1]. 
#' rescale_11 rescales the vectors as a group (instead of rescaling each vector 
#' independently). This is a helpful feature for rescaling related variables (such 
#' as xmin and xmax) without nullifying the difference between the two.
#' 
#' If a vector is a character or factor vector, rescale_11 attempts to coerce it 
#' to numeric before scaling. The scale is determined by finding the range of 
#' values contained in the list of vectors and mapping it ot [-1, 1]. 
#' 
#' If the full range of values to be scaled is not present in the vectors, users 
#' can specify the range to be scaled to [-1,1] with the xlim argument. Values in 
#' the vectors will be rescaled as if they according to this range.
#' 
#' @param xvars a list of vectors
#' @param xlim NULL (default) or a numeric vector of length two that specifies 
#' the range of values to scale to [-1, 1]
#' @param zero logical. Should zero be added to the range before rescaling?
#' @return a list of vectors
#' @seealso \code{\link{rescale_01}}, \code{\link{rescale_2pi}}
#' @export
rescale_11 <- function(xvars, xlim=NULL, zero=FALSE) {
	2 * rescale_01(xvars, xlim, zero) - 1
}

#' rescale vectors to [0, 2 * pi]
#' 
#' rescale_2pi rescales every vector in a list of vectors to the range 
#' [0, 2 * pi] (e.g, for working with radians). rescale_2pi rescales the vectors 
#' as a group (instead of rescaling each vector independently). This is a 
#' helpful feature for rescaling related variables (such as xmin and xmax) 
#' without nullifying the difference between the two.
#' 
#' If a vector is a character or factor vector, rescale_11 attempts to coerce it 
#' to numeric before scaling. The scale is determined by finding the range of 
#' values contained in the list of vectors and mapping it to [0, 2 * pi]. 
#' 
#' If the full range of values to be scaled is not present in the vectors, users 
#' can specify the range to be scaled to [0, 2 * pi] with the xlim argument. Values in 
#' the vectors will be rescaled as if they according to this range.
#' 
#' @param xvars a list of vectors
#' @param xlim NULL (default) or a numeric vector of length two that specifies 
#' the range of values to scale to [0, 2 * pi]
#' @param zero logical. Should zero be added to the range before rescaling?
#' @return a list of vectors
#' @seealso \code{\link{rescale_01}}, \code{\link{rescale_11}}
#' @export
rescale_2pi <- function(xvars, xlim = NULL, zero = FALSE) {
  2 * pi * rescale_01(xvars, xlim, zero)
}

