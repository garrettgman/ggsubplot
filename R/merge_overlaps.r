#' Identify and merge overlapping subplot assignments
#'
#' merge_overlaps checks subplot positions against subplot width and heights to
#' identify groups of overlapping subplots. It then computes an alternative
#' SUBPLOT variable that assigns all subplots in an overlapping group to the
#' same name.
#'
#' @keywords internal
#' @param globals a data frame of subplot names and positions
#' @param width subplot width in the same units as the global x positions
#' @param height subplot height in the same units as global y positions
#' @return A named vector The names of the vector correspond to old subplot
#' assignments, the values correspond to new assignments that merge overlapping
#' subplots.
#' @noRd
merge_overlaps <- function(globals, width, height) {
  x.overlaps <- abs(outer(globals$x, globals$x, "-")) < width
  y.overlaps <- abs(outer(globals$y, globals$y, "-")) < height
  overlaps <- data.frame(x.overlaps & y.overlaps)
  names(overlaps) <- as.character(globals$SUBPLOT)

  for (i in seq_along(overlaps)) {
    names(overlaps)[overlaps[[i]]] <- names(overlaps)[i]
  }
  vec <- as.numeric(factor(names(overlaps)))
  names(vec) <- globals$SUBPLOT
  vec
}
