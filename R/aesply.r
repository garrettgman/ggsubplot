#' Evaluate aesthetics by group
#'
#' aesply splits a data frame into pieces, evaluates a list of aesthetics within
#' each piece, and combines the results into a new data frame. Each aesthetic in
#' the list must return either a single value per piece or a single value per
#' row in the piece.
#'
#' @keywords internal
#' @param data a data frame
#' @param .vars a vector of variables to split data by. Each element should be
#' the name of a variable in data saved as a character string.
#' @param aesthetics an object of class uneval, usually the output of
#' \code{\link[ggplot2]{aes}}
#' @return a data frame
#' @noRd
aesply <- function(data, .var, aesthetics) {
  no.ply <- unlist(lapply(aesthetics, first_name)) == "I"
  if (!any(no.ply)) {
    return(plyr::compact(eval.plyr(aesthetics, data, .var)))
  }

  all.aes <- lapply(aesthetics[no.ply], remove_I)
  evaled <- plyr::compact(plyr::eval.quoted(all.aes, data))
  lengths <- vapply(evaled, length, integer(1))
  n <- if (length(lengths) > 0) max(lengths) else 0
  wrong <- lengths != 1 & lengths != n
  if (any(wrong)) {
    stop("Aesthetics must either be length one, or the same length as the data",
         "Problems:", paste(aesthetics[wrong], collapse = ", "),
         call. = FALSE)
  }
  data <- cbind(data, evaled)
  all.data <- data.frame(evaled)
  aesthetics[no.ply] <- lapply(names(aesthetics)[no.ply], as.name)
  plyr::compact(eval.plyr(aesthetics, data, .var))
}

#' replace I() with identity
#'
#' remove_I searches through an expression for the \code{\link{I}} function and
#' replaces it with \code{\link{identity}}. remove_I is used when an aesthetic
#' has been surrounded with I() to prevent groupwise calculation.
#' @param expr an expression
#' @noRd
remove_I <- function(expr) {
  Identity <- function(x) {
    if (x == "I") x <- quote(identity)
    x
  }
  as.call(lapply(expr, Identity))
}

#' Evaluate a list of expressions by group
#'
#' Evaluates qoted variables by group in a data frame. Based on
#' \code{\link[plyr]{eval.quoted}}.
#' @keywords internal
#' @param exprs a list of expressions
#' @param data a data frame
#' @param by a vector of character strings that specify variable names in data.
#' data will be split into groups based on the unique combinations of the values
#' of these variables within the data frame. exprs will be evaluated separately
#' for each group.
#' @param enclos an environment
#' @return a data frame formed by combining the results of evaluating exprs in
#' each group of data
#' @noRd
eval.plyr <- function (exprs, data = NULL, by = NULL, enclos = NULL,
  try = FALSE) {
  if (is.numeric(exprs))
    return(exprs)
  qenv <- if (plyr::is.quoted(exprs))
    attr(exprs, "env")
  else parent.frame()
  if (is.null(data))
    data <- qenv
  if (is.data.frame(data) && is.null(enclos))
    enclos <- qenv
  if (try) {
    results <- plyr::failwith(NULL, plyr::ddply, quiet = TRUE) (data, by,
      apply_maps, exprs, qenv)
  } else {
    results <- plyr::ddply(data, by, apply_maps, exprs, qenv)
  }
  results
}

#' Calculate aesthetic values for a data frame
#'
#' apply_maps evaluates a mapping within a data frame to calculate aesthetic
#' values. apply_maps is intended to be used in conjunction with
#' \code{\link[plyr]{ddply}}. If each mapping returns a single value,
#' apply_mapping will return a single row of data. This provides a convenient
#' way to reduce a set of geoms, similar to using \code{\link[plyr]{summarise}}
#' with ddply.
#' @keywords internal
#' @param data a data frame
#' @param mapping an object of class uneval, usually the output of
#' \code{\link[ggplot2]{aes}}
#' @param enclos and environment
#' @return a data frame
#' @noRd
apply_maps <- function(data, mapping, enclos = parent.frame()) {
  map <- null_omit(mapping)
  vars <- plyr::llply(map, eval, envir = data, enclos)
  n <- nrow(data)
  vars <- lapply(vars, condense)
  lengths <- unlist(lapply(vars, length))
  wrong <- lengths != 1 & lengths != n
  if (any(wrong)) {
    stop(paste(
      "Aesthetics must either be length one, or the same length as the data",
      "Problems:", paste(names(wrong)[wrong], collapse = ", ")),
      call. = FALSE)
  }
  data.frame(vars)
}

#' reduce a single valued vector to a single element
#' @keywords internal
#' @param var a vector
#' @return a vector of length one if var only contains one unique value, var
#' otherwise
#' @noRd
condense <- function(var) {
  if (length(unique(var)) == 1) {
    return(unique(var))
  }
  var
}
