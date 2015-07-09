#' Compute aesthetics groupwise
#'
#' ply_aes causes the aesthetics of a layer to be computed groupwise. ply_aes
#' implements the split-apply-combine strategy of data analysis in a graphical
#' framework. It first splits a layer's data frame into subgroups, then evaluates
#' the layers mappings separately within each group, and finally combines the
#' results into a single data frame which is used to build the plot for
#' rendering.
#'
#' Users may specify which groupings to use through the .vars
#' argument. If this argument is left NULL, ply_aes will search for and use a
#' group aes, a glyphing or gridding criteria (in a sp_layer), a facetting
#' criteria, or any combination of these that it finds.
#'
#' @param layer a ggplot2 layer or sp_layer object. This layer's aesthetics will
#' be computed groupwise, but the layer will remain the same in every other
#' respect.
#' @param .vars variable names to group by (optional), stored as a character
#' string
#' @noRd
ply_aes <- function(layer, .vars = NULL) {
  UseMethod("ply_aes")
}

#' @export
ply_aes.list <- function(layer, .vars = NULL) {
  lapply(layer, ply_aes, .vars)
}

#' @export
ply_aes.sp_layer <- function(layer, .vars = NULL) {
  if (!is.null(.vars)) {
    layer$plyr <- list(ply.by = .vars)
  } else {
    layer$plyr <- list()
  }
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}

#' @export
ply_aes.proto <- function(layer, .vars = NULL) {
  if (!is.null(.vars)) {
    layer$plyr <- list(ply.by = .vars)
  } else {
    layer$plyr <- list()
  }
  layer$compute_aesthetics <- plyr_aesthetics
  layer
}


# compute_aesthetics groupwise at build
#
# plyr_aesthetics replaces a layer's compute_aesthetics method when
# \code{\link{ply_aes}} is called. This results in aesthetics being computed
# groupwise during \code{\link[ggplot2]{ggplot_build}}
plyr_aesthetics <- function (., data, plot) {
  aesthetics <- .$layer_mapping(plot$mapping)
  if (!is.null(.$subset)) {
    include <- data.frame(plyr::eval.quoted(.$subset, data, plot$env))
    data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
  }
  if (!is.null(.$geom_params$group)) {
    aesthetics["group"] <- .$geom_params$group
  }

  f <- get("scales_add_defaults", envir = asNamespace("ggplot2"))
  f(plot$scales, data, aesthetics, plot$plot_env)

  if (!is.null(aesthetics$group)) {
    data$group <- unlist(eval(aesthetics$group, envir = data,
      enclos = plot$plot_env))
    aesthetics$group <- quote(group)
  }
  if ("GLYPH" %in% names(data)) {
    aesthetics$GLYPH <- quote(GLYPH)
  }
  aesthetics$PANEL <- quote(PANEL)

  criteria <- c("group", "GLYPH", "PANEL", .$plyr$ply.by)
  criteria <- criteria[criteria %in% names(data)]
  data$ply.by <- plyr::id(data[criteria], drop = TRUE)

  data <- aesply(data, "ply.by", aesthetics)
  data$ply.by <- NULL
  data
}
