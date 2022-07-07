#' Colorblind-safe color scales
#'
#' The \code{colorsafe} scales are colorblind safe and most palettes safely
#' convert to grayscale.
#'
#' @param ...
#' Other arguments passed \code{ggplot2} functions \code{discrete_scale()},
#' \code{continuous_scale()}, or \code{binned_scale()}.
#' @param aesthetic
#' \code{"colour"} or \code{"fill"} aesthetic.
#' @param palette
#' Name of palette. Run \code{\link{palettes}()} to see palette names and hex
#' codes.
#' @param chroma
#' Color chroma based on HCL colorspace. Any of \code{"high"}, \code{"low"},
#' or \code{"variable"}.
#' @param grayscale_safe
#' If \code{TRUE}, palette must be grayscale safe.
#' @param reverse
#' Reverse the order of the color palette.
#' @param luminance_range
#' Minimum and maximum luminance values in HCL colorspace.
#' @param max_chroma
#' Maximum chroma in HCL colorspace. Applicable when
#' \code{chroma = "variable"}.
#' @examples
#' p <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'        geom_tile()
#' p + scale_fill_colorsafe_binned()
#' p + scale_fill_colorsafe_continuous(reverse = TRUE)
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'        geom_point(aes(colour = factor(cyl)), size = 2)
#' p + scale_colorsafe_discrete(palette = "Set 1.4", chroma = "high")
#' @export
scale_colorsafe_continuous <- function (
  ..., aesthetic = "colour", palette = "Red", chroma = "variable", grayscale_safe = FALSE,
  reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  ggplot2::continuous_scale(
    aesthetic, "colorsafe_continuous",
    scales::gradient_n_pal(colorsafe_palette(
      palette = palette,
      chroma = chroma,
      grayscale_safe = grayscale_safe,
      reverse = reverse,
      luminance_range = luminance_range,
      max_chroma = max_chroma)),
    ...
  )
}

#' @rdname scale_colorsafe_continuous
#' @export
scale_color_colorsafe_continuous <- function (
  ..., palette = "Red", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE,
  luminance_range = c(20, 90), max_chroma = 105
) {
  scale_colorsafe_continuous(
    ...,
    aesthetic  = "colour",
    palette    = palette,
    chroma     = chroma,
    grayscale_safe = grayscale_safe,
    reverse    = reverse,
    luminance_range = luminance_range,
    max_chroma = max_chroma
  )
}

#' @rdname scale_colorsafe_continuous
#' @export
scale_fill_colorsafe_continuous <- function (
  ..., palette = "Red", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE,
  luminance_range = c(20, 90), max_chroma = 105
) {
  scale_colorsafe_continuous(
    ...,
    aesthetic  = "fill",
    palette    = palette,
    chroma     = chroma,
    grayscale_safe = grayscale_safe,
    reverse    = reverse,
    luminance_range = luminance_range,
    max_chroma = max_chroma
  )
}

#' @rdname scale_colorsafe_continuous
#' @export
scale_colorsafe_discrete <- function (
  ..., aesthetic = "colour", palette = "Set 1.6", chroma = "low",
  grayscale_safe = FALSE, reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  ggplot2::discrete_scale(
    aesthetic, "colorsafe_discrete",
    discrete_palette(
      palette    = palette,
      chroma     = chroma,
      grayscale_safe = grayscale_safe,
      reverse    = reverse,
      luminance_range = luminance_range,
      max_chroma = max_chroma
    ),
    ...
  )
}

#' @rdname scale_colorsafe_continuous
#' @export
scale_colour_colorsafe_discrete <- function (
  ..., palette = "Set 1.6", chroma = "low", grayscale_safe = FALSE, reverse = FALSE,
  luminance_range = c(20, 90), max_chroma = 105
) {
  scale_colorsafe_discrete(
    ...,
    aesthetic  = "colour",
    palette    = palette,
    chroma     = chroma,
    grayscale_safe = grayscale_safe,
    reverse    = reverse,
    luminance_range = luminance_range,
    max_chroma = max_chroma
  )
}

#' @rdname scale_colorsafe_continuous
#' @export
scale_fill_colorsafe_discrete <- function (
    ..., palette = "Set 1.6", chroma = "low", grayscale_safe = FALSE, reverse = FALSE,
    luminance_range = c(20, 90), max_chroma = 105
) {
  scale_colorsafe_discrete(
    ...,
    aesthetic  = "fill",
    palette    = palette,
    chroma     = chroma,
    grayscale_safe = grayscale_safe,
    reverse    = reverse,
    luminance_range = luminance_range,
    max_chroma = max_chroma
  )
}

#' @rdname scale_colorsafe_continuous
#' @export
scale_colorsafe_binned <- function (
  ..., aesthetic = "colour", palette = "Red-Blue", chroma = "variable", grayscale_safe = FALSE,
  reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  ggplot2::binned_scale(
    aesthetic, "colorsafe_binned",
    scales::gradient_n_pal(colorsafe_palette(
      palette = palette,
      chroma  = chroma,
      grayscale_safe = grayscale_safe,
      reverse = reverse,
      luminance_range = luminance_range,
      max_chroma = max_chroma
      )),
    ...
  )
}

#' @rdname scale_colorsafe_continuous
#' @export
scale_color_colorsafe_binned <- function (
  ..., palette = "Red-Blue", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE,
  luminance_range = c(20, 90), max_chroma = 105
) {
  scale_colorsafe_binned(
    ...,
    aesthetic  = "colour",
    palette    = palette,
    chroma     = chroma,
    grayscale_safe = grayscale_safe,
    reverse    = reverse,
    luminance_range = luminance_range,
    max_chroma = max_chroma
  )
}

#' @rdname scale_colorsafe_continuous
#' @export
scale_fill_colorsafe_binned <- function (
    ..., palette = "Red-Blue", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE,
    luminance_range = c(20, 90), max_chroma = 105
) {
  scale_colorsafe_binned(
    ...,
    aesthetic  = "fill",
    palette    = palette,
    chroma     = chroma,
    grayscale_safe = grayscale_safe,
    reverse    = reverse,
    luminance_range = luminance_range,
    max_chroma = max_chroma
  )
}
