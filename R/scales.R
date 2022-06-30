scale_fill_colorsafe_continuous <- function (
    hue = "Red", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE, ...
) {
    ggplot2::continuous_scale(
      "fill", "colorsafe_continuous",
      scales::gradient_n_pal(colorsafe_palette(
        name   = hue,
        chroma = chroma,
        grayscale_safe = grayscale_safe,
        reverse = reverse,
        ...)),
      ...
    )
}

scale_colour_colorsafe_continuous <- function (
  ..., hue = "Red", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE
) {
  ggplot2::continuous_scale(
    "colour", "colorsafe_continuous",
    scales::gradient_n_pal(colorsafe_palette(
      name   = hue,
      chroma = chroma,
      grayscale_safe = grayscale_safe,
      reverse = reverse,
      ...)),
    ...
  )
}

scale_color_colorsafe_continuous <- function (
  ..., hue = "Red", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE
) {
  scale_colour_colorsafe_continuous(
    ...,
    hue     = hue,
    chroma  = chroma,
    grayscale_safe = grayscale_safe,
    reverse = reverse
  )
}

scale_fill_colorsafe_discrete <- function (
  ..., palette = "Set 1.5", chroma = "high", grayscale_safe = FALSE, reverse = FALSE
) {
  ggplot2::discrete_scale("fill", "colorsafe_discrete", colorsafe(
    palette = palette,
    chroma  = chroma,
    grayscale_safe = grayscale_safe,
    reverse = reverse,
    ...
  ), ...)
}

scale_colour_colorsafe_discrete <- function (
  ..., palette = "Set 1.5", chroma = "high", grayscale_safe = FALSE, reverse = FALSE
) {
  ggplot2::discrete_scale("colour", "colorsafe_discrete", colorsafe(
    palette = palette,
    chroma  = chroma,
    grayscale_safe = grayscale_safe,
    reverse = reverse,
    ...
  ), ...)
}

scale_color_colorsafe_discrete <- function (
  ..., name = "Set 1.5", chroma = "high", grayscale_safe = FALSE, reverse = FALSE
) {
  scale_colour_colorsafe_discrete(
    ...,
    palette = palette,
    chroma  = chroma,
    grayscale_safe = grayscale_safe,
    reverse = reverse,

  )
}

scale_fill_colorsafe_binned <- function (
  ..., hue = "Red", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE
) {
  ggplot2::binned_scale(
    "fill", "colorsafe_binned",
    scales::gradient_n_pal(colorsafe_palette(
      palette = hue,
      chroma  = chroma,
      grayscale_safe = grayscale_safe,
      reverse = reverse,
      ...)),
    ...
  )
}

scale_colour_colorsafe_binned <- function (
  ..., hue = "Red", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE
) {
  ggplot2::binned_scale(
    "colour", "colorsafe_binned",
    scales::gradient_n_pal(colorsafe_palette(
      palette = hue,
      chroma  = chroma,
      grayscale_safe = grayscale_safe,
      reverse = reverse,
      ...)),
    ...
  )
}

scale_color_colorsafe_binned <- function (
  ..., hue = "Red", chroma = "variable", grayscale_safe = FALSE, reverse = FALSE
) {
  scale_colour_colorsafe_binned(
    hue     = hue,
    chroma  = chroma,
    grayscale_safe = grayscale_safe,
    reverse = reverse,
    ...
  )
}
