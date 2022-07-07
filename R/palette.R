#' Colorblind-safe color palettes
#'
#' \code{colorsafe_palette} returns the named color palette.
#'
#' @param palette
#' Name of palette. Run \code{palettes()} to see palette names and hex codes.
#' @param n
#' Number of colors to return. \code{n} may not be greater than the maximum
#' number colors for a given palette and chroma.
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
#' @return
#' A color palette function, palette, or list of palettes (see description).
#' @examples
#' pal <- colorsafe_palette("Paired 6")
#' colorspace::swatchplot(pal)
#' @export
colorsafe_palette <- function(
    palette, n, chroma = c("high","low","variable"), grayscale_safe = FALSE,
    reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  chroma <- match.arg(chroma, several.ok = TRUE)
  palette_list(
    palette, n = n, chroma = chroma, grayscale_safe = grayscale_safe, reverse = reverse,
    luminance_range = luminance_range, max_chroma = max_chroma
  )[[1]]
}

#' @rdname colorsafe_palette
#' @description
#'\code{discrete_palette} returns a palette function for discrete scales.
#' @export
discrete_palette <- function(
    palette, chroma = c("high","low","variable"), grayscale_safe = FALSE,
    reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  function (n) {
    colorsafe_palette(palette, n, chroma, grayscale_safe, reverse, luminance_range, max_chroma)
  }
}

#' @rdname colorsafe_palette
#' @description
#'\code{diverging_palette} returns a two-color diverging palette function.
#' @param hue_pair
#' Name of a diverging palette.
#' @examples
#'
#' pal <- diverging_palette()(5)
#' colorspace::swatchplot(pal)
#' pal <- diverging_palette(reverse = TRUE, luminance_range = c(50,97))(5)
#' colorspace::swatchplot(pal)
#' pal <- diverging_palette("Chartreuse-Magenta", luminance_range = c(40,90))(6)
#' colorspace::swatchplot(pal, cvd = c("deutan","protan"))
#' @export
diverging_palette <- function(
    hue_pair = "Red-Blue", chroma = "variable",
    reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  pals <- unique(palette_master[palette_master$type == "diverging", "palette_name"])
  if (!hue_pair %in% pals)
    stop("Diverging palette not found. Must be one of: %s", paste(pals, collapse = ", "))
  function (n) {
    colorsafe_palette(hue_pair, n, "variable", FALSE, reverse, luminance_range, max_chroma)
  }
}

#' @rdname colorsafe_palette
#' @description
#'\code{qualitative_palette} returns a multi-hue qualitative palette function.
#' @param name
#' Name of a qualitative palette.
#' @examples
#'
#' pal <- qualitative_palette()(5)
#' colorspace::swatchplot(pal)
#' pal <- qualitative_palette("Set 2.5")(5)
#' colorspace::swatchplot(pal)
#' pal <- qualitative_palette(chroma = "low", grayscale_safe = TRUE)(5)
#' colorspace::swatchplot(pal, cvd = c("deutan","protan","desaturate"))
#' @export
qualitative_palette <- function(
  name = "Set 1.5", chroma = c("high","low","variable"), grayscale_safe = FALSE,
  reverse = FALSE
) {
  pals <- unique(palette_master[palette_master$type == "qualitative", "palette_name"])
  if (!name %in% pals)
    stop("Qualitative palette not found. Must be one of: %s", paste(pals, collapse = ", "))
  function (n) {
    colorsafe_palette(name, n, chroma, grayscale_safe, reverse)
  }
}

#' @rdname colorsafe_palette
#' @description
#'\code{sequential_palette} returns a single-hue sequential palette function.
#' @param hue
#' Name of a seqential palette.
#' @examples
#'
#' pal <- sequential_palette()(4)
#' colorspace::swatchplot(pal)
#' pal <- sequential_palette(chroma = "variable", luminance_range = c(50,97))(4)
#' colorspace::swatchplot(pal)
#' pal <- sequential_palette(chroma = "low")(4)
#' colorspace::swatchplot(pal)
#' @export
sequential_palette <- function(
    hue = "Red", chroma = c("high","low","variable"), grayscale_safe = FALSE,
    reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  pals <- unique(palette_master[palette_master$type == "sequential", "palette_name"])
  if (!hue %in% pals)
    stop("Sequential palette not found. Must be one of: %s", paste(pals, collapse = ", "))
  function (n) {
    colorsafe_palette(hue, n, chroma, grayscale_safe, reverse, luminance_range, max_chroma)
  }
}

#' @rdname colorsafe_palette
#' @description
#'\code{palettes} returns a list of available palettes.
#' @param type
#' Type of palette. Must be \code{"qualitative"},  \code{"sequential"}, or
#' \code{"diverging"}.
#' @examples
#'
#' names(palettes(type = "diverging", chroma = "variable"))
#' palettes(5, type = "qualitative", grayscale_safe = TRUE)
#' @export
palettes <- function(
  n, type = c("qualitative","sequential","diverging"), chroma = c("high","low","variable"),
  grayscale_safe = FALSE, reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  type <- match.arg(type, several.ok = TRUE)
  chroma <- match.arg(chroma, several.ok = TRUE)

  pals <- lapply(
    type,
    palette_list,
    name = NULL, n = n, chroma = chroma, grayscale_safe = grayscale_safe, reverse = reverse,
    luminance_range = luminance_range, max_chroma = max_chroma
  )
  names(pals) <- type
  if (length(pals) == 1) {
    pals <- pals[[1]]
  }
  return(pals)
}

palette_list <- function (
    name, type, n, chroma = c("high","low","variable"), grayscale_safe = FALSE, reverse = FALSE,
    luminance_range = c(20, 90), max_chroma = 105
) {
  chroma <- match.arg(chroma, several.ok = TRUE)

  if (!missing(name) && !is.null(name)) {
    pals <- palette_master[palette_master$palette_name %in% name,]
  } else if (!missing(type) && !is.null(type)) {
    type <- match.arg(tolower(type), choices = c("sequential","diverging","qualitative"))
    pals <- palette_master[palette_master$type %in% type,]
  } else {
    stop("'palette_name' or 'type' must be given as input.")
  }
  if (!missing(n) && is.numeric(n)) {
    pals <- pals[pals$max_n >= n,]
  }
  if (identical(grayscale_safe, TRUE)) {
    pals <- pals[pals$is_grayscale_safe,]
  }
  pals <- pals[pals$chroma %in% chroma,]
  if (nrow(pals) == 0)
    stop(sprintf("No palette found (n=%s).", n))

  hex <- list()
  for (i in seq_len(nrow(pals))) {
    if (missing(n)) {
      palette_n <- pals[i, "max_n"]
    } else {
      palette_n <- n
    }
    HCL <- get_hcl(pals[i, "palette_code"], palette_n, reverse, luminance_range, max_chroma)
    if (length(chroma) > 1) {
      name <- paste0(pals[i, "palette_name"], " (", pals[i, "chroma"], ")")
    } else {
      name <- pals[i, "palette_name"]
    }
    hex[[name]] <- hcl(HCL[,"H"], HCL[,"C"], HCL[,"L"], fixup = TRUE)
  }
  return(hex)
}

get_hcl <- function (
    palette_code, n, reverse = FALSE, luminance_range = c(20, 90), max_chroma = 105
) {
  type <- palette_master[palette_master$palette_code == palette_code, "type"]
  if (length(type) == 0)
    stop("palette_code not found.")
  if (n < 2)
    stop("At least 2 colors are required.")

  if (type == "qualitative") {
    pal <- palette_qualitative[
      palette_qualitative$palette_code == palette_code & palette_qualitative$n >= n,
    ]
    pal <- pal[pal$n == min(pal$n),]
    if (nrow(pal) == 0)
      stop(sprintf("Palette not found (n=%s).", n))

    palette_hcl <- cbind(
      H = pal[,"H"],
      C = pmin(pal[,"C"], max_chroma),
      L = pal[,"L"]
    )
  } else {
    pal <- palette_sequential[palette_sequential$palette_code == palette_code,]
    H_n <- length(pal[,"H"])
    L_n <- ceiling(n / H_n)
    min_L <- max(pal$min_L, luminance_range[1])
    max_L <- min(pal$max_L, luminance_range[2])

    H <- rep(pal[,"H"], each = L_n)
    L <- rep(seq(min_L, max_L, length.out = L_n), H_n)
    if (any(is.na(pal$C))) {
      C <- colorspace::max_chroma(H, L, TRUE)
    } else {
      C <- rep(pal$C, each = L_n)
    }

    palette_hcl <- cbind(
      H = H,
      C = pmin(C, max_chroma),
      L = L
    )
    if (type == "diverging") {
      if (L_n * H_n == n) {
        palette_hcl[L_n + 1:L_n,] <- palette_hcl[L_n + L_n:1,]
      } else {
        palette_hcl <- rbind(
          palette_hcl[1:(L_n - 1),],
          cbind(H = 0, C = 0, L = L[L_n]),
          palette_hcl[L_n + (L_n - 1):1,]
        )
      }
    }
  }
  if (identical(reverse, TRUE)) {
    palette_hcl <- palette_hcl[rev(seq_len(nrow(palette_hcl))),]
  }
  return(palette_hcl[1:n,])
}
