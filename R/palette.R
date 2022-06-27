#' @export
colorsafe_palette <- function(
  name, n, chroma = c("high","low","variable"), grayscale_safe = FALSE, reverse = FALSE, ...
) {
  chroma <- match.arg(chroma, several.ok = TRUE)
  palette_list(
    name, n = n, chroma = chroma, grayscale_safe = grayscale_safe, reverse = reverse, ...
  )[[1]]
}

#' @export
colorsafe <- function(
  name, chroma = c("high","low","variable"), grayscale_safe = FALSE, reverse = FALSE, ...
) {
  function (n) {
    colorsafe_palette(name, n, chroma, grayscale_safe, reverse, ...)
  }
}

#' @export
palettes <- function(
  n, type = c("sequential","diverging","qualitative"), chroma = c("high","low","variable"),
  grayscale_safe = FALSE, reverse = FALSE, ...
) {
  type <- match.arg(type, several.ok = TRUE)
  chroma <- match.arg(chroma, several.ok = TRUE)

  pals <- lapply(
    type,
    palette_list,
    name = NULL, n = n, chroma = chroma, grayscale_safe = grayscale_safe, reverse = reverse, ...
  )
  names(pals) <- type
  if (length(pals) == 1) {
    pals <- pals[[1]]
  }
  return(pals)
}

palette_list <- function (
    name, type, n, chroma = c("high","low","variable"), grayscale_safe = FALSE, reverse = FALSE, ...
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
    stop("No palette found.")

  hex <- list()
  for (i in seq_len(nrow(pals))) {
    if (missing(n)) {
      palette_n <- pals[i, "max_n"]
    } else {
      palette_n <- n
    }
    HCL <- get_hcl(pals[i, "palette_code"], palette_n, reverse, ...)
    if (length(chroma) > 1) {
      name <- paste0(pals[i, "palette_name"], " (", pals[i, "chroma"], ")")
    } else {
      name <- pals[i, "palette_name"]
    }
    hex[[name]] <- hcl(HCL[,"H"], HCL[,"C"], HCL[,"L"], fixup = FALSE)
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
      stop("Palette not found.")

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
