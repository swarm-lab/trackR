#' @export
pdiff <- function(a, b) {
  nr <- length(a)
  nc <- length(b)

  ma <- matrix(a, nrow = nr, ncol = nc)
  mb <- matrix(b, nrow = nr, ncol = nc, byrow = TRUE)

  ma - mb
}


#' @export
simplerTracker <- function(current, past, maxDist = 10) {
  if (nrow(past) == 0) {
    current$track <- 1:nrow(current)
    return(current)
  }

  frames <- seq(max(past$frame), min(past$frame), -1)

  for (f in frames) {
    if (nrow(past) > 0 & sum(is.na(current$track)) > 0) {
      tmp <- past[past$frame == f, ]

      if (nrow(tmp) > 0) {
        mat <- abs(pdiff(current$x, tmp$x)) + abs(pdiff(current$y, tmp$y))
        mat[!is.na(current$track), ] <- max(mat) * 2

        # if (nrow(mat) > ncol(mat)) {
        #   h <- rep(NA, nrow(mat))
        #   h[as.vector(clue::solve_LSAP(t(mat)))] <- seq(1, ncol(mat))
        # } else {
        #   h <- as.vector(clue::solve_LSAP(mat))
        # }

        h <- RcppHungarian::HungarianSolver(sqrt(mat))$pairs[, 2]
        h[h == 0] <- NA

        valid <- mat[(h - 1) * nrow(mat) + 1:nrow(mat)] <= (maxDist * (current$frame[1] - f))
        h[!valid] <- NA
        current$track[!is.na(h)] <- tmp$track[h[!is.na(h)]]

        past <- past[past$frame != f, ]
        past <- past[!(past$track %in% tmp$track[h]), ]
      }
    } else {
      break()
    }
  }

  current
}


#' @export
applyShiny <- function(X, MARGIN, FUN, message = NULL, inc = 0.1, ...) {
  shiny::withProgress(message = message, value = 0, {
    unit <- 1 / prod(dim(X)[MARGIN])
    trigger <- 0

    apply(X, MARGIN, function(x) {
      trigger <<- trigger + unit
      if (trigger >= inc) {
        shiny::incProgress(inc)
        trigger <<- 0
      }

      FUN(x, ...)
    })

  })
}
