applyShiny <- function(X, MARGIN, FUN, message = NULL, inc = 0.1, ...) {
  withProgress(message = message, value = 0, {
    unit <- 1 / prod(dim(X)[MARGIN])
    trigger <- 0

    apply(X, MARGIN, function(x) {
      trigger <<- trigger + unit
      if (trigger >= inc) {
        incProgress(inc)
        trigger <<- 0
      }

      FUN(x, ...)
    })

  })
}
