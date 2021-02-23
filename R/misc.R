.pdiff <- function(a, b) {
  a - matrix(b, nrow = length(a), ncol = length(b), byrow = TRUE)
}

#' @title A Simple Tracker
#'
#' @description Given a set of current x/y positions, this function attempts to
#'  find which trajectory they belong to in a set of past tracked positions
#'  using the Hungarian method.
#'
#' @param current A data frame with at least 4 columns: x, y, frame, and track.
#'
#' @param past A data frame with at least 4 columns: x, y, frame, and track.
#'
#' @param maxDist The maximum distance between two successive positions
#'  belonging to the same trajectory.
#'
#' @return A data frame with at least 4 columns: x, y, frame, and track.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
simplerTracker <- function(current, past, maxDist = 10) {
  if (nrow(past) == 0) {
    current$track <- NA
    return(current)
  }

  frames <- seq(max(past$frame), min(past$frame), -1)

  for (f in frames) {
    if (nrow(past) > 0 & sum(is.na(current$track)) > 0) {
      tmp <- past[past$frame == f, ]

      if (nrow(tmp) > 0) {
        mat <- abs(.pdiff(current$x, tmp$x)) + abs(.pdiff(current$y, tmp$y))
        mat[!is.na(current$track), ] <- max(mat) * 2

        if (nrow(mat) > ncol(mat)) {
          h <- rep(NA, nrow(mat))
          h[as.vector(clue::solve_LSAP(t(sqrt(mat))))] <- seq(1, ncol(mat))
        } else {
          h <- as.vector(clue::solve_LSAP(sqrt(mat)))
        }

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


.dist2ellipse <- function(x, y, cx, cy, width, height, angle) {
  relx <- -.pdiff(cx, x)
  rely <- -.pdiff(cy, y)
  cosa <- cos(-angle)
  sina <- sin(-angle)
  sqrt(((relx * cosa - rely * sina) / (width / 2)) ^ 2 +
         ((relx * sina + rely * cosa) / (height / 2)) ^ 2)
}

#' @title Fit an Ellipse
#'
#' @description Given a set of x/y positions, this function attempts to find the
#'  best fitting ellipse that goes through these points.
#'
#' @param x,y Vectors of x and x positions.
#'
#' @return A vector with 5 elements: the x and y coordinated of the center of
#'  the ellipse, the width and height of the ellipse, and the angle of the
#'  ellipse relative to the y axis.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' optimEllipse(rnorm(100), rnorm(100))
#'
#' @export
optimEllipse <- function(x, y) {
  d <- Rfast::Dist(cbind(x, y))
  start <- c(mean(x), mean(y), max(d), max(d), 0)

  opt <- stats::optim(start, function(par) {
    sum((.dist2ellipse(x, y, par[1], par[2], par[3], par[4], par[5]) - 1) ^ 2)
  }, method = "L-BFGS-B",
  lower = c(min(x), min(y), -Inf, -Inf, -pi),
  upper = c(max(x), max(y), Inf, Inf, pi))

  out <- opt$par
  out[5] <- 180 * out[5] / pi
  out
}


.xyangle <- function(x, y, directed = FALSE) {
  if (missing(y)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  out <- atan2(y, x)
  if (!directed)
    out <- out%%pi
  out
}

#' @title Points on an Ellipse
#'
#' @description This functions computes \code{npoints} regularly spaced along an
#'  ellipse.
#'
#' @param x,y Numeric values corresponding to the coordinates of the center of
#'  the ellipse.
#'
#' @param width,height Numeric values corresponding to the width and height of
#'  the ellipse.
#'
#' @param angle Numeric value corresponding to the angle of the ellipse relative
#'  to the y axis.
#'
#' @param npoints The number of points to compute.
#'
#' @return A matrix with two columns corresponding to the x and y coordinates of
#'  the points.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' plot(ellipse(0, 0, 30, 50, 33), asp = 1)
#'
#' @export
ellipse <- function(x, y, width, height, angle, npoints = 100) {
  angle <- angle * pi / 180
  segment <- c(0, 2 * pi)
  z <- seq(segment[1], segment[2], length = npoints + 1)
  xx <- (width / 2) * cos(z)
  yy <- (height / 2) * sin(z)
  alpha <- .xyangle(xx, yy, directed = TRUE)
  rad <- sqrt(xx^2 + yy^2)
  cbind(x = rad * cos(alpha + angle) + x,
        y = rad * sin(alpha + angle) + y)
}


.cov2shape <- function(sigma, mu) {
  eig <- eigen(sigma, symmetric = TRUE)
  eigval <- eig$values
  eigvec <- eig$vectors
  eigidx <- order(eigval)

  if (eigidx[1] == 1) {
    a <- 2 * sqrt(eigval[2])
    b <- 2 * sqrt(eigval[1])
  } else {
    a <- 2 * sqrt(eigval[1]);
    b <- 2 * sqrt(eigval[2]);
  }

  alpha <- atan(eigvec[2, 1] / eigvec[2, 2])

  c(x = mu[1], y = mu[2], width = b * 2, height = a * 2,
    angle = (180 * alpha / pi) + 90)
}


#' @title Customized Cross-Entropy Clustering
#'
#' @description This function performs cross-entropy clustering on a data matrix.
#'  It is based on \code{\link[CEC]{cec}} but is limited to 2D matrices and
#'  implements its own splitting process.
#'
#' @param x A numeric matrix with two columns.
#'
#' @param centers Either a matrix of initial centers or the number of initial
#'  centers.
#'
#' @param iter.max Maximum number of iterations at each clustering.
#'
#' @param split Enables split mode. This mode discovers new clusters after
#'  initial clustering, by trying to split single clusters into two.
#'
#' @param split.width The maximum authorized width of a cluster. If a cluster is
#'  wider than \code{split.width}, the function will attempt to split it in two.
#'
#' @param split.height The maximum authorized height of a cluster. If a cluster
#'  is higher than \code{split.height}, the function will attempt to split it in
#'  two.
#'
#' @param split.density The minimum authorized density of a cluster. If a
#'  cluster is less dense than \code{split.density}, the function will attempt
#'  to split it in two.
#'
#' @param min.size The minimum authorized size (in number of items) of a cluster.
#'  If a cluster is smaller than \code{min.size}, the function will attempt to
#'  split it in two.
#'
#' @param split.sensitivity The minimum amount of improvement in the cost
#'  function of the cross-entropy clustering for a splitting event to be
#'  considered valid.
#'
#' @return A matrix with 6 columns: x and y coordinates of the centers of the
#'  clusters, width, height, and angle of the covariance ellipse best describing
#'  each cluster, and the number of element in each cluster.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' x <- c(rnorm(25, 4), rnorm(25, -2))
#' y <- c(rnorm(25, 2), rnorm(25, -3))
#' k <- kbox(cbind(x, y), 2)
#' plot(x, y, asp = 1)
#' apply(k, 1, function(k) {
#'   lines(ellipse(k[1], k[2], k[3], k[4], k[5]))
#' })
#'
#' @export
kbox <- function(x, centers = 1, iter.max = 10, split = FALSE, split.width = Inf,
                 split.height = Inf, split.density = 0, min.size = 0,
                 split.sensitivity = 0) {
  if (!is.matrix(x))
    x <- as.matrix(x)

  if (is.null(dim(centers))) {
    cl <- tryCatch(
      CEC::cec(x, centers, iter.max = iter.max, card.min = min.size,
               nstart = 10, threads = "auto", param = NULL),
      error = function(cond) {
        NULL
      }
    )
  } else {
    if (!is.matrix(centers))
      centers <- as.matrix(centers)

    cl <- tryCatch(
      CEC::cec(x, centers, iter.max = iter.max, card.min = min.size, param = NULL),
      error = function(cond) {
        NULL
      }
    )
  }

  if (is.null(cl)) {
    split <- FALSE
  } else {
    sh <- rbind(
      mapply(.cov2shape, cl$covariances, asplit(cl$centers, 1), SIMPLIFY = TRUE),
      n = Rfast::Table(cl$cluster)
    )
  }

  while (split) {
    test_width <- sh[3, ] / split.width
    test_height <- sh[4, ] / split.height
    test_density <- split.density / (sh[6, ] / ((sh[3, ] / 2) *
                                                  (sh[4, ] / 2) * pi))
    to_split <- test_width > 1 | test_height > 1 | test_density > 1

    if (any(to_split)) {
      tmp_cl <- CEC::cec(x, cl$nclusters, iter.max = iter.max, param = NULL,
                         card.min = min.size, nstart = 10, threads = "auto")
      tmp_sh <- rbind(
        mapply(.cov2shape, tmp_cl$covariances, asplit(tmp_cl$centers, 1),
               SIMPLIFY = TRUE),
        n = Rfast::Table(tmp_cl$cluster)
      )

      tmp_test_width <- tmp_sh[3, ] / split.width
      tmp_test_height <- tmp_sh[4, ] / split.height
      tmp_test_density <- split.density / (tmp_sh[6, ] / ((tmp_sh[3, ] / 2) *
                                                            (tmp_sh[4, ] / 2) * pi))

      if (any(tmp_test_width > 1 | tmp_test_height > 1 | tmp_test_density > 1)) {
        tmp_tmp_centers <- rbind(
          cl$centers[!to_split, ],
          do.call(rbind, lapply(which(to_split), function(i) {
            cluster::clara(x[cl$cluster == i, ], 2, samples = 50)$medoids
          }))
        )

        tmp_tmp_cl <- tryCatch(
          CEC::cec(x, tmp_tmp_centers, iter.max = iter.max, card.min = min.size,
                   param = NULL),
          error = function(cond) {
            cl
          }
        )

        if (1 - tmp_tmp_cl$cost.function / cl$cost.function >
            split.sensitivity / sqrt(tmp_tmp_cl$nclusters)) {
          cl <- tmp_tmp_cl
          sh <- rbind(
            mapply(.cov2shape, tmp_tmp_cl$covariances, asplit(tmp_tmp_cl$centers, 1), SIMPLIFY = TRUE),
            n = Rfast::Table(tmp_tmp_cl$cluster)
          )
        } else {
          split <- FALSE
        }
      } else {
        cl <- tmp_cl
        sh <- tmp_sh
        split <- FALSE
      }
    } else {
      split <- FALSE
    }
  }

  if (is.null(cl)) {
    NULL
  } else {
    t(sh)
  }
}


#' @title Fast Approximate Minimum Volume Enclosing Ellipsoid
#'
#' @description Given a set of current x/y positions, this function computes an
#'  approximation of their minimum volume enclosing ellipsoid.
#'
#' @param x A matrix with two columns corresponding to the x and y positions for
#'  which to compute the ellipsoid.
#'
#' @return A vector with 5 elements: the x and y coordinated of the center of
#'  the ellipsoid, the width and height of the ellipsoid, and the angle of the
#'  ellipsoid relative to the y axis.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' x <- rnorm(25, 4)
#' y <- rnorm(25, 2)
#' ell <- amvee(cbind(x, y))
#' plot(x, y, asp = 1)
#' lines(ellipse(ell[1], ell[2], ell[3], ell[4], ell[5]))
#'
#' @export
amvee <- function(x) {
  chull <- Rvision::convexHull(x[, 1], x[, 2])

  if (length(chull) < 5) {
    ell <- Rvision::minAreaRect(x[chull, 1], x[chull, 2])
  } else {
    ell <- Rvision::fitEllipse(x[chull, 1], x[chull, 2])
  }

  c(x = ell$center[1], y = ell$center[2], width = ell$width,
    height = ell$height, angle = ell$angle)
}
