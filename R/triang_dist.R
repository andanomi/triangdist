#' Triangular Distribution Density
#'
#' Computes the probability density function (PDF) of triangular distribution.
#'
#' @param x A numeric vector of quantiles.
#' @param min The lower limit of the distribution.
#' @param max The upper limit of the distribution.
#' @param mode The mode (peak) of the distribution.
#' @return A numeric vector containing the density values
#' @examples
#' dtriang(0.5, 0, 1, 0.5)
#' dtriang(c(0.2, 0.5, 0.8), 0, 1, 0.5)
#' dtriang(-1, 0, 1, 0.5)  # fuera de rango, devuelve 0
#' @export
dtriang <- function(x, min, max, mode) {
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: 'min' cannot be greater than 'max'")
  }

  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: 'mode' cannot be out of '[min,max]'")
  }
  ifelse(x < min | x > max, 0,
    ifelse(min <= x & x <= mode,
      (2 * (x - min)) / ((max - min) * (mode - min)),
      (2 * (max - x)) / ((max - min) * (max - mode))
    )
  )
}

#' Cumulative Distribution
#'
#' Computes the cumulative probability (CDF) of the triangular distribution.
#'
#' @param q A numeric vector of quantiles.
#' @param min The lower limit of the distribution.
#' @param max The upper limit of the distribution.
#' @param mode The mode (peak) of the distribution.
#' @return A numeric vector containing the cumulative probability.
#' @examples
#' ptriang(0.5, 0, 1, 0.5)
#' ptriang(c(0.2, 0.5, 0.8), 0, 1, 0.5)
#' ptriang(0, 0, 1, 0.5)   # en el límite inferior, devuelve 0
#' ptriang(1, 0, 1, 0.5)   # en el límite superior, devuelve 1
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: 'min' cannot be greater than 'max'")
  }

  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: 'mode' cannot be out of '[min,max]'")
  }

  ifelse(
    q <= min, 0,
    ifelse(
      q >= max, 1,
      ifelse(
        q <= mode,
        ((q - min)^2) / ((max - min) * (mode - min)),
        1 - ((max - q)^2) / ((max - min) * (max - mode))
      )
    )
  )

}

#' Triangular Distribution Quantiles
#'
#' Computes the quantile function (inverse CDF) of the triangular distribution.
#'
#' @param p A numeric vector of probabilities.
#' @param min The lower limit of the distribution.
#' @param max The upper limit of the distribution.
#' @param mode The mode (peak) of the distribution.
#' @return A numeric vector containing the corresponding quantiles.
#' @examples
#' ptriang(0.5, 0, 1, 0.5)
#' ptriang(c(0.2, 0.5, 0.8), 0, 1, 0.5)
#' ptriang(0, 0, 1, 0.5)   # en el límite inferior, devuelve 0
#' ptriang(1, 0, 1, 0.5)   # en el límite superior, devuelve 1
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: 'min' cannot be greater than 'max'")
  }

  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: 'mode' cannot be out of '[min,max]'")
  }

  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    stop("Error: 'p' must be in [0,1]")
  }

  ifelse(
    p <= (mode - min) / (max - min),
    sqrt(p * (max - min) * (mode - min)) + min,
    max - sqrt((1 - p) * (max - min) * (max - mode))
  )

}

#' Triangular Distribution Random Generation
#'
#' Computes random deviates of the triangular distribution.
#'
#' @param n Number of observations.
#' @param min The lower limit of the distribution.
#' @param max The upper limit of the distribution.
#' @param mode The mode (peak) of the distribution.
#' @return A numeric vector of random generated values.
#' @examples
#' rtriang(10, 0, 1, 0.5)
#' set.seed(42)
#' rtriang(5, 0, 1, 0.5)
#' @importFrom stats runif
#' @export
rtriang <- function(n, min, max, mode) {
  if (any(min > max, na.rm = TRUE)) {
    stop("Error: 'min' cannot be greater than 'max'")
  }

  if (any(mode < min | mode > max, na.rm = TRUE)) {
    stop("Error: 'mode' cannot be out of '[min,max]'")
  }

  u <- runif(n)
  qtriang(u, min, max, mode)
}
