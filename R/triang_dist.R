#' Triangular Distribution Density
#'
#' Computes the probability density function (PDF) of the triangular distribution.
#'
#' @param x A numeric vector of quantiles.
#' @param min The lower limit of the distribution.
#' @param max The upper limit of the distribution.
#' @param mode The mode (peak) of the distribution.
#' @return A numeric vector containing the density values
#' @export

dtriang <- function(x,min,max,mode) {
if (any(min > max, na.rm =T)) {
  stop("Error: 'min' no puede ser mayor que 'max'")
}

if (any(mode < min | mode > max, na.rm = T)) {
  stop("Error: 'mode' no puede estar fuera de '[min,max]'")
}

resultado <- ifelse(x < min | x > max,0,
                    ifelse(min <= x & x <=mode,
                           (2*(x-min)) / ((max-min)*(mode-min)),
                                        (2*(max-x)) / ((max-min)*(max-mode))
                          )
                    )

  return(resultado)
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
#' @export

ptriang <- function(q,min,max,mode) {
  if (any(min > max, na.rm =T)) {
    stop("Error: 'min' no puede ser mayor que 'max'")
  }

  if (any(mode < min | mode > max, na.rm = T)) {
    stop("Error: 'mode' no puede estar fuera de '[min,max]'")
  }

  res <- ifelse(q <= min,0,
                ifelse(q >= max, 1,
                       ifelse(q <= mode,
                              ((q-min)^2) / ((max-min)*(mode-min)),
                              1 - ((max-q)^2) / ((max-min)*(max-mode))
                       )
                )
  )
  return(res)
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
#' @export

qtriang <- function(p,min,max,mode) {
  if (any(min > max, na.rm =T)) {
    stop("Error: 'min' no puede ser mayor que 'max'")
  }

  if (any(mode < min | mode > max, na.rm = T)) {
    stop("Error: 'mode' no puede estar fuera de '[min,max]'")
  }
  if (any(p < 0 | p > 1,na.rm = T)){
    stop("Error: 'p' debe estar estrictamente en el intervalo [0,1] ")
  }
  res <- ifelse(p <= (mode-min) / (max-min),
                sqrt(p *(max-min)*(mode-min)) + min,
                max - sqrt((1-p)*(max-min)*(max-mode)))
  return(res)
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
#' @export

rtriang <- function(n,min,max,mode){
  if (any(min > max, na.rm =T)) {
    stop("Error: 'min' no puede ser mayor que 'max'")
  }

  if (any(mode < min | mode > max, na.rm = T)) {
    stop("Error: 'mode' no puede estar fuera de '[min,max]'")
  }

  u <- runif(n)
  res <- qtriang(u,min,max,mode)
  return(res)
}
