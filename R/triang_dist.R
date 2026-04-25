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

dtriang(c(1,4,8,10),0,10,5)
ptriang(c(1,4,8,10),0,10,3)

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
qtriang(100,0,10,5)
