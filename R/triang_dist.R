dtriang <- function(x,a,b,c) {
if (any(a > b, na.rm =T)) {
  stop("Error: 'min' no puede ser mayor que 'max'")
}

if (any(c < a | c > b, na.rm = T)) {
  stop("Error: 'mode' no puede estar fuera de '[min,max]'")
}

resultado <- ifelse(x < a | x > b,0,
                    ifelse(a <= x & x <=c,
                           (2*(x-a)) / ((b-a)*(c-a)),
                                        (2*(b-x)) / ((b-a)*(b-c))
                          )
                    )

  return(resultado)
}


ptriang <- function(q,a,b,c) {
  res <- ifelse(q <= a,0,
                ifelse(q >= b, 1,
                       ifelse(q <= c,
                              ((q-a)^2) / ((b-a)*(c-a)),
                              1 - ((b-q)^2) / ((b-a)*(b-c))
                       )
                )
  )
  return(res)
}

dtriang(c(1,4,8,10),0,10,5)
ptriang(c(1,4,8,10),0,10,3)
