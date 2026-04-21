dtriang <- function(x,a,b,mode) {
if (any(a > b, na.rm =T)) {
  stop("Error: 'min' no puede ser mayor que 'max'")
}

if (any(mode < a | mode > b, na.rm = T)) {
  stop("Error: 'mode' no puede estar fuera de '[min,max]'")
}

resultado <- ifelse(x < a | x > b) {resultado = 0}
            ifelse(a <= x & x <=c) {
              resultado = (2*(x-a)) / ((b-a)*(c-a))
            }else{
             resultado = (2*(b-x)) / ((b-a)*(b-c))
            }
  return(resultado)
}
