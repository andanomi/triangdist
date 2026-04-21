if (any(min > max, na.rm =T)) {
  stop("Error: 'min' no puede ser mayor que 'max'")
}

if (any(mode < min | mode > max, na.rm = T)) {
  stop("Error: 'mode' no puede estar fuera de '[min,max]'")
}
