test_that("parámteros ilógicos", {
  expect_error(dtriang(5,10,0,5))
  expect_error(ptriang(5,10,0,5))

  expect_error(qtriang(0.5,10,0,5))
  expect_error(rtriang(5,10,0,5))

  expect_error(dtriang(5,0,10,-2))
  expect_error(ptriang(5,0,10,-2))

  expect_error(qtriang(0.5,0,10,-2))
  expect_error(rtriang(5,0,10,-2))

  expect_error(dtriang(5,0,10,15))
  expect_error(ptriang(5,0,10,15))

  expect_error(qtriang(0.5,0,10,15))
  expect_error(rtriang(5,0,10,15))
})

test_that("probabilidades imposibles", {
  expect_error(qtriang(-100,0,10,5))
  expect_error(qtriang(100,0,10,5))
})

test_that("dtriang calcula densidades físicas exactas", {
  # Disparo fuera del mapa (debe devolver altura 0)
  expect_equal(dtriang(-1, 0, 10, 5), 0)
  expect_equal(dtriang(12, 0, 10, 5), 0)

  # Disparo en el pico exacto (moda). Altura teórica: 2 / (10 - 0) = 0.2
  expect_equal(dtriang(5, 0, 10, 5), 0.2)
})

test_that("ptriang barre el área de probabilidad correctamente", {
  # Antes del inicio no hay área (0)
  expect_equal(ptriang(-1, 0, 10, 5), 0)
  # Después del final está todo el área (1)
  expect_equal(ptriang(15, 0, 10, 5), 1)
  # En una distribución simétrica perfecta, la moda divide el área a la mitad (0.5)
  expect_equal(ptriang(5, 0, 10, 5), 0.5)
})

test_that("qtriang ejecuta la ingeniería inversa perfecta", {
  # Si el área es 0, estoy en el límite inferior
  expect_equal(qtriang(0, 0, 10, 5), 0)
  # Si el área es 1, estoy en el límite superior
  expect_equal(qtriang(1, 0, 10, 5), 10)
  # Si el área es 0.5 en distribución simétrica, la coordenada es la moda
  expect_equal(qtriang(0.5, 0, 10, 5), 5)
})

test_that("rtriang genera vectores estocásticos funcionales", {
  # Forzamos la creación de 50 elementos
  simulacion <- rtriang(50, 0, 10, 5)

  # Verificamos que la máquina no escupió de más ni de menos
  expect_equal(length(simulacion), 50)
  # Verificamos que ningún número físico ha violado la estructura de la base
  expect_true(all(simulacion >= 0 & simulacion <= 10))
})
