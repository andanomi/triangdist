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
