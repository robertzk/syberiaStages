context("data stage")
require(mungebits)

test_that("it turns a munge_procedure into a stagerunner", {
  tmp <- new.env(); tmp$data <- iris
  munge_procedure <-
    rep(list(list(mungebits::column_transformation(function(x) 2 * x), 1)), 2)
  data_stage(tmp, munge_procedure)$run()

  expect_equal(tmp$data[[1]], 4 * iris[[1]])
})

# TODO: Test triggers!

