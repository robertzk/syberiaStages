context('build_export_stagerunner')

test_that('it returns a stageRunner', {
  modelenv <- new.env()
  sr <- stageRunner$new(modelenv, build_export_stagerunner(list()))
  expect_is(sr, 'stageRunner')
})

test_that('it correctly builds a stagerunner for one data source', {
  correct_filename <- file.path(tempfile())
  modelenv <- new.env()
  modelenv$model_stage <- list(model = test_model <- list(1, 2, 3))
  sr <- stageRunner$new(modelenv,
    build_export_stagerunner(list(file = correct_filename)))
  sr$run()
  expect_identical(readRDS(correct_filename), test_model)
  unlink(correct_filename)
})

context('export_stage')

test_that('it runs an example export stage correctly', {
  somefile <- tempfile()
  filename <- file.path(somefile)
  modelenv <- new.env()
  some_model_data <- list("this is the model", 5)
  modelenv$model_stage <- list(model = some_model_data)
  stageRunner$new(modelenv, export_stage(list(file = filename)))$run()
  expect_identical(readRDS(filename), some_model_data)
  unlink(somefile)
})

test_that('it runs an example export stage with a copy correctly', {
  somefile <- tempfile()
  filename <- file.path(somefile)
  modelenv <- new.env(); mock_globalenv <- new.env()
  some_model_data <- list("this is the model", 5)
  modelenv$model_stage <- list(model = some_model_data)
  sr <- stageRunner$new(modelenv,
    export_stage(list(file = filename)))
  sr$run()
  expect_identical(readRDS(filename), some_model_data)
  unlink(somefile)
})

