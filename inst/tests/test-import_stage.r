context('build_import_stagerunner')

test_that('it returns a stageRunner', {
  modelenv <- new.env()
  sr <- stageRunner$new(modelenv, build_import_stagerunner(list()), remember = TRUE)
  expect_is(sr, 'stageRunner')
})

test_that('it correctly builds a stagerunner for one data source', {
  correct_filename <- file.path(tempfile())
  write.csv(iris, correct_filename, row.names = FALSE)
  modelenv <- new.env()
  sr <- stageRunner$new(modelenv,
    build_import_stagerunner(list(file = list(name = correct_filename, stringsAsFactors = TRUE))),
    remember = FALSE)
  sr$run()
  expect_identical(modelenv$data, iris)
  unlink(correct_filename)
})

test_that('it correctly checks whether no data source loaded correctly', {
  modelenv <- new.env()
  sr <- stageRunner$new(modelenv, build_import_stagerunner(list()))
  expect_error(sr$run(), 'Failed to load data from all data sources')
})

test_that('it skips data sources it can\'t load from', {
  modelenv <- new.env()
  correct_filename <- file.path(tempfile())
  write.csv(iris, correct_filename, row.names = FALSE)
  opts <- list(file = 'nonexistent', file = correct_filename)
  sr <- stageRunner$new(modelenv, build_import_stagerunner(opts))
    
  sr$run()
  expect_identical(modelenv$data, within(iris, Species <- as.character(Species)))
  unlink(correct_filename)
})

context('import_stage')

test_that('it runs an example import stage correctly', {
  modelenv <- new.env()
  file <- tempfile()
  write.csv(iris, file, row.names = FALSE)
  filename <- file.path(file)
  stageRunner$new(modelenv, import_stage(filename))$run()
  expect_identical(modelenv$data, within(iris, Species <- as.character(Species)))
  unlink(file)
})

test_that('it runs an example import stage with a skip correctly', {
  modelenv <- new.env()
  mock_globalenv <- new.env(); mock_globalenv$cached_data <- iris
  sr <- stageRunner$new(modelenv,
    import_stage(list(file = 'nonexistent',
                      R = list(name = 'cached_data', env = mock_globalenv))))
  sr$run()
  expect_identical(modelenv$data, iris)
})

