context('adapter reference class')

test_that('it can initialize an adapter correctly', {
  a <- adapter(identity, identity)
})

test_that('it can read using a simple example adapter correctly', {
  tmp <- new.env()
  read_function <- function(opts) tmp$x <- opts$resource
  a <- adapter(read_function, identity)
  expect_identical(a$read("test"), "test")
  expect_identical(tmp$x, "test")
})

test_that('it can write using a simple example adapter correctly', {
  tmp <- new.env()
  read_function <- function(opts) tmp[[opts$resource]]
  write_function <- function(val, opts) { force(opts); tmp$x <- val }
  a <- adapter(read_function, write_function)
  a$write("test")
  expect_identical(tmp$x, "test")
  expect_identical(a$read('x'), "test")
})

test_that('it formats options according to a formatting function', {
  formatter <- function(opts) list(file = opts$resource)
  a <- adapter(identity, identity, formatter)
  expect_identical(a$read("test")$file, "test")
})

test_that('it merges in default options if set', {
  a <- adapter(identity, identity, identity, list(blah = 'foo'))
  expect_identical(a$read("test")$blah, "foo")
})

test_that('it does not overwrite set values with defaults', {
  a <- adapter(identity, identity, identity, list(blah = 'foo'))
  expect_identical(a$read(list(blah = 'bar'))$blah, "bar")
})

context('fetch_adapter')

test_that('it fetches the s3 adapter', {
  expect_identical(fetch_adapter('s3')$.keyword, 's3')
})

test_that('it fetches the default adapter', {
  expect_identical(fetch_adapter('file')$.keyword, 'file')
})

test_that('it fetches the R adapter', {
  expect_identical(fetch_adapter('R')$.keyword, 'R')
})

