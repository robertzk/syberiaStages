context('s3 adapter')

test_that('it correctly formats a bucket into the appropriate s3 path', {
  s3_adapter <- fetch_adapter('s3')
  formatted_opts <- s3_adapter$format(list(file = 'blah', bucket = 'blah'))
  expect_identical(formatted_opts$s3path, 's3://blah/',
    info = paste0('The s3 adapter should have transformed the bucket = ',
                  '"blah" parameter into s3path = "s3://blah/"'))
})
