test_that("creating simple analysis does not error", {
  expect_no_error(make_simple_analysis())
})

test_that("creating small large analysis does not error", {
  expect_no_error(make_large_analysis(n=2))
})

test_that("creating large analysis does not error", {
  expect_no_error(make_large_analysis(n=2))
  expect_no_error(make_large_analysis())
})