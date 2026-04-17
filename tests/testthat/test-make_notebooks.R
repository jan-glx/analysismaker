simple_analysis <- make_simple_analysis()

test_that("makefile generation does not error", {
  wd <- withr::local_tempdir()
  copy_notebooks(simple_analysis, wd)
  withr::local_dir(wd)

  write_makefile(simple_analysis, analysis_name = "test_analysis_1")
  expect_true(TRUE)
  make_output <- system2("make", stdout = TRUE)
  expect_equal(attr(make_output, "status"), NULL)
  Sys.sleep(1)
  expect_equal(substr(system2("make", stdout = TRUE), 1, 14), substr("make: Nothing to be done for `test_analysis_1'.", 1, 14))
})

