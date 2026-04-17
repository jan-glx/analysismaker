simple_analysis <- make_simple_analysis()

test_that("makefile generation does not error", {
  write_makefile(simple_analysis, analysis_name = "test_analysis_1")
  expect_true(TRUE)
  make_output <- system2("make", stdout = TRUE)
  expect_equal(attr(make_output, "status"), NULL)
  Sys.sleep(1)
  expect_equal(substr(system2("make", stdout = TRUE), 1, 14), substr("make: Nothing to be done for `test_analysis_1'.", 1, 14))
  # Cleanup
  fs::file_delete("makefile")
  fs::file_delete("test_analysis_1.mk")
  fs::dir_delete("results")
  fs::dir_delete("results_human")
})

