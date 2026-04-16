test_that("creating simple analysis + makefile does not error", {
  analysis_1 <- make_simple_analysis(name = "test_analysis_1")
  analysis_1 %>% write_makefile()
  expect_true(TRUE)
  make_output <- system2("make", stdout = TRUE)
  Sys.sleep(1)
  expect_equal(attr(make_output, "status"), NULL)
  expect_equal(substr(system2("make", stdout = TRUE), 1, 14), substr("make: Nothing to be done for `test_analysis_1'.", 1, 14))
  fs::file_delete("makefile")
  fs::file_delete("test_analysis_1.mk")
  fs::dir_delete("results")
  fs::dir_delete("results_human")
})

