context("simple checks")

test_that("creating simple analysis + makefile does not error", {
  analysis_1 <- new_analysis(name = "test_analysis_1", notebook_dir = ".")
  analysis_1 %<>% add_notebook("test_notebook_1.Rmd")
  analysis_1 %>% write_makefile()
})
