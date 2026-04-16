# Shared test fixtures — auto-loaded by testthat before all test files.

#' Build the canonical two-notebook analysis used across make and nextflow tests.
#' Must be called from a directory that contains a `notebooks/` subdirectory
#' (testthat sets the working directory to `tests/testthat/` automatically).
make_simple_analysis <- function(name = "test_nf") {
  analysis <- new_analysis(name = name)
  suppressMessages(
    analysis %<>% add_notebook(
      "test_notebook_1.Rmd",
      products = c(result_1 = "test_dep_file.txt")
    )
  )
  suppressMessages(
    analysis %<>% add_notebook(
      "test_notebook_2.Rmd",
      dependencies = c(example_dep_1 = "result_1")
    )
  )
  analysis
}
