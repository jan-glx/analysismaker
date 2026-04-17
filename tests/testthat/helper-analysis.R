# Shared test fixtures -- auto-loaded by testthat before all test files.
# Build the canonical two-notebook analysis used across make and nextflow tests.
# Must be called from a directory that contains a `notebooks/` subdirectory.
make_simple_analysis <- function(name = "test_nf", notebook_dir = test_path("notebooks"), ...) {
  analysis <- new_analysis(name = name, notebook_dir = notebook_dir, ...)
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

# Copy the analysis notebook_dir into a target directory, preserving the
# relative path structure stored in the analysis object.
copy_notebooks <- function(analysis, target_dir) {
  fs::dir_copy(
    fs::path(analysis$root, analysis$notebook_dir),
    fs::path(target_dir,    analysis$notebook_dir)
  )
}
