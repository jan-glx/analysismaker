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

  analysis %<>% add_external_dependency(fs::path(analysis$notebook_dir, "test_external_dep_file.txt"), "external_dep_1")

  suppressMessages(
    analysis %<>% add_notebook(
      "test_notebook_2.Rmd",
      dependencies = c(example_dep_1 = "result_1", external_dep_1 = "external_dep_1"),
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

# Build a large chained analysis for chunking tests.
# Each notebook depends on the previous one's product.
make_large_analysis <- function(n = 500, notebook_dir = test_path("notebooks"), ...) {
  analysis <- new_analysis("big", notebook_dir = notebook_dir, ...)
  analysis %<>% add_external_dependency(fs::path(analysis$notebook_dir, "test_external_dep_file.txt"), "external_dep_1")
  
  suppressMessages({
    analysis %<>% add_notebook(
      "test_notebook_1.Rmd",
      notebook_name = "nb_001",
      products = c(dep_001 = "test_dep_file.txt")
    )
    for (i in 2:n) {
      analysis %<>% add_notebook(
        "test_notebook_2.Rmd",
        notebook_name = sprintf("nb_%03d", i),
        dependencies = c(example_dep_1 = sprintf("dep_%03d", i - 1), external_dep_1 = "external_dep_1"),
        products = setNames("out.txt", sprintf("dep_%03d", i))
      )
    }
  })
  analysis
}

expect_system2_success <- function(command, args = character(), stdout = TRUE, stderr = TRUE, ...) {
  result <- suppressWarnings(system2(command, args, stdout = stdout, stderr = stderr, ...))
  expect_equal(attr(result, "status"), NULL, info = paste(
    "Error in ", command, paste(args, collapse = " "), 
    "\nFailed with status", attr(result, "status"), 
    "\nOutput:\n", paste(result, collapse = "\n"), 
    "\nError:", paste(attr(result, "stderr"), collapse = "\n")))
}