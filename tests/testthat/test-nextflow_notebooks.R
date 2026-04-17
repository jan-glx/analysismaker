
simple_analysis <- make_simple_analysis()

# Smoke test ----------------------------------------------------------------
test_that("write_nextflow does not error on simple analysis", {
  tf <- withr::local_tempfile(fileext = ".nf")
  expect_no_error(write_nextflow(simple_analysis, nf_file = tf))
  expect_true(file.exists(tf))
})

# Structural content --------------------------------------------------------
test_that("emitted .nf contains process and workflow blocks", {
  nf <- nf_text(simple_analysis)
  expect_match(nf, "process render_test_notebook_1", fixed = TRUE)
  expect_match(nf, "process render_test_notebook_2", fixed = TRUE)
  expect_match(nf, "workflow {",                     fixed = TRUE)
  expect_match(nf, "publishDir",                     fixed = TRUE)
})

# Process name sanitization -------------------------------------------------
test_that("process names are valid Groovy identifiers", {
  analysis <- new_analysis("x")
  suppressMessages(
    analysis %<>% add_notebook(
      "test_notebook_1.Rmd",
      notebook_name = "60_ngs-qc.Rmd_ILSE.123"
    )
  )
  nf <- nf_text(analysis)
  expect_match(nf, "process render_60_ngs_qc_Rmd_ILSE_123", fixed = TRUE)
  proc_lines <- grep("^process ", strsplit(nf, "\n")[[1]], value = TRUE)
  expect_false(any(grepl("[-.]", proc_lines)))
})

# Params are serialised into the script block ------------------------------
test_that("params are baked into the Rscript call inside script block", {
  analysis <- new_analysis("x")
  suppressMessages(
    analysis %<>% add_notebook(
      "test_notebook_1.Rmd",
      params = list(example_param_1 = "hg38")
    )
  )
  nf <- nf_text(analysis)
  expect_match(nf, "hg38", fixed = TRUE)
})

# Multiple products appear in output block ---------------------------------
test_that("multiple products all appear in output block", {
  analysis <- new_analysis("x")
  suppressMessages(
    analysis %<>% add_notebook(
      "test_notebook_1.Rmd",
      products = c(a_file = "test_dep_file_1.txt", b_file = "test_dep_file_2.txt")
    )
  )
  nf <- nf_text(analysis)
  expect_match(nf, "test_dep_file_1.txt", fixed = TRUE)
  expect_match(nf, "test_dep_file_2.txt", fixed = TRUE)
})

# Dependency paths from upstream notebook appear in downstream input --------
test_that("upstream output path appears in downstream process input block", {
  nf <- nf_text(simple_analysis)

  # The dep file produced by notebook_1 must appear somewhere in the
  # input block of the notebook_2 process
  lines       <- strsplit(nf, "\n")[[1]]
  nb2_start   <- which(grepl("process render_test_notebook_2", lines))
  nb2_end     <- nb2_start + which(lines[seq(nb2_start + 1, length(lines))] == "}")[1]
  nb2_block   <- paste(lines[nb2_start:nb2_end], collapse = "\n")
  expect_match(nb2_block, "test_dep_file.txt", fixed = TRUE)
})


# Integration: nextflow inspect -------------------------------------------
test_that("emitted .nf passes nextflow inspect", {
  skip_if(nchar(Sys.which("nextflow")) == 0L, "nextflow not installed")
  tf <- withr::local_tempfile(fileext = ".nf")
  write_nextflow(simple_analysis, nf_file = tf)
  status <- system2("nextflow", c("inspect", tf), stdout = FALSE, stderr = FALSE)
  expect_equal(status, 0L)
})

# Execution: nextflow run actually renders both notebooks ------------------
test_that("nextflow run executes both notebooks end-to-end", {
  skip_if(nchar(Sys.which("nextflow")) == 0L, "nextflow not on PATH")

  wd <- withr::local_tempdir()
  copy_notebooks(simple_analysis, wd)
  withr::local_dir(wd)

  write_nextflow(simple_analysis, nf_file = "test_nf.nf")

  status <- system2(
    "nextflow",
    c("run", "test_nf.nf", "-ansi-log", "false"),
    stdout = FALSE, stderr = FALSE
  )
  expect_equal(status, 0L)

  nb1_dir <- simple_analysis$notebooks[["test_notebook_1"]]$out_dir_human
  nb2_dir <- simple_analysis$notebooks[["test_notebook_2"]]$out_dir_human
  expect_true(file.exists(file.path(nb1_dir, "test_notebook_1.html")))
  expect_true(file.exists(file.path(nb1_dir, "test_dep_file.txt")))
  expect_true(file.exists(file.path(nb2_dir, "test_notebook_2.html")))
})
