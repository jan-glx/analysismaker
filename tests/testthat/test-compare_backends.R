test_that("make and nextflow produce identical results_human structure and products", {
  skip_if(nchar(Sys.which("make"))      == 0L, "make not on PATH")
  skip_if(nchar(Sys.which("nextflow")) == 0L, "nextflow not on PATH")

  analysis <- make_simple_analysis()

  # --- run Make in its own tempdir -------------------------------------------
  make_dir <- withr::local_tempdir()
  copy_notebooks(analysis, make_dir)
  withr::with_dir(make_dir, {
    write_makefile(analysis)
    system2("make", stdout = FALSE, stderr = FALSE)
  })

  # --- run Nextflow in its own tempdir ----------------------------------------
  nf_dir <- withr::local_tempdir()
  copy_notebooks(analysis, nf_dir)
  withr::with_dir(nf_dir, {
    write_nextflow(analysis, nf_file = "pipeline.nf")
    system2("nextflow", c("run", "pipeline.nf", "-ansi-log", "false"),
            stdout = FALSE, stderr = FALSE)
  })

  # --- helpers ----------------------------------------------------------------
  rh_files <- function(dir) {
    root <- file.path(dir, "results_human")
    paths <- list.files(root, recursive = TRUE, all.files = TRUE)
    sort(paths)
  }


  # Both backends now use identical relative paths throughout, so only
  # ISO 8601 timestamps (wall-clock) need to be stripped.
  normalize_html <- function(path) {
    html <- paste(readLines(path, warn = FALSE), collapse = "\n")
    html <- gsub("\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}:\\d{2}", "TIMESTAMP", html)
    html
  }

  # --- same set of paths under results_human/ -----------------------------
  make_files <- rh_files(make_dir)
  nf_files   <- rh_files(nf_dir)
  expect_equal(make_files, nf_files)

  # --- non-HTML product files have identical content ----------------------
  non_html_files <- make_files[!grepl("\\.html$", make_files)]

  for (f in non_html_files) {
    make_content <- readLines(file.path(make_dir, "results_human", f), warn = FALSE)
    nf_content   <- readLines(file.path(nf_dir,   "results_human", f), warn = FALSE)
    expect_equal(make_content, nf_content, label = paste("content:", f))
  }

  # --- HTML files are structurally equivalent after stripping timestamps --
  html_files <- make_files[grepl("\\.html$", make_files)]

  for (f in html_files) {
    make_html <- normalize_html(file.path(make_dir, "results_human", f))
    nf_html   <- normalize_html(file.path(nf_dir,   "results_human", f))
    expect_equal(make_html, nf_html, label = paste("html:", f))
  }
})
