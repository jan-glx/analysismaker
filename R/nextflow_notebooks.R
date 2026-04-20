# Sanitize a notebook_name into a valid Groovy/Nextflow identifier
nf_process_name <- function(notebook_name) {
  paste0("render_", gsub("[^a-zA-Z0-9_]", "_", notebook_name))
}

# Derive a valid, unique Nextflow emit name for each output file path.
# Uses the name from analysis$dependencies (which is guaranteed unique by
# add_notebook()) rather than basename(), avoiding both collisions and
# leading-digit identifiers.
path_to_emit <- function(paths, analysis) {
  dep_paths <- as.character(analysis$dependencies)
  dep_names <- names(analysis$dependencies)
  vapply(paths, function(p) {
    idx <- match(p, dep_paths)
    raw <- if (!is.na(idx)) dep_names[[idx]] else gsub("[^a-zA-Z0-9_]", "_", basename(p))
    raw <- gsub("[^a-zA-Z0-9_]", "_", raw)
    if (grepl("^[0-9]", raw)) raw <- paste0("_", raw)
    raw
  }, character(1))
}

gen_nf_process <- function(notebook_name, notebook, analysis) {
  proc_name <- nf_process_name(notebook_name)

  nb_file   <- as.character(notebook$notebook_file)  # e.g. notebooks/foo.Rmd
  dep_paths <- unique(unlist(notebook$dependencies))  # e.g. results/nb/hash/f.tsv
  out_dir   <- as.character(notebook$out_dir)         # e.g. results/nb/hash

  # --- input: stageAs mirrors the project directory structure ----------------
  # Notebook staged into notebooks/, deps staged into their full relative path.
  # rmarkdown::render() sets CWD to the notebook dir (notebooks/) automatically,
  # so params like results_dir = "../results/nb/hash" resolve correctly --
  # identical to Make, with no parameter remapping needed.
  input_lines <- c(
    sprintf('    path "%s", stageAs: "%s"', basename(nb_file), nb_file),
    if (length(dep_paths) > 0)
      sprintf('    path "%s", stageAs: "%s"', basename(dep_paths), dep_paths)
  )

  # --- output ----------------------------------------------------------------
  # Outputs are declared at their full subpath -- matching where the notebook
  # writes them (out_dir for HTML, params$results_dir for products).
  out_files      <- as.character(c(notebook$out_file, notebook$other_out_files))
  out_emit_names <- path_to_emit(out_files, analysis)

  output_lines <- c(    
      sprintf('    path "%s/**" ', out_dir),      
      sprintf('    path "%s", emit: %s', out_files, out_emit_names)
  )

  # --- render command --------------------------------------------------------
  render_cmd <- gen_render_command(
    notebook_file = nb_file,
    out_file      = as.character(notebook$out_file),
    out_dir       = out_dir,
    params        = notebook$params
  )

  # --- publishDir: mirror work dir structure into project root ---------------
  # publishDir "." copies all specified output from work dir to launch dir, preserving
  # directory structure
  publish_lines <- c(
            '  publishDir ".", mode: \'symlink\', overwrite: true',
    sprintf('  publishDir ".", mode: \'symlink\', overwrite: true, saveAs: { f -> f.replace("%s", "%s") }', out_dir, notebook$out_dir_human)
  )

  # --- symlink: create results_human link via afterScript --------------------
  rel_target    <- fs::path_rel(out_dir, start = notebook$out_dir_human)

  # --- script: mkdir and render ----------------------------------------------
  script_lines <- c(
    sprintf('    mkdir -p "%s"', out_dir),
    sprintf('    %s', render_cmd),
    sprintf('    mkdir -p "%s"', dirname(notebook$out_dir_human)),
    sprintf('    ln -sfn "%s" "%s"', rel_target, notebook$out_dir_human)
  )

  paste0(
    sprintf("process %s {\n", proc_name),
    paste(publish_lines, collapse = "\n"), "\n\n",
    "  input:\n",
    paste(input_lines, collapse = "\n"), "\n\n",
    "  output:\n",
    paste(output_lines, collapse = "\n"), "\n\n",
    "  script:\n",
    '  """\n',
    paste(script_lines, collapse = "\n"), "\n",
    '  """\n',
    "}\n"
  )
}

gen_workflow_lines <- function(analysis) {
  nb_names <- names(analysis$notebooks)

  # Build a map: output file path -> (process name, emit name)
  produced_by <- list()
  for (nb_name in nb_names) {
    nb <- analysis$notebooks[[nb_name]]
    for (p in c(nb$out_file, nb$other_out_files)) {
      p_chr <- as.character(p)
      emit  <- path_to_emit(p_chr, analysis)
      produced_by[[p_chr]] <- list(proc = nb_name, emit = emit)
    }
  }

  call_lines <- vapply(nb_names, function(nb_name) {
    proc      <- nf_process_name(nb_name)
    nb        <- analysis$notebooks[[nb_name]]
    dep_paths <- unique(unlist(nb$dependencies))

    dep_args <- vapply(dep_paths, function(dep) {
      info <- produced_by[[dep]]
      if (!is.null(info)) {
        sprintf("%s.out.%s", nf_process_name(info$proc), info$emit)
      } else {
        sprintf('Channel.fromPath("%s")', dep)
      }
    }, character(1))

    args <- c(
      sprintf('Channel.fromPath("%s")', nb$notebook_file),
      dep_args
    )
    sprintf("  %s(%s)", proc, paste(args, collapse = ", "))
  }, character(1))

  call_lines
}

gen_nf_workflow <- function(analysis) {
  call_lines <- gen_workflow_lines(analysis)
  paste0(
    "workflow {\n",
    paste(call_lines, collapse = "\n"), "\n",
    "}\n"
  )
}

#' Return the full .nf content as a single character string
#' @param analysis_name character. Used in the file header comment.
#' @param process_blocks character vector, each containing a process block.
#' @param call_lines character vector, each containing a workflow call line.
#' @param includes named character vector, each containing a output to include named by module.
#' @return length-1 character vector.
#' @export
nf_text_single <- function(analysis_name = analysis$name,  process_blocks, call_lines, includes = character(0)) {
  header <- paste0(
    "// Nextflow pipeline generated by analysismaker2\n",
    "// Analysis: ", analysis_name, "\n",
    "// Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
    "nextflow.enable.dsl = 2\n\n"
  )
 
  paste0(
    header,
    paste0("include { ", includes, " } from './", names(includes), collapse = "\n", recycle0 = TRUE), "\n\n",
    paste(process_blocks, collapse = "\n"),
    "\n",
    "workflow {\n",
    paste(call_lines, collapse = "\n"), 
    "\n",
    "}\n"
  )
}

#' Return the full .nf content as a single character string
#' @param analysis analysis object
#' @param analysis_name character. Used in the file header comment.
#' @return length-n character vector.
#' @export
nf_text <- nf_texts <- function(analysis, analysis_name = analysis$name) {

  process_blocks <- mapply(
    gen_nf_process,
    notebook_name = names(analysis$notebooks),
    notebook      = analysis$notebooks,
    MoreArgs      = list(analysis = analysis),
    SIMPLIFY      = FALSE
  )
  
  call_lines <- gen_workflow_lines(analysis)
  nf_text_single(analysis_name, process_blocks, call_lines)
}

#' Emit a static Nextflow pipeline file
#' @param analysis analysis object
#' @param nf_file character. Output path. Defaults to \code{<name>.nf}.
#' @param analysis_name character. Used only for the default file name.
#' @return Invisibly returns the path to the written file.
#' @export
write_nextflow <- function(analysis,
                           nf_file = paste0(analysis_name, ".nf"),
                           analysis_name = analysis$name) {
  writeLines(nf_text(analysis, analysis_name = analysis_name), con = nf_file)
  invisible(nf_file)
}
