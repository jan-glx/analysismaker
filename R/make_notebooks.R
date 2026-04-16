# Make-specific backend for analysismaker2.
# Shared analysis-building functions live in R/notebooks.R.

gen_make_rule <- function(outs, deps = character(0), recipe = character(0)) {
  gen_rule_header <- function(outs, deps)  paste0(paste(outs, collapse = " "), " :", paste0(" ", unlist(deps), recycle0 = TRUE, collapse = ""), "\n")
  explicit_rule_header <- gen_rule_header(outs, deps)
  implicit_rule_header <- if(length(outs) > 1) {
    chars <- lapply(strsplit(outs,""), unique, simplify=FALSE)
    common_chars <- Reduce(intersect, chars[-1], chars[[1]])
    replaced_char <- common_chars[[length(common_chars)]]
    outs <- stringi::stri_replace_last_fixed(outs, replaced_char, "%")
    gen_rule_header(outs, deps)
  } else ""
  paste0(explicit_rule_header, implicit_rule_header, paste0("\t", recipe, "\n", recycle0 = TRUE, collapse=""))
}

gen_clean_command <- function(out_dir) {
  paste0('-rm -rf "', out_dir, '"')
}

gen_out_dir_command <- function(out_dir) {
  paste0('-mkdir -p "', out_dir, '"')
}

gen_symlink_commands <- function(to, from) {
  c(paste0('-rm -rf "', from, '"'),
    paste0('mkdir -p "', fs::path_dir(from), '"'),
    paste0('-ln -s "$$(realpath --relative-to="', fs::path_dir(from), '" "', to, '")" "', from, '"')
  )
}

gen_make_rules_nb <- function(notebook, rmarkdown_params = NULL){
  with(notebook, {
    clean_target <- paste0("clean_", out_dir)
    c(
      gen_make_rule(
        outs = clean_target,
        recipe = c(
          gen_clean_command(out_dir = out_dir),
          gen_clean_command(out_dir = out_dir_human)
        ),
      ),
      gen_make_rule(
        outs = c(out_file, other_out_files),
        deps = c(
          notebook_file,
          dependencies
        ),
        recipe = c(
          gen_clean_command(out_dir = out_dir),
          gen_out_dir_command(out_dir = out_dir),
          gen_render_command(
            notebook_file = notebook_file,
            out_file = out_file,
            out_dir = out_dir,
            params = params,
            rmarkdown_params = rmarkdown_params
          )
        )
      ),
      gen_make_rule(
        outs = out_file_human,
        deps = out_file,
        recipe = gen_symlink_commands(
          to = out_dir,
          from = out_dir_human
        )
      )
    )
  })
}

gen_make_rules <- function(analysis, analysis_name = analysis$name, rmarkdown_params = NULL) {
  c(gen_make_rule(
      outs = analysis_name,
      deps = sapply(analysis$notebooks, function(notebook) notebook$out_file_human)
    ),
    gen_make_rule(
      outs = paste0("clean_", analysis_name),
      deps = sprintf("clean_%s", sapply(analysis$notebooks, function(notebook) notebook$out_dir))
    ),
    sapply(analysis$notebooks, gen_make_rules_nb, rmarkdown_params = rmarkdown_params)
  ) %>% paste0(collapse="\n")
}

#' Write makefile
#'
#' @param analysis analysis object
#' @param analysis_name character string. Name of make targets
#' @param makefile file path. Name of the makefile
#' @param rmarkdown_params list of additional parameters for rmarkdown::render
#' @export
write_makefile <- function(analysis, analysis_name = analysis$name, makefile = paste0(analysis_name, ".mk"), rmarkdown_params = NULL) {
  all_rules <- gen_make_rules(
    analysis = analysis,
    analysis_name = analysis_name,
    rmarkdown_params = rmarkdown_params
  )
  cat(paste0(all_rules, collapse="\n"), file = makefile)
  if(!fs::file_exists("Makefile") & !fs::file_exists("makefile")) cat("include *.mk\n\n..SECONDARY:\n\nSUFFIXES:\n", file="makefile")
  invisible(NULL)
}
