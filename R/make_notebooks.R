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
    c(
      # Two clean rules rather than one, so that each rule is either fully
      # variant-independent or fully variant-specific -- never a mix.
      #
      # out_dir is content-addressed (results/<nb>/<hash>), so it carries no
      # analysis-variant component: two analyses that render a notebook with the
      # same params (e.g. a `debug` variant of a notebook whose params ignore
      # DEBUG) produce the very same target in their respective makefiles. Since
      # makefiles are usually pulled in together via `include *.mk`, make then
      # sees a repeated target and warns "overriding recipe" / "ignoring old
      # recipe", and the last-included recipe silently wins.
      #
      # Folding the variant-specific out_dir_human removal into that shared
      # target made the collision consequential -- whichever makefile was
      # included last decided which variant's symlink got cleaned. Splitting it
      # out keeps the shared target's recipe identical across variants, so the
      # duplicates are byte-identical and can simply be deduplicated by the
      # caller, while the variant's symlink is cleaned by its own uniquely-named
      # target.
      gen_make_rule(
        outs = paste0("clean_", out_dir),
        recipe = gen_clean_command(out_dir = out_dir)
      ),
      gen_make_rule(
        outs = paste0("clean_", out_dir_human),
        recipe = gen_clean_command(out_dir = out_dir_human)
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
      deps = sprintf("clean_%s", c(
        sapply(analysis$notebooks, function(notebook) notebook$out_dir),
        sapply(analysis$notebooks, function(notebook) notebook$out_dir_human)
      ))
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
