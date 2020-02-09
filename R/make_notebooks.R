#' @export
new_analysis <- function(name="analysis", notebook_dir="notebooks") {
  rval <- list(name=name, notebooks=character(0), dependencies=list(), notebook_dir=notebook_dir)
  class(rval) <- "analysis"
  rval
}

#' @export
add_notebook <- function(object, ...){
  dots <- ensyms(...)
  dependencies <- tidyselect::vars_select(.vars = names(object$notebooks), !!!dots[-1])
  notebook_file <- tidyselect::vars_select(.vars = sapply(dots[1], as.character), !!!dots[1])

  object$notebooks %<>% append(notebook_file)
  object$dependencies %<>% append(list2(!!names(notebook_file):=dependencies))
  object
}

#' @export
render_separately <- function(...)  callr::r(function(...) rmarkdown::render(..., envir = globalenv()), args = list(...), show = TRUE)

hash_params <- function(params) {
  params_string <- gsub(" +", " ", paste(deparse(params[sort(names(params))]), collapse=""))
  params_hash <- substr(digest::digest(params_string), 1, 8)
}

#' @export
bind_parameters <- function(analysis, ..., output_dir="results", parameter_set_name=analysis$name){
  all_params <- as.list(enexprs(...))
  unused_params <- all_params
  analysis$params <- list()
  analysis$out_file <- list()
  analysis$out_dir <- list()
  analysis$file_dependencies <- list()
  for(notebook in names(analysis$notebooks)) {
    notebook_file <- analysis$notebooks[[notebook]]
    params <- rmarkdown::yaml_front_matter(fs::path(analysis$notebook_dir, notebook_file))$params
    params$results_dir <- NULL #results_dir

    augmented_params <- list()
    augmented_params[names(analysis$dependencies[[notebook]])] <- all_params[analysis$dependencies[[notebook]]]
    #augmented_params[is.null(augmented_params)] <- NULL
    augmented_params <- c(augmented_params, all_params)

    # to inform the user that he might have done wrong:
    params_not_supplied <- setdiff(names(params), names(augmented_params))
    unused_params[names(params)] <- NULL
    if (length(params_not_supplied)>0) message(length(params_not_supplied), " parameter(s) not supplied for \"", notebook_file, "\", (", notebook, "). Using defaults:\n",
                                               paste0(params_not_supplied, ": ", params[params_not_supplied],  collapse="\n"), "\n")
    augmented_params[params_not_supplied] <- params[params_not_supplied]
    this_notebook_params <- augmented_params[names(params)]
    this_notebook_params_hash <- hash_params(this_notebook_params)
    results_dir <- fs::path(output_dir, fs::path_sanitize(fs::path_ext_remove(notebook_file)), fs::path_sanitize(this_notebook_params_hash))
    this_notebook_params$results_dir <- results_dir

    params_string <- gsub(" +", " ", paste(deparse(this_notebook_params[sort(names(this_notebook_params))]), collapse=""))

    #save results_dir for dependent
    all_params[notebook] <- results_dir

    analysis$params_string[[notebook]] <- params_string
    analysis$params_hash[[notebook]] <- this_notebook_params_hash
    analysis$params[[notebook]] <- this_notebook_params
    analysis$out_file[[notebook]] <- fs::path(results_dir, fs::path_ext_set(notebook_file, "html"))
    analysis$out_dir[[notebook]] <- results_dir
    analysis$file_dependencies[[notebook]] <- analysis$out_file[analysis$dependencies[[notebook]]]
  }
  if(length(unused_params)>0) message(length(unused_params), " parameter(s) supplied but not used: ", paste0(names(unused_params),  collapse=", "))
  analysis
}

expr_to_shell <- function(expr) {
  paste0("Rscript -e '", gsub(pattern = "'", replacement = "'''", paste0(deparse(expr), collapse="")) ,"'")
}


gen_make_rule <- function(out, deps = character(0), recipe = character(0)) {
  paste0(out, ":", paste0(sprintf(" %s", deps), collapse = ""), "\n", paste0(sprintf("\t%s\n", recipe), collapse=""))
}

gen_render_command <- function(notebook_file, output_file, output_dir, params, rmarkdown_params = NULL) {
  rmarkdown_params <- rmarkdown_params %||% exprs(output_format = rmarkdown::html_document(dev="png", keep_md=TRUE))
  render_expr <- expr(
    rmarkdown::render(
      input = !!notebook_file,
      output_file = !!output_file,
      output_dir = !!output_dir,
      params=!!params,
      !!!rmarkdown_params
    )
  )
  expr_to_shell(render_expr)
}

#' @export
gen_make_rules <- function(analysis, rmarkdown_params = NULL, analysis_name = deparse(substitute(analysis))) {
  c(gen_make_rule(analysis_name, analysis$out_file),
    sapply(names(analysis$notebooks), function(notebook) {
      notebook_file <- fs::path(analysis$notebook_dir, analysis$notebooks[[notebook]])
      gen_make_rule(
        out = analysis$out_file[[notebook]],
        deps = c(notebook_file, fs::path(analysis$notebook_dir, "setup_chunk.R"), analysis$file_dependencies[[notebook]]),
        recipe = gen_render_command(
          notebook_file = notebook_file,
          output_file = analysis$out_file[[notebook]],
          output_dir = analysis$out_dir[[notebook]],
          params =  analysis$params[[notebook]],
          rmarkdown_params = rmarkdown_params
        )
      )
    })
  ) %>% paste0(collapse="\n")
}

gen_folders <- function(analysis, analysis_name) {
  for(notebook in names(analysis$notebooks)) {
    notebook_file <- fs::path(analysis$notebook_dir, analysis$notebooks[[notebook]])
    results_dir <- analysis$out_dir[[notebook]]
    fs::dir_create(results_dir)
    sym_link_from <- fs::path(results_dir, "..", analysis_name)
    tryCatch(fs::link_delete(sym_link_from), error = function(e) NULL)
    fs::link_create(fs::path_sanitize(analysis$params_hash[[notebook]]), sym_link_from)
    cat(analysis$params_string[[notebook]], file = fs::path(results_dir, "params.txt"))
    fs::file_create(fs::path(results_dir,  substr(fs::path_sanitize(analysis$params_string[[notebook]]), 1, 50)))
  }
}

#' @export
make_makefile <- function(analysis,
                  analysis_name = paste0(deparse(substitute(analysis)), collapse = ""),
                  makefile = paste0(analysis_name, ".mk")) {
  all_rules <- gen_make_rules(analysis = analysis, analysis_name = analysis_name)
  gen_folders(analysis = analysis, analysis_name = analysis_name)
  cat(paste0(all_rules, collapse="\n"), file = makefile)
  if(!fs::file_exists("Makefile")) cat("include *.mk\n", file="Makefile")
  invisible(NULL)
}
