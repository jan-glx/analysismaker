#' Create new analysis object
#' @param name character string. Specifies name of the analysis (used in output file names)
#' @param notebook_dir base directory of the notebook files
#' @param out_dir base directory for the result files
#' @param out_dir_human base directory for links to the result files with human readable names
#' @export
new_analysis <- function(name="analysis", notebook_dir="notebooks", out_dir = "results",  out_dir_human = fs::path("results_human", name)) {
  rval <- list(name=name, notebooks=list(), dependencies=character(0), notebook_dir=notebook_dir, out_dir=out_dir, out_dir_human=out_dir_human)
  class(rval) <- "analysis"
  rval
}

#' Add external file to use as dependency
#' @param analysis analysis object (as created with new_analysis)
#' @param dependency_file character string. Specifies the path of the external dependency
#' @param dependency_name character string. Name under which above path is available as parameter to notebooks.
#' @export
add_external_dependency <- function(analysis, dependency_file, dependency_name = fs::path_file(dependency_file)) {
  analysis$dependencies[dependency_name] <- dependency_file
  analysis
}

#' Add notebook to analysis
#' @param analysis analysis object
#' @param notebook_file file path of the notebook to add to analysis (relative to analysis$notebook_dir)
#' @param products (named) character vector specifying file paths of side products of the notebook, they are available as parameters to subsequent notebooks by their name
#' @param dependencies named character vector specifying file paths of files required by the notebook, they are available as parameters to the notebook by their name
#' @param params named list of parameters to supply to the notebook
#' @param notebook_name character string to use as name of the notebook under which its out_dir will available as parameter to subsequent notebooks
#' @export
add_notebook <- function(analysis, notebook_file, products = character(0), dependencies = character(0), params = list(), notebook_name = fs::path_sanitize(fs::path_ext_remove(notebook_file))){
  params_call <- params
  force(notebook_name)
  notebook_file <- fs::path(analysis$notebook_dir, notebook_file)
  params_nb <- rmarkdown::yaml_front_matter(notebook_file)$params
  params_nb$results_dir <- NULL # change in results_dir should not change hash & is overwritten anyway

  working_dir <- fs::path_dir(notebook_file)
  deps <- analysis$dependencies[dependencies]
  names(deps) <- names(dependencies)


  params_deps <- as.list(deps[names(deps) %in% names(params_nb)])
  params_deps <- lapply(params_deps, function(x) fs::path_rel(x, working_dir))

  params_sepcified_as_dep_and_as_param <- intersect(names(params_deps), names(params_call))
  if(length(params_sepcified_as_dep_and_as_param)>0) stop(length(params_sepcified_as_dep_and_as_param), " parameter(s) supplied in both, dependencies and params: ", paste0(names(params_sepcified_as_dep_and_as_param),  collapse=", "))

  params <- c(params_deps, params_call)

  params_not_supplied <- setdiff(names(params_nb), names(params))
  if (length(params_not_supplied)>0) message(length(params_not_supplied), " parameter(s) not supplied for \"", notebook_file, "\", (", notebook_name, "). Using defaults:\n",
                                             paste0(params_not_supplied, ": ", params_nb[params_not_supplied],  collapse="\n"), "\n")


  invalid_params <- setdiff(names(params_call), names(params_nb))
  if(length(invalid_params)>0) stop(length(invalid_params), " parameter(s) supplied but specified in ", notebook_file, " (", notebook_name, "):", paste0(names(invalid_params),  collapse=", "))

  params[names(params_call)] <- params_call

  #sort parameter list for hashing
  params <- params[sort(names(params))]

  params_hash <- hash_params(params)

  out_dir <- fs::path(analysis$out_dir, notebook_name, fs::path_sanitize(params_hash))

  out_file <- fs::path(out_dir, fs::path_ext_set(notebook_name, "html"))
  other_out_files <- fs::path(out_dir, products)
  names(other_out_files) <- names(products)

  out_dir_human <- fs::path(analysis$out_dir_human, notebook_name)
  out_file_human <- fs::path(out_dir_human, fs::path_ext_set(notebook_name, "html"))

  analysis$dependencies[notebook_name] <- out_file
  analysis$dependencies[names(other_out_files)] <- other_out_files

  params$results_dir <- fs::path_rel(out_dir, working_dir)

  notebook <- list(out_file_human = out_file_human, out_dir_human = out_dir_human, out_file = out_file, other_out_files = other_out_files, dependencies = deps, notebook_file = notebook_file, params = params, out_dir = out_dir)
  analysis$notebooks[[notebook_name]] <- notebook
  analysis
}

#' Render separately
#'
#' helper function to render rmarkdown in a separate process using callr
#' @param ... parameters to rmarkdown::render
#' @export
render_separately <- function(...)  callr::r(function(...) rmarkdown::render(..., envir = globalenv()), args = list(...), show = TRUE)

hash_params <- function(params) {
  params_string <- gsub(" +", " ", paste(deparse(params[sort(names(params))]), collapse=""))
  params_hash <- substr(digest::digest(params_string), 1, 8)
}

expr_to_shell <- function(expr) {
  paste0("\"$(R_HOME)/bin/Rscript\" -e '", gsub(pattern = "'", replacement = "'''", paste0(deparse(expr), collapse="")) ,"'")
}

gen_make_rule <- function(outs, deps = character(0), recipe = character(0)) {
  outs <- stringi::stri_replace_last_fixed(outs, ".", "%")
  paste0(paste(outs, collapse = " "), " :", paste0(sprintf(" %s", deps), collapse = ""), "\n", paste0(sprintf("\t%s\n", recipe), collapse=""))
}

gen_render_command <- function(notebook_file, out_file, out_dir, params, rmarkdown_params = NULL) {
  rmarkdown_params <- rmarkdown_params %||% exprs(intermediates_dir = tempdir(), clean = FALSE)
  render_expr <- expr(
    rmarkdown::render(
      input = !!notebook_file,
      output_file = !!out_file,
      output_dir = !!out_dir,
      params=!!params,
      !!!rmarkdown_params
    )
  )
  expr_to_shell(render_expr)
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
      gen_make_rule( # clean rule
        outs = clean_target,
        recipe = c(
          gen_clean_command(out_dir = out_dir),
          gen_clean_command(out_dir = out_dir_human)
        ),
      ),
      gen_make_rule( # notebook rule
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
            params =  params,
            rmarkdown_params = rmarkdown_params
          )
        )
      ),
      gen_make_rule( # rule for human readable file name
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
  c(gen_make_rule( # analysis level rule
      outs = analysis_name,
      deps = sapply(analysis$notebooks, function(notebook) notebook$out_file_human)
    ),
    gen_make_rule( # analysis level clean rule
      outs = paste0("clean_", analysis_name),
      deps = sprintf("clean_%s", sapply(analysis$notebooks, function(notebook) notebook$out_dir))
    ),
    # individual notebook rules
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
  if(!fs::file_exists("Makefile") & !fs::file_exists("makefile")) cat("include *.mk\n", file="makefile")
  invisible(NULL)
}
