# Analysismaker2

A lightweight workflow builder for R Markdown–based analyses.

`analysismaker2` lets you define an analysis as a directed acyclic graph (DAG) of notebooks with explicit **dependencies** (input files) and **products** (output files). From this specification, it generates a `Makefile`, enabling reproducible, incremental execution of complex analysis pipelines.

The package is designed for workflows where analysis steps are naturally expressed as notebooks, but still require reliable orchestration, parameterization, and caching.

## Key ideas

- **Explicit inputs and outputs**  
  Each step declares the files it depends on and the files it produces.

- **Parameterized notebooks**  
  The same notebook can be reused multiple times with different parameters and names.

- **Incremental execution**  
  Generated `Makefile`s allow `make` to rerun only those steps whose inputs have changed.

- **Bridging notebooks and pipelines**  
  Retains notebook ergonomics while providing workflow-level guarantees.

## Installation

````r
# install.packages("remotes")
remotes::install_github("jan-glx/analysismaker")
````

(Adjust the repository name if using a fork or variant such as `analysismaker2`.)

## Minimal example

````r
library(analysismaker2)

analysis <-
  new_analysis(name = "analysis") %>%
  add_external_dependency("meta_data/WNT_genes.tsv", "WNT_genes_file") %>%
  add_notebook(
    "2_get_all_WNT_guides.Rmd",
    dependencies = c(WNT_genes_file = "WNT_genes_file"),
    products     = c(allguides_plus_file = "allguides_plus_dt.tsv")
  ) %>%
  add_notebook(
    "3_select_WNT_guides.Rmd",
    dependencies = c(allguides_plus_file = "allguides_plus_file"),
    products     = c(selected_guides_file = "selected_guides.tsv"),
    params       = list(GENOME = "hg38")
  )

analysis %>% write_makefile()
````

Run the pipeline:

````bash
make -j 4
````

## Core API concepts

### `new_analysis(name)`
Creates a new analysis object. The name is used for output organization and Makefile generation.

### `add_external_dependency(path, dependency_name)`
Registers an input file that is not produced by the workflow itself.

### `add_notebook(notebook_file, dependencies, products, params, notebook_name)`
Adds an R Markdown notebook as a workflow step.

- `dependencies`: named mapping from notebook parameter names to dependency identifiers  
- `products`: named mapping from product identifiers to output filenames  
- `params`: parameters passed to `rmarkdown::render()`  
- `notebook_name`: optional override when reusing the same notebook multiple times

### `write_makefile()`
Generates a `Makefile` describing the dependency graph and build rules.

## Typical use cases

- large analyses composed of many R Markdown notebooks
- parameter sweeps or repeated analyses with shared logic
- incremental recomputation on local machines or HPC systems
- workflows where notebook execution order must be explicit and reproducible

## Relation to workflowr

`workflowr` focuses on reproducible research websites, providing versioned reports with tight Git integration.  
`analysismaker2` focuses on workflow orchestration: explicit dependencies and products for notebooks, and incremental execution via generated `Makefile`s.

The two tools address different aspects of reproducible research and can be used independently. In projects where both apply, `analysismaker2` can be used to run and manage the analysis pipeline, while `workflowr` can be used to publish selected results.

This project is not intended as a drop-in replacement for `workflowr`’s site or publishing features.

## Design notes and stability

This package was originally developed to address concrete needs in my own analysis workflows, particularly for large, multi-step pipelines built around parameterized R Markdown notebooks. Some design choices therefore reflect pragmatic trade-offs made under real usage constraints rather than a fully generalized API design.

While the current interface is stable enough for ongoing use, I am not fully satisfied with all design decisions. A future major version (v3) may therefore introduce breaking changes if this leads to a clearer, more consistent, or more maintainable API. Such changes would be motivated by simplification and improved correctness rather than feature expansion.

## License

MIT
