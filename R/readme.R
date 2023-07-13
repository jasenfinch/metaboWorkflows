#' @importFrom glue glue

readme <- function(project_name,workflow,path = '.',renv = TRUE){
  message('Adding project README')
  
  project_directory <- projectDirectory(project_name, path)
  
  if (isTRUE(renv)){
    renv_text <- 'The [renv](https://rstudio.github.io/renv/index.html) package is used for managing the project R package dependencies.'
  } else {
    renv_text <- ''
  }
  
  body <- glue("# {project_name}
             
This is a {workflow} metabolomics analysis workflow project.
This project uses the [targets](https://docs.ropensci.org/targets/) package for workflow management and reproducibility.
A [`git`](https://git-scm.com/) repository is initialted within the project that can be used to track changes to the analysis code.
{renv_text}

The R code for the analysis targets can be found in the `_targets.R` file.
Files containing additional functions can be placed in `R/functions` and package loading utilities can be found in `R/utils.R`.

## Running the analysis

To run the analysis, either run the `misc/run.R` script or execute `targets::tar_make()` in an R session loaded from within the project directory.

")
  
  writeLines(body, glue('{project_directory}/README.md'))
}
