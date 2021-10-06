# metaboWorkflows 0.9.0

* Added a `NEWS.md` file to track changes to the package.

* A package website is now available at <https://jasenfinch.github.io/metaboWorkflows/>.

* The package can now be used to firstly define a workflow and then generate a project template directory that facilitates reproducible analysis powered by the use of [`targets`](https://docs.ropensci.org/targets/) package, [`renv`](https://rstudio.github.io/renv/), [git](https://git-scm.com/) and [docker](https://www.docker.com/).

* A choice of workflow data inputs are now available that include file paths and converted data file retrieval from a [`grover`](https://jasenfinch.github.io/grover/) web API.

* Workflows can be defined using [`defineWorkflow()`](https://jasenfinch.github.io/metaboWorkflows/reference/defineWorkflow.html) based the workflow input type and metabolomic technique, also enabling the selection of project template parameters.

* Workflow definitions can be modified which is detailed in the [Workflow customisation and extension](https://jasenfinch.github.io/metaboWorkflows/articles/workflow-customisation.html) vignette.

* Project directories for workflow definitions can be generated using [`generateWorkflow()`](https://jasenfinch.github.io/metaboWorkflows/reference/generateWorkflow.html).

* The [Introduction vignette](https://jasenfinch.github.io/metaboWorkflows/articles/metaboWorkflows.html) has been updated to detail the new package functionality.
