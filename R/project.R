#' S4 class to store project directory definitions
#' @description An S4 class to store the workflow project directory definitions.
#' @slot project_name the project name
#' @slot path the project directory path
#' @slot renv add infrastructure for reproducible R package environment management from the `renv` package
#' @slot docker creation for project docker infrastructure
#' @slot github creation of a GitHub repository
#' @slot private private GitHub repository
#' @slot github_actions creation of GitHub actions infrastructure
#' @slot parallel_plan an expression to define the parallel future plan to use
#' @slot force force project creation if the project directory already exists

setClass('Project',
         slots = list(
           project_name = 'character',
           path = 'character',
           renv = 'logical',
           docker = 'logical',
           github = 'logical',
           private = 'logical',
           github_actions = 'logical',
           parallel_plan = 'call',
           force = 'logical'
         ),
         prototype = list(
           project_name = 'A project name',
           path = '.',
           renv = TRUE,
           docker = TRUE,
           github = FALSE,
           private = FALSE,
           github_actions = FALSE,
           parallel_plan = rlang::expr(jfmisc::suitableParallelPlan()),
           force = FALSE
         ))

setMethod('show',signature = 'Project',
          function(object){
            cat('Project name:',projectName(object),'\n')
            cat('Directory path:',path(object),'\n')
            cat('Use renv:',renv(object),'\n')
            cat('Docker:',docker(object),'\n')
            cat('GitHub repository:',github(object),'\n')
            cat('Private repository:',private(object),'\n')
            cat('GitHub Actions:',githubActions(object),'\n')
            cat('Parallel plan:',expr_text(parallelPlan(object)),'\n')
            cat('Force creation:',force(object),'\n')
          })

#' `Project` class get and set methods
#' @rdname Project-accessors
#' @description Get and set methods for the `Project` S4 class.
#' @param x S4 object of class `Project`
#' @param value value to set
#' @examples 
#' workflow_project <- defineProject('A metabolomics project')
#' 
#' ## Return the project name
#' projectName(workflow_project)
#' 
#' ## Set the project name
#' projectName(workflow_project) <- 'A new name'
#' 
#' ## Return the project directory path
#' path(workflow_project)
#' 
#' ## Set the project directory path
#' path(workflow_project) <- './a_directory'
#' 
#' ## Return the project renv option
#' renv(workflow_project)
#' 
#' ## Set the project renv option
#' renv(workflow_project) <- FALSE
#' 
#' ## Return the project docker option
#' docker(workflow_project)
#' 
#' ## Set the project docker option
#' docker(workflow_project) <- FALSE
#' 
#' ## Return the project github option
#' github(workflow_project)
#' 
#' ## Set the project github option
#' github(workflow_project) <- TRUE
#' 
#' ## Return the project private option
#' private(workflow_project)
#' 
#' ## Set the project private option
#' private(workflow_project) <- TRUE
#' 
#' ## Return the project github actions option
#' githubActions(workflow_project)
#' 
#' ## Set the project github actions option
#' githubActions(workflow_project) <- TRUE
#'
#' ## Return the expression project parallel plan
#' parallelPlan(workflow_project)
#' 
#' ## Set the expression for the project parallel plan
#' parallelPlan(workflow_project) <- rlang::expr(future::plan(strategy = 'multisession',workers = 2))
#' 
#' ## Return the project force option
#' force(workflow_project)
#' 
#' ## Set the project force option
#' force(workflow_project) <- TRUE
#' @export

setGeneric('projectName',function(x)
  standardGeneric('projectName'))

#' @rdname Project-accessors

setMethod('projectName',signature = 'Project',
          function(x){
            x@project_name
          })

#' @rdname Project-accessors
#' @export

setGeneric('projectName<-',function(x,value)
  standardGeneric('projectName<-'))

#' @rdname Project-accessors

setMethod('projectName<-',signature = 'Project',
          function(x,value){
           x@project_name <- value
           return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('path',function(x)
  standardGeneric('path'))

#' @rdname Project-accessors

setMethod('path',signature = 'Project',
          function(x){
            x@path
          })

#' @rdname Project-accessors
#' @export

setGeneric('path<-',function(x,value)
  standardGeneric('path<-'))

#' @rdname Project-accessors

setMethod('path<-',signature = 'Project',
          function(x,value){
            x@path <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('renv',function(x)
  standardGeneric('renv'))

#' @rdname Project-accessors

setMethod('renv',signature = 'Project',
          function(x){
            x@renv
          })

#' @rdname Project-accessors
#' @export

setGeneric('renv<-',function(x,value)
  standardGeneric('renv<-'))

#' @rdname Project-accessors

setMethod('renv<-',signature = 'Project',
          function(x,value){
            x@renv <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('docker',function(x)
  standardGeneric('docker'))

#' @rdname Project-accessors

setMethod('docker',signature = 'Project',
          function(x){
            x@docker
          })

#' @rdname Project-accessors
#' @export

setGeneric('docker<-',function(x,value)
  standardGeneric('docker<-'))

#' @rdname Project-accessors

setMethod('docker<-',signature = 'Project',
          function(x,value){
            x@docker <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('github',function(x)
  standardGeneric('github'))

#' @rdname Project-accessors

setMethod('github',signature = 'Project',
          function(x){
            x@github
          })

#' @rdname Project-accessors
#' @export

setGeneric('github<-',function(x,value)
  standardGeneric('github<-'))

#' @rdname Project-accessors

setMethod('github<-',signature = 'Project',
          function(x,value){
            x@github <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('private',function(x)
  standardGeneric('private'))

#' @rdname Project-accessors

setMethod('private',signature = 'Project',
          function(x){
            x@private
          })

#' @rdname Project-accessors
#' @export

setGeneric('private<-',function(x,value)
  standardGeneric('private<-'))

#' @rdname Project-accessors

setMethod('private<-',signature = 'Project',
          function(x,value){
            x@private <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('githubActions',function(x)
  standardGeneric('githubActions'))

#' @rdname Project-accessors

setMethod('githubActions',signature = 'Project',
          function(x){
            x@github_actions
          })

#' @rdname Project-accessors
#' @export

setGeneric('githubActions<-',function(x,value)
  standardGeneric('githubActions<-'))

#' @rdname Project-accessors

setMethod('githubActions<-',signature = 'Project',
          function(x,value){
            x@github_actions <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('parallelPlan',function(x)
  standardGeneric('parallelPlan'))

#' @rdname Project-accessors

setMethod('parallelPlan',signature = 'Project',
          function(x){
            x@parallel_plan
          })

#' @rdname Project-accessors
#' @export

setGeneric('parallelPlan<-',function(x,value)
  standardGeneric('parallelPlan<-'))

#' @rdname Project-accessors

setMethod('parallelPlan<-',signature = 'Project',
          function(x,value){
            x@parallel_plan <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('force',function(x)
  standardGeneric('force'))

#' @rdname Project-accessors

setMethod('force',signature = 'Project',
          function(x){
            x@force
          })

#' @rdname Project-accessors
#' @export

setGeneric('force<-',function(x,value)
  standardGeneric('force<-'))

#' @rdname Project-accessors

setMethod('force<-',signature = 'Project',
          function(x,value){
            x@force <- value
            return(x)
          })

#' Define a project directory
#' @description Define the project directory for a workflow
#' @param project_name project name/title
#' @param path target file path for project directory 
#' @param renv add infrastructure for reproducible R package environment management from the `renv` package
#' @param docker TRUE/FALSE. Create project infrastructure for building a docker container to compile the project.
#' @param github TRUE/FALSE. Create a GitHub repository?
#' @param private TRUE/FALSE. Should the GitHub repository be private? Evaluated only if argument `github` is TRUE.
#' @param github_actions TRUE/FALSE. Add Github actions infrastructure? Evaluated only if argument `github` is TRUE.
#' @param parallel_plan An expression denoting the `future` parallel plan to use in the project template. See `future::plan()` for more information on `future` parallel plans.
#' @param force force project creation if project directory already exists
#' @return An S4 object of class `Project`.
#' @examples 
#' workflow_project <- defineProject('A metabolomics project')
#' 
#' workflow_project
#' @importFrom rlang enexpr 
#' @export

defineProject <- function(project_name,
                          path = '.',
                          renv = TRUE,
                          docker = TRUE,
                          github = FALSE,
                          private = FALSE,
                          github_actions = FALSE,
                          parallel_plan = jfmisc::suitableParallelPlan(),
                          force = FALSE){
  
  parallel_plan <- enexpr(parallel_plan)
  
  new('Project',
      project_name = project_name,
      path = path,
      renv = renv,
      docker = docker,
      github = github,
      private = private,
      github_actions = github_actions,
      parallel_plan = parallel_plan,
      force = force)
  
}
