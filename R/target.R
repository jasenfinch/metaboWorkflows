#' S4 class to store a target definition
#' @description An S4 class for storing a workflow target definition.
#' @slot name the target name
#' @slot command the R code to run the target as a string
#' @slot type the target archetype
#' @slot args arguments to pass to the specified target archetype
#' @importFrom rlang exprs

setClass('Target',
         slots = list(
           name = 'character',
           command = 'list',
           type = 'character',
           args = 'list',
           comment = 'character'
         ),
         prototype = list(
           name = 'a_target',
           command = list(1 + 1),
           type = 'tar_target',
           args = list(),
           comment = character()
         ))

#' @import targets tarchetypes

setValidity('Target',function(object){
  
  if (grepl(' ',name(object))){
    return('The target name should not include spaces.')
  }
  
  target_types <- c(ls(asNamespace('targets'))) %>% 
    .[grepl('tar_',.)]
  
  if (!grepl('::',type(object)) & !(type(object) %in% target_types)) {
    return(glue('Target type "{type(object)}" unavailable in the targets package.'))
  }
  
  return(TRUE)
})

setMethod('show',signature = 'Target',
          function(object){
            object %>%
              code() %>% 
              print()
          })

#' `Target` class get and set methods
#' @rdname Target-accessors
#' @description Get and set methods for the Target S4 class.
#' @param x S4 object of class Target
#' @param value value to set
#' @examples 
#' workflow_target <- target('a_target',
#'                           1 + 1,
#'                           args = list(memory = 'persistent'), 
#'                           comment = 'A target')
#' 
#' ## Return the target name
#' name(workflow_target)
#' 
#' ## Set the target name
#' name(workflow_target) <- 'a_new_name'
#' 
#' ## Return the target R code
#' command(workflow_target)
#' 
#' ## Set the target R code
#' command(workflow_target) <- rlang::expr(1 * 2)
#' 
#' ## Return the target type
#' type(workflow_target)
#' 
#' ## Set the target type
#' type(workflow_target) <- 'tarchetypes::tar_file'
#' 
#' ## Return the list target arguments
#' args(workflow_target)
#' 
#' ## Set the target arguments
#' args(workflow_target) <- list(error = 'continue')
#' 
#' ## Return the target comment
#' comment(workflow_target)
#' 
#' ## Set the workflow comment
#' comment(workflow_target) <- 'A new comment'
#' @export

setGeneric('name',function(x)
  standardGeneric('name'))

#' @rdname Target-accessors

setMethod('name',signature = 'Target',
          function(x){
            x@name
          })

#' @rdname Target-accessors
#' @export

setGeneric('name<-',function(x,value)
  standardGeneric('name<-'))

#' @rdname Target-accessors

setMethod('name<-',signature = 'Target',
          function(x,value){
            x@name <- value
            return(x)
          })

#' @rdname Target-accessors
#' @export

setGeneric('command',function(x)
  standardGeneric('command'))

#' @rdname Target-accessors

setMethod('command',signature = 'Target',
          function(x){
            x@command
          })

#' @rdname Target-accessors
#' @export

setGeneric('command<-',function(x,value)
  standardGeneric('command<-'))

#' @rdname Target-accessors
#' @importFrom rlang enexprs

setMethod('command<-',signature = 'Target',
          function(x,value){
            value <- enexprs(value)
            
            x@command <- value
            return(x)
          })

#' @rdname Target-accessors
#' @export

setGeneric('type',function(x)
  standardGeneric('type'))

#' @rdname Target-accessors

setMethod('type',signature = 'Target',
          function(x){
            x@type
          })

#' @rdname Target-accessors
#' @export

setGeneric('type<-',function(x,value)
  standardGeneric('type<-'))

#' @rdname Target-accessors

setMethod('type<-',signature = 'Target',
          function(x,value){
            x@type <- value
            return(x)
          })

#' @rdname Target-accessors
#' @export

setGeneric('args',function(x)
  standardGeneric('args'))

#' @rdname Target-accessors

setMethod('args',signature = 'Target',
          function(x){
            x@args
          })

#' @rdname Target-accessors
#' @export

setGeneric('args<-',function(x,value)
  standardGeneric('args<-'))

#' @rdname Target-accessors

setMethod('args<-',signature = 'Target',
          function(x,value){
            x@args <- value
            return(x)
          })

#' @rdname Target-accessors
#' @export

setGeneric('comment',function(x)
  standardGeneric('comment'))

#' @rdname Target-accessors

setMethod('comment',signature = 'Target',
          function(x){
            x@comment
          })

#' @rdname Target-accessors
#' @export

setGeneric('comment<-',function(x,value)
  standardGeneric('comment<-'))

#' @rdname Target-accessors

setMethod('comment<-',signature = 'Target',
          function(x,value){
            x@comment <- value
            return(x)
          })


#' @rdname Target-accessors
#' @export

setGeneric('code',function(x)
  standardGeneric('code'))

#' @rdname Target-accessors
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom styler style_text
#' @importFrom rlang expr_text

setMethod('code',signature = 'Target',
          function(x){
            
            target_arguments <- x %>% 
              args() 
            
            if (length(target_arguments) > 0){
              target_arguments <- target_arguments %>% 
                names() %>% 
                map_chr(~{
                  if (.x == 'pattern') {
                    glue('{.x} = {target_arguments[[.x]]}')
                  } else {
                    glue('{.x} = "{target_arguments[[.x]]}"') 
                  }
                }) %>% 
                paste(collapse = ',\n  ') %>% 
                paste0(',\n  ',.)
            } else {
              target_arguments <- ''
            }
            
            target_command <- x %>%
              command() %>%
              map(expr_text)
            
            target_code <- glue('
{type(x)}(
  {name(x)},
  {target_command}{target_arguments}
)
') 

  if (length(comment(x)) > 0){
    target_comment <- x %>% 
      comment() %>% 
      {glue('## {.}')}
  
    target_code <- glue('{target_comment}
                                  {target_code}')
  }

  target_code <- style_text(target_code)

  return(target_code)
  }
)

#' Create a workflow target definition
#' @description Create a workflow target definition.
#' @param name the target name
#' @param command the R command to run the target
#' @param type the target archetype
#' @param args a list of arguments to pass the the specified target archetype
#' @param comment optional comment that precedes the target code
#' @return An S4 object of class Target. 
#' @details 
#' The specified target `type` can be one of any provided by the `targets` package such as `tar_target`. 
#' For target archetypes outside of `targets`, declare the package directly for argument 
#' `type`. For instance, `type = "tarchetypes::tar_file"`. 
#' @examples 
#' workflow_target <- target('a_target',
#'                           1 + 1,
#'                           args = list(memory = 'persistent'), 
#'                           comment = 'A target')
#' 
#' workflow_target
#' @export

target <- function(name,command,type = 'tar_target',args = list(),comment = character()){
  
  command <- enexprs(command)
  
  new('Target',
      name = name,
      command = command,
      type = type,
      args = args,
      comment = comment
  )
}

