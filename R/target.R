#' S4 class to store target a definition
#' @description An S4 class for storing a workflow target definition.
#' @slot name the target name
#' @slot command the R code to run the target
#' @slot type the target archetype
#' @slot pattern target branching definition
#' @slot args arguments to pass to the specified target archetype
#' @importFrom rlang exprs

setClass('Target',
         slots = list(
           name = 'character',
           command = 'list',
           type = 'character',
           pattern = 'list',
           args = 'list'
         ),
         prototype = list(
           name = 'a_target',
           command = rlang::exprs(1 + 1),
           type = 'tar_target',
           pattern = rlang::exprs(NULL),
           args = list()
         ))

#' @import targets tarchetypes

setValidity('Target',function(object){
  
  if (grepl(' ',name(object))){
    return('The target name should not include spaces.')
  }
  
  target_types <- c(ls(asNamespace('targets')),ls(asNamespace('tarchetypes'))) %>% 
    .[grepl('tar_',.)]
  
  if (!(type(object) %in% target_types)) {
    return(glue('Target type "{type(object)}" unavailable in targets or tarchetypes packages.'))
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

setGeneric('pattern',function(x)
  standardGeneric('pattern'))

#' @rdname Target-accessors

setMethod('pattern',signature = 'Target',
          function(x){
            x@pattern
          })

#' @rdname Target-accessors
#' @export

setGeneric('pattern<-',function(x,value)
  standardGeneric('pattern<-'))

#' @rdname Target-accessors

setMethod('pattern<-',signature = 'Target',
          function(x,value){
            x@pattern <- value
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

setGeneric('code',function(x)
  standardGeneric('code'))

#' @rdname Target-accessors
#' @importFrom glue glue
#' @importFrom purrr map_chr

setMethod('code',signature = 'Target',
          function(x){
            target_arguments <- x %>% 
              args() 
              
            if (length(target_arguments) > 0){
              target_arguments <- target_arguments %>% 
                names() %>% 
                map_chr(~{
                  glue('{.x} = "{target_arguments[[.x]]}"')
                }) %>% 
                paste(collapse = ',\n  ') %>% 
                paste0(',\n  ',.)
            } else {
              target_arguments <- ''
            }
            
            glue('
{type(x)}(
  {name(x)},
  {command(x)}
  pattern = {pattern(x)}{target_arguments}
)
') 
          })

#' Create a workflow target definition
#' @description Create a workflow target definition.
#' @param name the target name
#' @param command the R command to run the target
#' @param type the target archetype
#' @param pattern target branching definition
#' @param args a list of arguments to pass the the specified target archetype
#' @return An S4 object of class Target. 
#' @details 
#' Target types can be one of any provided by the `targets` or `tarchetypes` packages.
#' @examples 
#' workflow_target <- target('a_target',1 + 1,args = list(memory = 'persistent'))
#' 
#' workflow_target
#' @export

target <- function(name,command,type = 'tar_target',pattern = NULL,args = list()){
  target_command <- enexprs(command)
  pattern_command <- enexprs(pattern)
  
  new('Target',
      name = name,
      command = target_command,
      type = type,
      pattern = pattern_command,
      args = args
      )
}

