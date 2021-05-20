#' S4 class for storing file path input information
#' @description An S4 class for storing file path type workflow input information.
#' @slot file_paths character vector of file paths
#' @slot sample_information a tibble containing sample information

setClass('FilePathInput',
         slots = list(
           file_paths = 'character',
           sample_information = 'tbl_df'
         ))

#' @importFrom magrittr %>% 

setValidity('FilePathInput',function(object){
  
  mzml_pattern <- grepl('.mzML',object@file_paths)
  
  if (FALSE %in% mzml_pattern){
    return('File paths should only contain .mzML files')
  }
  
  necessary_names <- c('fileOrder','injOrder','fileName','batch','block','name','class')
  
  info_names <- object %>%
    .@sample_information %>%
    colnames()
  
  presence <- necessary_names %in% info_names
  
  if (FALSE %in% presence) {
    paste0('Sample information should contain the following column names: ',
           paste0(necessary_names,collapse = ', '),
           '.') %>% 
      return()
  } 
  
  return(TRUE)
})

setMethod('show',signature = 'FilePathInput',
          function(object){
            cat('File path workflow input\n')
            cat('# files:',length(object@file_paths))
          })

#' `FilePathInput` get and set accessors
#' @rdname FilePathInput-accessors
#' @description Get and set methods for the `FilePathInput` S4 class.
#' @param x S4 object of class `FilePathInput`
#' @param value value to set
#' @export

setGeneric('filePaths',
           function(x)
             standardGeneric('filePaths'))

#' @rdname FilePathInput-accessors

setMethod('filePaths',signature = 'FilePathInput',
          function(x){
            x@file_paths
          })

#' @rdname FilePathInput-accessors
#' @export

setGeneric('filePaths<-',
           function(x,value)
             standardGeneric('filePaths<-'))

#' @rdname FilePathInput-accessors

setMethod('filePaths<-',signature = 'FilePathInput',
          function(x,value){
            x@file_paths <- value
            return(x)
          })

#' @rdname FilePathInput-accessors
#' @export

setGeneric('sampleInformation',
           function(x)
             standardGeneric('sampleInformation'))

#' @rdname FilePathInput-accessors

setMethod('sampleInformation',signature = 'FilePathInput',
           function(x){
             x@sample_information
           })

#' @rdname FilePathInput-accessors
#' @export

setGeneric('sampleInformation<-',
           function(x,value)
             standardGeneric('sampleInformation<-'))

#' @rdname FilePathInput-accessors

setMethod('sampleInformation<-',signature = 'FilePathInput',
           function(x,value){
             x@sample_information <- value
             return(x)
           })

#' Workflow input using file paths
#' @description Use file paths to .mzML data files for workflow input.
#' @param file_paths character vector of .mzML file paths
#' @param sample_information a tibble containing sample information. See details for specifications
#' @return An S4 object of class `FilePathInput`.
#' @details 
#' The tibble containing sample information should at least contain the following columns:
#' * `fileOrder` - the numeric file order of the input files when order alphabetically as returned by list.files().
#' * `injOrder` - the sample injection order during MS analysis
#' * `fileName` - the sample file name
#' * `batch` - the analytical batch number
#' * `block` - the randomised block number
#' * `name` - the sample name
#' * `class` - the sample class name
#' @examples
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' workflow_input <- inputFilePath(file_paths,sample_information)
#' 
#' workflow_input
#' @export

inputFilePath <- function(file_paths,sample_information){
  new('FilePathInput',
      file_paths = file_paths,
      sample_information = sample_information) 
}

#' S4 class for storing grover input information
#' @description An S4 class for storing grover type workflow input information.
#' @slot instrument analytical instrument name
#' @slot directory experiment directory name
#' @slot host grover API host address
#' @slot port grover API host port
#' @slot auth grover API host authentication code

setClass('GroverInput',
         slots = list(
           instrument = 'character',
           directory = 'character',
           host = 'character',
           port = 'numeric',
           auth = 'character'
         ))

setMethod('show',signature = 'GroverInput',
          function(object){
            cat('Grover API workflow input','\n') 
            cat('Instrument:',object@instrument,'\n')
            cat('Directory:',object@directory,'\n')
            cat('Host:',object@host,'\n')
            cat('Port:',object@port,'\n')
            cat('Authentication:',object@auth)
          })

#' `GroverInput` get and set methods
#' @rdname GroverInput-accessors
#' @description Get and set methods for the `GroverInput` S4 class.
#' @param x S4 object of class `GroverInput`
#' @param value value to set
#' @export

setGeneric('instrument',
           function(x)
             standardGeneric('instrument'))

#' @rdname GroverInput-accessors

setMethod('instrument',signature = 'GroverInput',
          function(x){
            x@instrument
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('instrument<-',
           function(x,value)
             standardGeneric('instrument<-'))

#' @rdname GroverInput-accessors

setMethod('instrument<-',signature = 'GroverInput',
          function(x,value){
            x@instrument <- value
            return(x)
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('directory',
           function(x)
             standardGeneric('directory'))

#' @rdname GroverInput-accessors

setMethod('directory',signature = 'GroverInput',
          function(x){
            x@directory
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('directory<-',
           function(x,value)
             standardGeneric('directory<-'))

#' @rdname GroverInput-accessors

setMethod('directory<-',signature = 'GroverInput',
          function(x,value){
            x@directory <- value
            return(x)
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('host',
           function(x)
             standardGeneric('host'))

#' @rdname GroverInput-accessors

setMethod('host',signature = 'GroverInput',
          function(x){
            x@host
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('host<-',
           function(x,value)
             standardGeneric('host<-'))

#' @rdname GroverInput-accessors

setMethod('host<-',signature = 'GroverInput',
          function(x,value){
            x@host <- value
            return(x)
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('port',
           function(x)
             standardGeneric('port'))

#' @rdname GroverInput-accessors

setMethod('port',signature = 'GroverInput',
          function(x){
            x@port
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('port<-',
           function(x,value)
             standardGeneric('port<-'))

#' @rdname GroverInput-accessors

setMethod('port<-',signature = 'GroverInput',
          function(x,value){
            x@port <- value
            return(x)
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('auth',
           function(x)
             standardGeneric('auth'))

#' @rdname GroverInput-accessors

setMethod('auth',signature = 'GroverInput',
          function(x){
            x@auth
          })

#' @rdname GroverInput-accessors
#' @export

setGeneric('auth<-',
           function(x,value)
             standardGeneric('auth<-'))

#' @rdname GroverInput-accessors

setMethod('auth<-',signature = 'GroverInput',
          function(x,value){
            x@auth <- value
            return(x)
          })

#' Workflow input using a grover API
#' @description Use .mzML files retrieved from a grover API for workflow input. 
#' @param instrument analytical instrument name
#' @param directory experiment directory name
#' @param host grover API host address
#' @param port grover API host port
#' @param auth grover API host authentication code
#' @return An S4 object of class GroverInput
#' @details This specifies the retrieval of sample data files and information using a grover API. See [`grover`](jasenfinch.github.io/grover/) for more information about mass spectral data retrieval from a grover API.
#' @export

inputGrover <- function(instrument,directory,host,port,auth){
  
  new('GroverInput',
      instrument = instrument,
      directory = directory,
      host = host,
      port = port,
      auth = auth)
}