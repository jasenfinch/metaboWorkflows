#' Workflow pipeline targets
#' @rdname workflowTargets
#' @description Target definitions for workflow input.
#' @param x S4 object of class `FilePathInput` or `GroverInput`
#' @return A list of `Target` S4 class target definitions.
#' @examples 
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- filePathInput(file_paths,sample_information)
#'
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                      'FIE-HRMS fingerprinting',
#'                                      'Example project')
#'
#' workflowTargets(workflow_input)
#' @export

setGeneric('workflowTargets',function(x)
  standardGeneric('workflowTargets'))

#' @rdname workflowTargets
#' 
setMethod('targetsWorkflow',signature = 'Workflow',
          function(x){
            list(
              input = targetsInput(x),
              spectral_processing = targetsSpectralProcessing(x),
              pre_treatment = targetsPretreatment(x),
              MF_assignment = targetsMFassignment(x),
              modelling = targetsModelling(x),
              correlations = targetsCorrelations(x)
            )
          })

#' @rdname workflowTargets
#' @export

setGeneric('targetsInput',function(x)
  standardGeneric('targetsInput'))

#' @rdname workflowTargets

setMethod('targetsInput',signature = 'FilePathInput',
          function(x){
            list(
              file_paths_list = target('file_paths_list',
                                       '"data/file_paths.txt"',
                                       type = 'tar_file'),
              converted_files = target('converted_files',
                                       'readLines(file_paths_list)',
                                       type = 'tar_file'),
              sample_information_file = target(
                'sample_information_file',
                '"data/runinfo.csv"',
                type = 'tar_file'
              ),
              sample_information = target(
                'sample_information',
                "readr::read_csv(sample_information_file)"
              ))
          })

#' @rdname workflowTargets

setMethod('targetsInput',signature = 'GroverInput',
          function(x){
            list(
              instrument = target(
                'instrument',
                instrument(x)
              ),
              experiment = target(
                'experiment',
                directory(x)
              ),
              grover_client_config = target(
                'grover_client_config',
                '"data/grover_client.yml"',
                type = 'tar_file'
              ),
              grover_client = target(
                'grover_client',
                'grover::readGrover(grover_client_config)'
              ),
              raw_files = target(
                'raw_files',
                'grover::listRawFiles(grover_client,
                             instrument,
                             experiment)'
              ),
              converted_files = target(
                'converted_files',
                'grover::convertFile(grover_client,
                                    instrument,
                                    experiment,
                                    raw_files,
                                    args = grover::conversionArgsPeakPick(),
                                    outDir = "data/mzML") %>% 
                  .[!grepl("Ctrl",.)] %>%
                  .[!grepl("Play",.)]',
                pattern = 'map(raw_files)'
              ),
              raw_sample_information = target(
                'raw_sample_information',
                'grover::sampleInfo(grover_client,
                                   instrument,
                                   experiment,
                                   raw_files))',
                pattern = 'map(raw_files)'
              ),
              sample_information = target(
                'sample_information',
                'raw_sample_information %>%
                metaboMisc::convertSampleInfo() %>%
                dplyr::filter(class != "Play",class != "Ctrl")'
              )
            )
          })

#' @rdname workflowTargets

setMethod('targetsInput',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              inputTargets()
          })

#' @rdname workflowTargets
#' @export

setGeneric('targetsSpectralProcessing',function(x)
  standardGeneric('targetsSpectralProcessing'))


#' @rdname workflowTargets

setMethod('targetsSpectralProcessing',signature = 'Workflow',
          function(x){
            
            workflow <- type(x)
            
            processing_workflows <- list(
              `FIE-HRMS fingerprinting` = list(
                spectral_processing_parameters = target(
                  'spectral_processing_parameters',
                  'binneR::detectParameters(converted_files)'
                ),
                spectral_processing = target(
                  'spectral_processing',
                  'binneR::binneRlyse(converted_files,
                                     sample_information,
                                     spectral_processing_parameters)'
                )
              )
            )
            
            return(processing_workflows[[workflow]])
          })

#' @rdname workflowTargets
#' @export

setGeneric('targetsPretreatment',function(x)
  standardGeneric('targetsPretreatment'))

#' @rdname workflowTargets

setMethod('targetsPretreatment',signature = 'Workflow',
          function(x){
            list(
              pre_treatment_parameters = target(
                'pre_treatment_parameters',
                'metaboMisc::detectPretreatmentParameters(spectral_processing)'
              ),
              pre_treated = target(
                'pre_treated',
                'metaboMisc::preTreatModes(spectral_processing,
                                          pre_treatment_parameters)'
              )
            )
          })

#' @rdname workflowTargets
#' @export

setGeneric('targetsMFassignment',function(x)
  standardGeneric('targetsMFassignment'))

#' @rdname workflowTargets

setMethod('targetsMFassignment',signature = 'Workflow',
          function(x){
            list(
              molecular_formula_assignment_parameters = target(
                'molecular_formula_assignment_parameters',
                'MFassign::assignmentParameters("FIE")'
              ),
              molecular_formula_assingment = target(
                'molecular_formula_assignment',
                'pre_treated %>% 
                  metabolyseR::dat(type = "pre-treated") %>% 
                  MFassign::assignMFs(molecular_formula_assignment_parameters)'
              ),
              assigned_data = target(
                'assigned_data',
                'metaboMisc::addAssignments(pre_treated,molecular_formula_assignment)'
              )
            )
          })

#' @rdname workflowTargets
#' @export

setGeneric('targetsModelling',function(x)
  standardGeneric('targetsModelling'))

#' @rdname workflowTargets

setMethod('targetsModelling',signature = 'Workflow',
          function(x){
            list(
              modelling_parameters = target(
                'modelling_parameters',
                'metaboMisc::detectModellingParameters(assigned_data,cls = "class")'
              ),
              modelling = target(
                'modelling',
                'metabolyseR::reAnalyse(assigned_data,
                          modelling_parameters)'
              )
            )
          })

#' @rdname workflowTargets
#' @export

setGeneric('targetsCorrelations',function(x)
  standardGeneric('targetsCorrelations'))

#' @rdname workflowTargets

setMethod('targetsCorrelations',signature = 'Workflow',
          function(x){
            list(
              correlations_parameters = target(
                'correlations_parameters',
                'metabolyseR::analysisParameters("correlations")'
              ),
              correlations = target(
                'correlations',
                'metabolyseR::reAnalyse(assigned_data,
                          correlations_parameters)'
              )
            )
          })

#' Visualise the workflow targets
#' @rdname glimpse
#' @description Visualise the directed acyclic graph of a workflow targets.
#' @param x S4 object of class Workflow
#' @examples 
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- filePathInput(file_paths,sample_information)
#' 
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                       'FIE-HRMS fingerprinting',
#'                                       'Example project')
#'                  
#' glimpse(workflow_definition)
#' @export

setGeneric('glimpse',function(x)
  standardGeneric('glimpse'))

#' @rdname glimpse
#' @importFrom targets tar_dir tar_script tar_glimpse
#' @importFrom rlang parse_exprs eval_tidy

setMethod('glimpse',signature = 'Workflow',
          function(x){
            graph <-  glue('targets::tar_dir({{
                              targets::tar_script({{
                                library(tarchetypes)
                                {x %>%
                                  targets() %>% 
                                  targetsList()}
                                }}, ask = FALSE)
                              targets::tar_glimpse()
                            }})') %>% 
              parse_exprs() %>% 
              map(eval_tidy)
            
            print(graph[[1]])
          })
