#' Workflow pipeline targets
#' @rdname targetsWorkflow
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
#' targetsWorkflow(workflow_definition)
#' @export

setGeneric('targetsWorkflow',function(x)
  standardGeneric('targetsWorkflow'))

#' @rdname targetsWorkflow
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

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsInput',function(x)
  standardGeneric('targetsInput'))

#' @rdname targetsWorkflow

setMethod('targetsInput',signature = 'FilePathInput',
          function(x){
            list(
              file_paths_list = target('file_paths_list',
                                       '"data/file_paths.txt"',
                                       type = 'tar_file'),
              mzML = target('mzML',
                            'readLines(file_paths_list)',
                            type = 'tar_files'),
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

#' @rdname targetsWorkflow

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
              mzML = target(
                'mzML',
                'grover::convertFile(grover_client,
                                    instrument,
                                    experiment,
                                    raw_files,
                                    args = grover::conversionArgsPeakPick(),
                                    outDir = "data/mzML") %>% 
                  .[!grepl("Ctrl",.)] %>%
                  .[!grepl("Play",.)]',
                args = list(pattern = 'map(raw_files)')
              ),
              raw_sample_information = target(
                'raw_sample_information',
                'grover::sampleInfo(grover_client,
                                   instrument,
                                   experiment,
                                   raw_files))',
                args = list(pattern = 'map(raw_files)')
              ),
              sample_information = target(
                'sample_information',
                'raw_sample_information %>%
                metaboMisc::convertSampleInfo() %>%
                dplyr::filter(class != "Play",class != "Ctrl")'
              ),
              export_sample_information = target(
                'export_sample_information',
                "write.csv('data/runinfo.csv',row.names = FALSE)",
                type = 'tar_file'
              )
            )
          })

#' @rdname targetsWorkflow

setMethod('targetsInput',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              targetsInput()
          })

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsSpectralProcessing',function(x)
  standardGeneric('targetsSpectralProcessing'))


#' @rdname targetsWorkflow

setMethod('targetsSpectralProcessing',signature = 'Workflow',
          function(x){
            
            workflow <- type(x)
            
            processing_workflows <- list(
              `FIE-HRMS fingerprinting` = list(
                spectral_processing_parameters = target(
                  'spectral_processing_parameters',
                  'binneR::detectParameters(mzML)'
                ),
                spectral_processed = target(
                  'spectral_processed',
                  'binneR::binneRlyse(mzML,
                                     sample_information,
                                     spectral_processing_parameters)'
                ),
                export_processed_data = target(
                  'export_processed_data',
                  'metaboMisc::export(spectral_processed,outPath = "exports/spectral_processing")',
                  type = 'tar_files')
              )
            )
            
            return(processing_workflows[[workflow]])
          })

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsPretreatment',function(x)
  standardGeneric('targetsPretreatment'))

#' @rdname targetsWorkflow

setMethod('targetsPretreatment',signature = 'Workflow',
          function(x){
            list(
              pre_treatment_parameters = target(
                'pre_treatment_parameters',
                'metaboMisc::detectPretreatmentParameters(spectral_processed)'
              ),
              pre_treated = target(
                'pre_treated',
                'metaboMisc::preTreatModes(spectral_processed,
                                          pre_treatment_parameters)'
              ),
              export_pre_treated_data = target(
                'export_pre_treated',
                'metaboMisc::exportData(pre_treated,outPath = "exports/pre-treated")',
                type = 'tar_file'
              ),
              export_pre_treated_sample_info = target(
                'export_pre_treated_sample_info',
                'metaboMisc::exportSampleInfo(pre_treated,outPath = "exports/pre-treated")',
                type = 'tar_file'
              )
            )
          })

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsMFassignment',function(x)
  standardGeneric('targetsMFassignment'))

#' @rdname targetsWorkflow

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
              ),
              export_assignments = target(
                'export_assignments',
                'metaboMisc::export(molecular_formula_assignment,outPath = "exports/molecular_formula_assignments")',
                type = 'tar_files'
              )
            )
          })

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsModelling',function(x)
  standardGeneric('targetsModelling'))

#' @rdname targetsWorkflow

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
              ),
              export_modelling = target(
                'export_modelling',
                'metabolyseR::exportModelling(modelling,outPath = "exports/modelling")',
                type = 'tar_files'
              )
            )
          })

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsCorrelations',function(x)
  standardGeneric('targetsCorrelations'))

#' @rdname targetsWorkflow

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
              ),
              export_correlations = target(
                'export_correlations',
                'metaboMisc::exportCorrelations(correlations,outPath = "exports/correlations")',
                type = 'tar_files'
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
