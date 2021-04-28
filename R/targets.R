#' Workflow input targets
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
#' inputTargets(workflow_input)
#' @export

setGeneric('workflowTargets',function(x)
  standardGeneric('workflowTargets'))

#' @rdname workflowTargets
#' 
setMethod('workflowTargets',signature = 'Workflow',
          function(x){
            list(
              input = inputTargets(x),
              spectral_processing = spectralProcessingTargets(x),
              pre_treatment = pretreatmentTargets(x),
              MF_assignment = MFassignmentTargets(x),
              modelling = modellingTargets(x),
              correlations = correlationsTargets(x)
            )
          })

#' @rdname workflowTargets
#' @export

setGeneric('inputTargets',function(x)
  standardGeneric('inputTargets'))

#' @rdname workflowTargets

setMethod('inputTargets',signature = 'FilePathInput',
          function(x){
            list(
              file_paths_list = target('file_paths_list',
                                       '"data/file_paths.txt"',
                                       type = 'tar_file'),
              converted_files = target('converted_files',
                                       'readLines(file_path_list)'),
              sample_information_file = target(
                'sample_information_file',
                '"data/runinfo.csv"',
                type = 'tar_file'
              ),
              sample_information = target(
                'sample_information',
                "readr::read_csv('runinfo.csv')"
              ))
          })

#' @rdname workflowTargets

setMethod('inputTargets',signature = 'GroverInput',
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
                                    args = conversionArgsPeakPick(),
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

setMethod('inputTargets',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              inputTargets()
          })

#' @rdname workflowTargets
#' @export

setGeneric('spectralProcessingTargets',function(x)
  standardGeneric('spectralProcessingTargets'))


#' @rdname workflowTargets

setMethod('spectralProcessingTargets',signature = 'Workflow',
          function(x){
            
            workflow <- type(x)
            
            processing_workflows <- list(
              `FIE-HRMS fingerprinting` = list(
                spectral_processing_parameters = target(
                  'spectral_processing_parameters',
                  'binneR::detectParameters(files)'
                ),
                spectral_processing = target(
                  'spectral_processing',
                  'binneR::binneRlyse(files,
                                     sample_information,
                                     spectral_binning_parameters)'
                )
              )
            )
            
            return(processing_workflows[[workflow]])
          })

#' @rdname workflowTargets
#' @export

setGeneric('pretreatmentTargets',function(x)
  standardGeneric('pretreatmentTargets'))

#' @rdname workflowTargets

setMethod('pretreatmentTargets',signature = 'Workflow',
          function(x){
            list(
              pre_treatment_parameters = target(
                'pre_treatment_parameters',
                'metaboMisc::detectPretreatmentParameters(spectral_binning)'
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

setGeneric('MFassignmentTargets',function(x)
  standardGeneric('MFassignmentTargets'))

#' @rdname workflowTargets

setMethod('MFassignmentTargets',signature = 'Workflow',
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

setGeneric('modellingTargets',function(x)
  standardGeneric('modellingTargets'))

#' @rdname workflowTargets

setMethod('modellingTargets',signature = 'Workflow',
          function(x){
            list(
              modelling_parameters = target(
                'modelling_parameters',
                'detectModellingParameters(assigned_data,cls = "class")'
              ),
              modelling = target(
                'modelling',
                'reAnalyse(assigned_data,
                          modelling_parameters)'
              )
            )
          })

#' @rdname workflowTargets
#' @export

setGeneric('correlationsTargets',function(x)
  standardGeneric('correlationsTargets'))

#' @rdname workflowTargets

setMethod('correlationsTargets',signature = 'Workflow',
          function(x){
            list(
              correlations_parameters = target(
                'correlations_parameters',
                'analysisParameters("correlations")'
              ),
              correlations = target(
                'correlations',
                'reAnalyse(assigned_data,
                          correlations_parameters)'
              )
            )
          })
