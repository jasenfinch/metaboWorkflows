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

fingerprinting <- function(x){
  list(
    input = targetsInput(x),
    spectral_processing = targetsSpectralProcessing(x),
    pre_treatment = targetsPretreatment(x),
    MF_assignment = targetsMFassignment(x),
    modelling = targetsModelling(x),
    correlations = targetsCorrelations(x) 
  )
}

LCprofiling <- function(x){
  fingerprinting(x)
}

GCprofiling <- function(x){
  list(
    input = targetsInput(x),
    spectral_processing = targetsSpectralProcessing(x),
    pre_treatment = targetsPretreatment(x),
    modelling = targetsModelling(x),
    correlations = targetsCorrelations(x) 
  )
}

#' @rdname targetsWorkflow

setMethod('targetsWorkflow',signature = 'Workflow',
          function(x){
            workflow_type <- type(x)
            
            workflow_targets <- switch(workflow_type,
              `FIE-HRMS fingerprinting` = fingerprinting(x),
              `NSI-HRMS fingerprinting` = fingerprinting(x),
              `RP-LC-HRMS profiling` = LCprofiling(x),
              `NP-LC-HRMS profiling` = LCprofiling(x),
              `GC-MS profiling` = GCprofiling(x)
            )
            
            return(workflow_targets)
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
                glue('"{instrument(x)}"')
              ),
              experiment = target(
                'experiment',
                glue('"{directory(x)}"')
              ),
              grover_client_config = target(
                'grover_client_config',
                '"misc/grover_client.yml"',
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
                args = list(pattern = 'map(raw_files)',
                            format = 'file')
              ),
              raw_sample_information = target(
                'raw_sample_information',
                'grover::sampleInfo(grover_client,
                                   instrument,
                                   experiment,
                                   raw_files)',
                args = list(pattern = 'map(raw_files)')
              ),
              sample_information = target(
                'sample_information',
                'raw_sample_information %>%
                metaboMisc::convertSampleInfo() %>%
                dplyr::filter(class != "Play",class != "Ctrl")'
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


fingerprintProcessing <- function(){
  list(
    spectral_processing_parameters = target(
      'spectral_processing_parameters',
      'binneR::detectParameters(mzML)'
    ),
    spectral_processed = target(
      'spectral_processed',
      'binneR::binneRlyse(mzML,
                          sample_information,
                          spectral_processing_parameters)',
      args = list(
        memory = 'transient'
      )
    ),
    plot_fingerprint = target(
      'plot_fingerprint',
      'binneR::plotFingerprint(spectral_processed)'
    ),
    plot_chromatogram = target(
      'plot_chromatogram',
      'binneR::plotChromatogram(spectral_processed)'
    ),
    plot_TIC = target(
      'plot_TIC',
      'binneR::plotTIC(spectral_processed)'
    ),
    plot_purity_dist = target(
      'plot_purity_dist',
      'binneR::plotPurity(spectral_processed)'
    ),
    plot_centrality_dist = target(
      'plot_centrality_dist',
      'binneR::plotCentrality(spectral_processed)'
    ),
    summarise_processed_features = target(
      'summarise_processed_features',
      'metaboMisc::featureSummary(spectral_processed)'
    ),
    export_processed_data = target(
      'export_processed_data',
      'metaboMisc::export(spectral_processed,outPath = "exports/spectral_processing")',
      type = 'tar_files'
    )
  )
}

profilingParameters <- function(x){
  parameter_specification <- switch(x,
                                    `RP-LC-HRMS profiling` = 'LCMS-RP',
                                    `NP-LC-HRMS profiling` = 'LCMS-NP')
  target(
    'spectral_processing_parameters',
    glue('profilePro::profileParameters("{parameter_specification}")')
  )
}

profilingProcessing <- function(x){
  list(
    spectral_processing_parameters = profilingParameters(x),
    spectral_processed = target(
      'spectral_processed',
      'profilePro::profileProcess(mzML,
                                  sample_information,
                                  spectral_processing_parameters)'
    )
  )
}

#' @rdname targetsWorkflow

setMethod('targetsSpectralProcessing',signature = 'Workflow',
          function(x){
            
            workflow <- type(x)
            
            processing_workflow <- switch(workflow,
                   `FIE-HRMS fingerprinting` = fingerprintProcessing(),
                   `NSI-HRMS fingerprinting` = fingerprintProcessing(),
                   `RP-LC-HRMS profiling` = profilingProcessing(workflow),
                   `NP-LC-HRMS profiling` = profilingProcessing(workflow),
                   `GC-MS profiling` = profilingProcessing(workflow))
            
            return(processing_workflow)
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
                'metaboMisc::exportData(pre_treated,type = "pre-treated",outPath = "exports/pre-treated")',
                type = 'tar_file'
              ),
              export_pre_treated_sample_info = target(
                'export_pre_treated_sample_info',
                'metaboMisc::exportSampleInfo(pre_treated,outPath = "exports/pre-treated")',
                type = 'tar_file'
              ),
              plot_PCA = target(
                'plot_PCA',
                'metabolyseR::plotPCA(pre_treated)'
              ),
              plot_LDA = target(
                'plot_LDA',
                'metabolyseR::plotLDA(pre_treated)'
              ),
              plot_unsupervised_RF = target(
                'plot_unsupervised_RF',
                'metabolyseR::plotUnsupervisedRF(pre_treated)'
              ),
              plot_supervised_RF = target(
                'plot_supervised_RF',
                'metabolyseR::plotSupervisedRF(pre_treated)'
              )
            )
          })

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsMFassignment',function(x)
  standardGeneric('targetsMFassignment'))


assignmentParameters <- function(x){
  technique <- switch(x,
                      `FIE-HRMS fingerprinting` = 'FIE',
                      `NSI-HRMS fingerprinting` = 'FIE',
                      `RP-LC-HRMS profiling` = 'RP-LC',
                      `NP-LC-HRMS profiling` = 'NP-LC')
  
  target(
    'molecular_formula_assignment_parameters',
    glue('MFassign::assignmentParameters("{technique}")')
  )
}

#' @rdname targetsWorkflow

setMethod('targetsMFassignment',signature = 'Workflow',
          function(x){
            list(
              molecular_formula_assignment_parameters = assignmentParameters(type(x)),
              molecular_formula_assingment = target(
                'molecular_formula_assignment',
                'pre_treated %>% 
                  metabolyseR::dat(type = "pre-treated") %>% 
                  MFassign::assignMFs(molecular_formula_assignment_parameters)', 
                args = list(memory = 'transient')
              ),
              assigned_data = target(
                'assigned_data',
                'metaboMisc::addAssignments(pre_treated,molecular_formula_assignment)'
              ),
              summarise_assignments = target(
                'summarise_assignments',
                'MFassign::summariseAssignment(molecular_formula_assignment)'
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

modellingTargets <- function(x){
  
  object_name <- switch(x,
                        assigned ='assigned_data',
                        unassigned = 'pre_treated')
  
  list(
    modelling_parameters = target(
      'modelling_parameters',
      glue('metaboMisc::detectModellingParameters({object_name},cls = "class")')
    ),
    modelling = target(
      'modelling',
      glue('metabolyseR::reAnalyse({object_name},
                          modelling_parameters)')
    ),
    plot_explanatory_heatmap = target(
      'plot_explanatory_heatmap',
      'metabolyseR::plotExplanatoryHeatmap(modelling)'
    ),
    summarise_modelling_metrics = target(
      'summarise_model_metrics',
      'metabolyseR::metrics(modelling)'
    ),
    summarise_modelling_importance = target(
      'summarise_model_importance',
      'metabolyseR::importance(modelling)'
    ),
    export_modelling = target(
      'export_modelling',
      'metaboMisc::exportModelling(modelling,outPath = "exports/modelling")',
      type = 'tar_files'
    )
  )
}

#' @rdname targetsWorkflow

setMethod('targetsModelling',signature = 'Workflow',
          function(x){
            workflow_type <- type(x)
            
           switch(workflow_type,
                  `FIE-HRMS fingerprinting` = modellingTargets('assigned'),
                  `NSI-HRMS fingerprinting` = modellingTargets('assigned'),
                  `RP-LC-HRMS profiling` = modellingTargets('assigned'),
                  `NP-LC-HRMS profiling` = modellingTargets('assigned'),
                  `GC-MS profiling` = modellingTargets('unassigned')
                  )
          })

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsCorrelations',function(x)
  standardGeneric('targetsCorrelations'))

correlationsTargets <- function(x){
  
  object_name <- switch(x,
                        assigned ='assigned_data',
                        unassigned = 'pre_treated')
  
  list(
    correlations_parameters = target(
      'correlations_parameters',
      'metabolyseR::analysisParameters("correlations")'
    ),
    correlations = target(
      'correlations',
      glue('metabolyseR::reAnalyse({object_name},
                          correlations_parameters)')
    ),
    summarise_correlations = target(
      'summarise_correlations',
      'metabolyseR::analysisResults(correlations,"correlations")'
    ),
    export_correlations = target(
      'export_correlations',
      'metaboMisc::exportCorrelations(correlations,outPath = "exports/correlations")',
      type = 'tar_files'
    )
  )
}

#' @rdname targetsWorkflow

setMethod('targetsCorrelations',signature = 'Workflow',
          function(x){
            workflow_type <- type(x)
            
            switch(workflow_type,
                   `FIE-HRMS fingerprinting` = correlationsTargets('assigned'),
                   `NSI-HRMS fingerprinting` = correlationsTargets('assigned'),
                   `RP-LC-HRMS profiling` = correlationsTargets('assigned'),
                   `NP-LC-HRMS profiling` = correlationsTargets('assigned'),
                   `GC-MS profiling` = correlationsTargets('unassigned')
            )
          })
