#' Workflow pipeline targets
#' @rdname targetsWorkflow
#' @description Target definitions for workflow input.
#' @param x the workflow type or an S4 object of class `Workflow`, `FilePathInput` or `GroverInput`
#' @return A list of `Target` S4 class target definitions.
#' @examples 
#' ## Full workflow example
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- inputFilePath(file_paths,sample_information)
#'
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                      'FIE-HRMS fingerprinting',
#'                                      'Example project')
#'
#' targetsWorkflow(workflow_definition)
#' 
#' ## Examples for individual modules
#' targetsSpectralProcessing('FIE-HRMS fingerprinting')
#' targetsInput(workflow_input)
#' targetsPretreatment('FIE-HRMS fingerprinting')
#' targetsMFassignment('FIE-HRMS fingerprinting')
#' targetsModelling('FIE-HRMS fingerprinting')
#' targetsCorrelations('FIE-HRMS fingerprinting')
#' @export

setGeneric('targetsWorkflow',function(x)
  standardGeneric('targetsWorkflow'))

fingerprinting <- function(x){
  workflow_type <- type(x)
  
  list(
    input = targetsInput(x),
    spectral_processing = targetsSpectralProcessing(workflow_type),
    pre_treatment = targetsPretreatment(workflow_type),
    molecular_formula_assignment = targetsMFassignment(workflow_type),
    modelling = targetsModelling(workflow_type),
    correlations = targetsCorrelations(workflow_type),
    report = targetsReport(workflow_type)
  )
}

LCprofiling <- function(x){
  fingerprinting(x)
}

GCprofiling <- function(x){
  workflow_type <- type(x)
  
  list(
    input = targetsInput(x),
    spectral_processing = targetsSpectralProcessing(workflow_type),
    pre_treatment = targetsPretreatment(workflow_type),
    modelling = targetsModelling(workflow_type),
    correlations = targetsCorrelations(workflow_type),
    report = targetsReport(workflow_type)
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
                                       type = 'tar_file',
                                       comment = 'Retrieve data file paths'),
              mzML = target('mzML',
                            'readLines(file_paths_list)',
                            type = 'tar_files',
                            comment = 'Track individual data files'),
              sample_information_file = target(
                'sample_information_file',
                '"data/runinfo.csv"',
                type = 'tar_file',
                comment = 'Sample information file path'
              ),
              sample_information = target(
                'sample_information',
                "readr::read_csv(sample_information_file)",
                comment = 'Parse sample information'
              ))
          })

#' @rdname targetsWorkflow

setMethod('targetsInput',signature = 'GroverInput',
          function(x){
            list(
              instrument = target(
                'instrument',
                glue('"{instrument(x)}"'),
                comment = 'Instrument name'
              ),
              experiment = target(
                'experiment',
                glue('"{directory(x)}"'),
                comment = 'Experiment raw data directory name'
              ),
              grover_client_config = target(
                'grover_client_config',
                '"misc/grover_client.yml"',
                type = 'tar_file',
                comment = 'File path to grover API host information'
              ),
              grover_client = target(
                'grover_client',
                'grover::readGrover(grover_client_config)',
                comment = 'Parse grover API host information'
              ),
              raw_files = target(
                'raw_files',
                'grover::listRawFiles(grover_client,
                             instrument,
                             experiment) %>% 
                  .[!grepl("Ctrl",.)] %>%
                  .[!grepl("Play",.)]',
                comment = 'Retrieve available raw data files, excluding control and play samples'
              ),
              mzML = target(
                'mzML',
                'grover::convertFile(grover_client,
                                    instrument,
                                    experiment,
                                    raw_files,
                                    args = grover::conversionArgsPeakPick(),
                                    outDir = "data/mzML")',
                args = list(pattern = 'map(raw_files)',
                            format = 'file'),
                comment = 'Retrieve converted raw data files in mzML format via grover API'
              ),
              raw_sample_information = target(
                'raw_sample_information',
                'grover::sampleInfo(grover_client,
                                   instrument,
                                   experiment,
                                   raw_files)',
                args = list(pattern = 'map(raw_files)'),
                comment = 'Retrieve sample information from grover API'
              ),
              sample_information = target(
                'sample_information',
                'raw_sample_information %>%
                metaboMisc::convertSampleInfo()',
                comment = 'Convert sample inforrmation table into workflow compatible format'
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

fingerprintProcessing <- function(){
  list(
    spectral_processing_parameters = target(
      'spectral_processing_parameters',
      'binneR::detectParameters(mzML)',
      comment = 'Detect spectral binning parameters'
    ),
    spectral_processed = target(
      'spectral_processed',
      'binneR::binneRlyse(mzML,
                          sample_information,
                          spectral_processing_parameters)',
      args = list(
        memory = 'transient'
      ),
      comment = 'Perform spectral binning'
    ),
    plot_fingerprint = target(
      'plot_fingerprint',
      'binneR::plotFingerprint(spectral_processed)',
      comment = 'Plot average spectrum fingerprint'
    ),
    plot_chromatogram = target(
      'plot_chromatogram',
      'binneR::plotChromatogram(spectral_processed)',
      comment = 'Plot average infusion chromatogram'
    ),
    plot_TIC = target(
      'plot_TIC',
      'binneR::plotTIC(spectral_processed)',
      comment = 'Plot sample total ion counts by randomised block'
    ),
    plot_purity_dist = target(
      'plot_purity_dist',
      'binneR::plotPurity(spectral_processed)',
      comment = 'Plot bin purity distribution'
    ),
    plot_centrality_dist = target(
      'plot_centrality_dist',
      'binneR::plotCentrality(spectral_processed)',
      comment = 'Plot bin centrality distribution'
    ),
    summarise_processed_features = target(
      'summarise_processed_features',
      'metaboMisc::featureSummary(spectral_processed)',
      comment = 'Summarise spectrally binned features'
    ),
    export_processed_data = target(
      'export_processed_data',
      'metaboMisc::export(spectral_processed,outPath = "exports/spectral_processing")',
      type = 'tar_files',
      comment = 'Export spectrally binned data'
    )
  )
}

profilingParameters <- function(x){
  parameter_specification <- switch(x,
                                    `RP-LC-HRMS profiling` = 'LCMS-RP',
                                    `NP-LC-HRMS profiling` = 'LCMS-NP')
  target(
    'spectral_processing_parameters',
    glue('profilePro::profileParameters("{parameter_specification}")'),
    comment = 'Generate spectral processing parameters'
  )
}

profilingProcessing <- function(x){
  list(
    spectral_processing_parameters = profilingParameters(x),
    spectral_processed = target(
      'spectral_processed',
      'profilePro::profileProcess(mzML,
                                  sample_information,
                                  spectral_processing_parameters)',
      comment = 'Perform spectral processing'
    )
  )
}

#' @rdname targetsWorkflow
#' @export

targetsSpectralProcessing <- function(x){
  processing_workflow <- switch(x,
                                `FIE-HRMS fingerprinting` = fingerprintProcessing(),
                                `NSI-HRMS fingerprinting` = fingerprintProcessing(),
                                `RP-LC-HRMS profiling` = profilingProcessing(x),
                                `NP-LC-HRMS profiling` = profilingProcessing(x),
                                `GC-MS profiling` = profilingProcessing(x))
  
  return(processing_workflow)
}

#' @rdname targetsWorkflow
#' @export

targetsPretreatment <- function(x){
  workflow <- checkWorkflow(x)
  
  list(
    pre_treatment_parameters = target(
      'pre_treatment_parameters',
      'metaboMisc::detectPretreatmentParameters(spectral_processed)',
      comment = 'Detect pre-treatment routine parameters'
    ),
    pre_treated = target(
      'pre_treated',
      'metaboMisc::preTreatModes(spectral_processed,
                                          pre_treatment_parameters)',
      comment = 'Perform data pre-treatment'
    ),
    export_pre_treated_data = target(
      'export_pre_treated',
      'metaboMisc::exportData(pre_treated,type = "pre-treated",outPath = "exports/pre-treated")',
      type = 'tar_file',
      comment = 'Export pre-treated data'
    ),
    export_pre_treated_sample_info = target(
      'export_pre_treated_sample_info',
      'metaboMisc::exportSampleInfo(pre_treated,outPath = "exports/pre-treated")',
      type = 'tar_file',
      comment = 'Export sample information of pre-treated data'
    ),
    plot_PCA = target(
      'plot_PCA',
      'metabolyseR::plotPCA(pre_treated,type = "pre-treated")',
      comment = 'Plot Principle Component Analysis'
    ),
    plot_LDA = target(
      'plot_LDA',
      'metabolyseR::plotLDA(pre_treated,type = "pre-treated")',
      comment = 'Plot Priniciple Component Analysis - Linear Discriminant Analysis'
    ),
    plot_unsupervised_RF = target(
      'plot_unsupervised_RF',
      'metabolyseR::plotUnsupervisedRF(pre_treated,type = "pre-treated")',
      comment = 'Plot multidimensional scaling plot of unsupervised random forest'
    ),
    plot_supervised_RF = target(
      'plot_supervised_RF',
      'metabolyseR::plotSupervisedRF(pre_treated,type = "pre-treated")',
      comment = 'Plot multidimensional scaling plot of supervised random forest'
    )
  )
}

assignmentParameters <- function(x){
  technique <- switch(x,
                      `FIE-HRMS fingerprinting` = 'FIE',
                      `NSI-HRMS fingerprinting` = 'FIE',
                      `RP-LC-HRMS profiling` = 'RP-LC',
                      `NP-LC-HRMS profiling` = 'NP-LC')
  
  target(
    'molecular_formula_assignment_parameters',
    glue('MFassign::assignmentParameters("{technique}")'),
    comment = 'Generate molecular formula assignment parameters'
  )
}

#' @rdname targetsWorkflow
#' @export

targetsMFassignment <- function(x){
  workflow <- checkWorkflow(x)
  
  list(
    molecular_formula_assignment_parameters = assignmentParameters(workflow),
    molecular_formula_assingment = target(
      'molecular_formula_assignment',
      'pre_treated %>% 
                  metabolyseR::dat(type = "pre-treated") %>% 
                  MFassign::assignMFs(molecular_formula_assignment_parameters)', 
      args = list(memory = 'transient'),
      comment = 'Perform molecular formula assignment'
    ),
    assigned_data = target(
      'assigned_data',
      'metaboMisc::addAssignments(pre_treated,molecular_formula_assignment)',
      comment = 'Add molecular formula assignments to pre-treated data features'
    ),
    summarise_assignments = target(
      'summarise_assignments',
      'MFassign::summariseAssignment(molecular_formula_assignment)',
      comment = 'Summarise the assigned molecular formulas'
    ),
    export_assignments = target(
      'export_assignments',
      'metaboMisc::export(molecular_formula_assignment,outPath = "exports/molecular_formula_assignments")',
      type = 'tar_files',
      comment = 'Export molecular formula assignments'
    )
  )
}

modellingTargets <- function(x){
  
  object_name <- switch(x,
                        assigned ='assigned_data',
                        unassigned = 'pre_treated')
  
  list(
    modelling_parameters = target(
      'modelling_parameters',
      glue('metaboMisc::detectModellingParameters({object_name},cls = "class")'),
      comment = 'Detect appropriate modelling parameters'
    ),
    modelling = target(
      'modelling',
      glue('metabolyseR::reAnalyse({object_name},
                          modelling_parameters)'),
      comment = 'Perform modelling'
    ),
    plot_explanatory_heatmap = target(
      'plot_explanatory_heatmap',
      'metabolyseR::plotExplanatoryHeatmap(modelling)',
      comment = 'Plot a heat map of explanatory features'
    ),
    summarise_modelling_metrics = target(
      'summarise_model_metrics',
      'metabolyseR::metrics(modelling)',
      comment = 'Retrieve modelling metrics'
    ),
    summarise_modelling_importance = target(
      'summarise_model_importance',
      'metabolyseR::importance(modelling)',
      comment = 'Retireve modelling feature importance'
    ),
    export_modelling = target(
      'export_modelling',
      'metaboMisc::exportModelling(modelling,outPath = "exports/modelling")',
      type = 'tar_files',
      comment = 'Export modelling results'
    )
  )
}

#' @rdname targetsWorkflow
#' @export

targetsModelling <- function(x){
  workflow <- checkWorkflow(x)
  
  switch(workflow,
         `FIE-HRMS fingerprinting` = modellingTargets('assigned'),
         `NSI-HRMS fingerprinting` = modellingTargets('assigned'),
         `RP-LC-HRMS profiling` = modellingTargets('assigned'),
         `NP-LC-HRMS profiling` = modellingTargets('assigned'),
         `GC-MS profiling` = modellingTargets('unassigned')
  )
}

correlationsTargets <- function(x){
  
  object_name <- switch(x,
                        assigned ='assigned_data',
                        unassigned = 'pre_treated')
  
  list(
    correlations_parameters = target(
      'correlations_parameters',
      'metabolyseR::analysisParameters("correlations")',
      comment = 'Generate parameters for correlation analysis'
    ),
    correlations = target(
      'correlations',
      glue('metabolyseR::reAnalyse({object_name},
                          correlations_parameters)'),
      comment = 'Perform correlation analysis'
    ),
    summarise_correlations = target(
      'summarise_correlations',
      'metabolyseR::analysisResults(correlations,"correlations")',
      comment = 'Retrieve correlation analysis results'
    ),
    export_correlations = target(
      'export_correlations',
      'metaboMisc::exportCorrelations(correlations,outPath = "exports/correlations")',
      type = 'tar_files',
      comment = 'Export correlation analysis results'
    )
  )
}

#' @rdname targetsWorkflow
#' @export

targetsCorrelations <- function(x){
  workflow <- checkWorkflow(x) 
  
  switch(workflow,
         `FIE-HRMS fingerprinting` = correlationsTargets('assigned'),
         `NSI-HRMS fingerprinting` = correlationsTargets('assigned'),
         `RP-LC-HRMS profiling` = correlationsTargets('assigned'),
         `NP-LC-HRMS profiling` = correlationsTargets('assigned'),
         `GC-MS profiling` = correlationsTargets('unassigned')
  )
}

#' @rdname targetsWorkflow
#' @export

targetsReport <- function(x){
  list(
    report = target(
      'report',
      '"report/report.Rmd"',
      type = 'tar_render',
      args = list(output_dir = "exports")
    )
  )
}