#' Pre-treatment of spectrally processed data
#' @description Targets for pre-treatment of spectral processed data.
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param spectral_processed S4 object of class `Binalysis` or `MetaboProfile`. If `NULL`, target input will be expected from an existing target. See details.
#' @param parameters S4 object of class `AnalysisParameters` containing pre-treatment parameters. If `NULL`, `metaboMisc::detectPretreatmentParameters()` will be used to detect the pre-treatment parameters based on the `cls` argument.
#' @param cls The name of the sample information table column containing the sample class information for parameter detection and plotting. Ignored for parameters if argument `parameters` is not `NULL`.
#' @param QCidx QC sample class label. Ignored if argument `parameters` is not `NULL`.
#' @param verbose Show pre-treatment console output.
#' @param plots A character vector of plot types. Set to `NULL` to skip all plots.
#' @param export_path Destination path of export files. Set to `NULL` to skip exports.
#' @details 
#' Specifying argument `spectral_processed` as `NULL` enables the use additional target factories outputing spectrally processed data. See the examples below.
#' @return 
#' A list of target objects for processing `mzML` data files using spectral binning.
#' @examples 
#' \dontrun{
#' ## Perform pre-treatment by specifying the spectrally processed data directly
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
#'                                             ask = FALSE)
#'         sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
#'                                            ask = FALSE)
#'         spectral_processed <- binneR::binneRlyse(file_paths,
#'                                                  sample_info,
#'                                                  binneR::detectParameters(file_paths))
#'         
#'         list(
#'             tar_pre_treatment(example,
#'                               spectral_processed = spectral_processed)
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_results_pre_treatment)
#'     targets::tar_read(example_plot_PCA)
#' })
#' 
#' ## Perform pre-treatment by combining the use of 
#' ## `tar_input_piggyback()` and `tar_spectral_binning()` 
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         name <- rlang::expr(example)
#'         
#'         list(
#'             tar_input_piggyback(!!name,
#'                                 'FIE-HRMS_BdistachyonTechnical',
#'                                 repo = 'jasenfinch/metaboData'),
#'             tar_spectral_binning(!!name),
#'             tar_pre_treatment(!!name)
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_results_pre_treatment)
#'     targets::tar_read(example_plot_PCA)
#' })
#' }
#' @export

tar_pre_treatment <- function(name,
                              spectral_processed = NULL,
                              parameters = NULL,
                              cls = 'class',
                              QCidx = 'QC',
                              verbose = TRUE,
                              plots = c('PCA',
                                        'LDA',
                                        'unsupervised_RF',
                                        'supervised_RF'),
                              export_path = "exports/pre-treated"){
    if (!is.null(parameters)) {
        if (class(parameters) != 'AnalysisParameters'){
            stop('If specified, argument `parameters` should be of S4 class `AnalysisParameters`.',
                 call. = FALSE)
        }
    }
    
    if (length(plots) > 0){
        plots <- match.arg(plots,
                           c('PCA',
                             'LDA',
                             'unsupervised_RF',
                             'supervised_RF'),
                           several.ok = TRUE)   
    }
    
    envir <- tar_option_get("envir")
    tidy_eval <- tar_option_get("tidy_eval")
    
    name <- tar_deparse_language(enexpr(name))
    
    if (is.null(spectral_processed)){
        spectral_processed_name <- sym(paste0(name,'_results_spectral_processing'))
    } else {
        spectral_processed_name <- spectral_processed
    }
    
    parameters_name <- paste0(name,'_parameters_pre_treatment')
    results_name <- paste0(name,'_results_pre_treatment')
    
    if (is.null(parameters)){
        command_parameters <- tar_tidy_eval(
            expr(metaboMisc::detectPretreatmentParameters(
                !!spectral_processed_name,
                cls = !!cls,
                QCidx = !!QCidx)),
            envir = envir,
            tidy_eval = tidy_eval
        )    
    } else {
        command_parameters <- call2(function(x) x,parameters)
    }
    
    
    command_results <- tar_tidy_eval(
        expr(metaboMisc::preTreatModes(processed_data = !!spectral_processed_name,
                                       parameters = !!sym(parameters_name),
                                       verbose = !!verbose)),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    target_parameters <- tar_target_raw(
        parameters_name,
        command_parameters
    )
    
    target_results <- tar_target_raw(
        results_name,
        command_results
    )
    
    pre_treatment_list <- list(target_parameters,
                               target_results)
    
    if (length(plots) > 0){
        pre_treatment_list <- c(pre_treatment_list,
                                pre_treatment_plots(name,
                                                    plots,
                                                    cls))
    }
    
    if (!is.null(export_path)) {
        export_name <- paste0(name,'_export_pre_treatment')
        export_info_name <- paste0(name,'_export_pre_treatment_sample_info')
        command_export <- tar_tidy_eval(
            expr(metaboMisc::exportData(!!sym(results_name),
                                        type = 'pre-treated',
                                        outPath = !!export_path)),
            envir = envir,
            tidy_eval = tidy_eval
        )
        command_export_info <- tar_tidy_eval(
            expr(metaboMisc::exportSampleInfo(!!sym(results_name),
                                              type = 'pre-treated',
                                              outPath = !!export_path)),
            envir = envir,
            tidy_eval = tidy_eval
        )
        
        export_target <- tar_target_raw(
            export_name,
            command_export,
            format = 'file'
        )
        
        export_info_target <- tar_target_raw(
            export_info_name,
            command_export_info,
            format = 'file'
        )
        
        pre_treatment_list <- c(pre_treatment_list,
                                list(export_target,
                                     export_info_target)
        )
    }
    
    return(pre_treatment_list)
}

pre_treatment_plots <- function(name,plots,cls){
    
    plot_targets <- lapply(plots,function(x,name,cls){
        plot_name <- paste0(name,'_plot_',x)
        results_name <- sym(paste0(name,'_results_pre_treatment'))
        
        plot_command <- switch(x,
                               PCA = expr(metabolyseR::plotPCA(!!results_name,
                                                               cls = !!cls,
                                                               type = "pre-treated")),
                               LDA = expr(metabolyseR::plotLDA(!!results_name,
                                                               cls = !!cls,
                                                               type = "pre-treated")),
                               unsupervised_RF = expr(metabolyseR::plotUnsupervisedRF(!!results_name,
                                                                                      type = "pre-treated",
                                                                                      cls = !!cls,
                                                                                      title = "Unsupervised random forest")),
                               supervised_RF = expr(metabolyseR::plotSupervisedRF(!!results_name,
                                                                                  cls = !!cls,
                                                                                  type = "pre-treated",
                                                                                  title = "Supervised random forest")))
        
        tar_target_raw(plot_name,
                       plot_command)
    },
    name = name,
    cls = cls)
    
    return(plot_targets)
}
