#' Molecular formula assignment
#' @description Targets for molecular formula assignment.
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param feature_data A tibble containing a feature intensity matrix for which to assign molecular formulas. If `NULL`, target input will be expected from an existing target. See details.
#' @param parameters S4 object of class `AssignmentParameters`.
#' @param verbose Show assignment console output.
#' @param summary Boolean. Include additional summary targets.
#' @param export_path Destination path of export files. Set to `NULL` to skip exports.
#' @details 
#' Specifying argument `feature_data` as `NULL` enables the use of input from the `tar_pre_treatment()` target factory, See the second example below.
#' @return 
#' A list of target objects for molecular formula assignment.
#' @examples 
#' \dontrun{
#' ## Perform molecular formula assignment by specifying the feature data directly
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         
#'         list(
#'             tar_mf_assignment(MFassign::feature_data)
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_summary_assignments)
#' })
#' 
#' ## Perform molecular formula assignment by using `tar_input_piggyback()`,
#' ## `tar_input_spectral_binning()` and `tar_pre_treatment()` 
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
#'             tar_pre_treatment(!!name),
#'             tar_mf_assignment(!!name)
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_results_spectral_processing)
#'     targets::tar_read(example_summary_assignments)
#' })
#' }
#' @export

tar_mf_assignment <- function(name,
                              feature_data = NULL,
                              parameters = MFassign::assignmentParameters('FIE'),
                              verbose = TRUE,
                              summary = TRUE,
                              export_path = 'exports/molecular_formula_assignments'){
    if (class(parameters) != 'AssignmentParameters'){
        stop('Argument `parameters` should be of S4 class `AssignmentParameters`.',
             call. = FALSE)
    }
    
    envir <- tar_option_get("envir")
    tidy_eval <- tar_option_get("tidy_eval")
    
    name <- tar_deparse_language(enexpr(name))
    
    parameters_name <- paste0(name,'_parameters_molecular_formula_assignment')
    results_name <- paste0(name,'_results_molecular_formula_assignment')
    assigned_data_name <- paste0(name,'_assigned_data')
    
    command_parameters <- call2(function(x) x,parameters)
    
    if (is.null(feature_data)){
        feature_data_name <- sym(paste0(name,'_results_pre_treatment'))
        results_expr <- expr(MFassign::assignMFs(feature_data = !!feature_data_name,
                                                 parameters = !!sym(parameters_name),
                                                 verbose = !!verbose,
                                                 type = 'pre-treated'))
    } else {
        feature_data_name <- feature_data
        results_expr <- expr(MFassign::assignMFs(feature_data = !!feature_data_name,
                                                 parameters = !!sym(parameters_name),
                                                 verbose = !!verbose))
    }
    
    command_results <- tar_tidy_eval(
        results_expr,
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    command_assigned_data <- tar_tidy_eval(
        expr(metaboMisc::addAssignments(!!feature_data_name,
                                        !!sym(results_name))),
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
    
    target_assigned_data <- tar_target_raw(
        assigned_data_name,
        command_assigned_data
    )
    
    assignment_list <- list(target_parameters,
                            target_results)
    
    if (isTRUE(summary)) {
        summary_name <- paste0(name,'_summary_assignments')
        command_summary <- expr(MFassign::summariseAssignment(!!sym(results_name)))
        summary_targets <- tar_target_raw(
            summary_name,
            command_summary
        )
        
        assignment_list <- c(assignment_list,
                             list(summary_targets)
        )
    }
    
    if (!is.null(export_path)) {
        export_name <- paste0(name,'_export_assignments')

        command_export <- tar_tidy_eval(
            expr(metaboMisc::export(!!sym(results_name),
                                        outPath = !!export_path)),
            envir = envir,
            tidy_eval = tidy_eval
        )
        
        export_target <- tar_target_raw(
            export_name,
            command_export,
            format = 'file'
        )
        
        
        assignment_list <- c(assignment_list,
                                list(export_target)
        )
    }
    
    return(assignment_list)
}
