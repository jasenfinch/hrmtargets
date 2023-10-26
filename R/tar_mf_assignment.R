#' Molecular formula assignment
#' @description Targets for molecular formula assignment.
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param feature_data A tibble containing a feature intensity matrix for which to assign molecular formulas. If `NULL`, target input will be expected from an existing target. See details.
#' @param parameters S4 object of class `AssignmentParameters`.
#' @param separate Separate the molecular formula assignment steps into individual targets. This can be useful for memory intensive data sets where resources may be limited and ensures that memory can be effectively cleared between steps.
#' @param verbose Show assignment console output.
#' @param summary Boolean. Include additional summary targets.
#' @param export_path Destination path of export files. Set to `NULL` to skip exports.
#' @details 
#' Specifying argument `feature_data` as `NULL` enables the use of input from the `tar_pre_treatment()` target factory, See the second example below.
#' @return 
#' A list of target objects for molecular formula assignment.
#' @inheritParams targets::tar_target
#' @examples 
#' \dontrun{
#' ## Perform molecular formula assignment by specifying the feature data directly
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         library(assignments)
#'
#'         list(
#'             tar_mf_assignment(
#'                 example,
#'                 feature_data
#'             )
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
                              parameters = assignments::assignmentParameters('FIE'),
                              separate = FALSE,
                              verbose = TRUE,
                              summary = TRUE,
                              export_path = 'exports/molecular_formula_assignments',
                              tidy_eval = targets::tar_option_get("tidy_eval"),
                              packages = targets::tar_option_get("packages"),
                              library = targets::tar_option_get("library"),
                              format = targets::tar_option_get("format"),
                              repository = targets::tar_option_get("repository"),
                              error = targets::tar_option_get("error"),
                              memory = targets::tar_option_get("memory"),
                              garbage_collection = targets::tar_option_get("garbage_collection"),
                              deployment = targets::tar_option_get("deployment"),
                              priority = targets::tar_option_get("priority"),
                              resources = targets::tar_option_get("resources"),
                              storage = targets::tar_option_get("storage"),
                              retrieval = targets::tar_option_get("retrieval"),
                              cue = targets::tar_option_get("cue")
){
    if (!inherits(parameters,'AssignmentParameters')){
        stop('Argument `parameters` should be of S4 class `AssignmentParameters`.',
             call. = FALSE)
    }
    
    envir <- tar_option_get("envir")
    
    name <- tar_deparse_language(enexpr(name))
    
    if (is.null(feature_data)){
        feature_data_name <- sym(paste0(name,'_results_pre_treatment'))    
    } else {
        feature_data_name <- feature_data
    }
    
    parameters_name <- paste0(name,'_parameters_molecular_formula_assignment')
    assigned_data_name <- paste0(name,'_assigned_data')
    results_name <- paste0(name,'_results_molecular_formula_assignment')
    
    
    
    command_parameters <- call2(function(x) x,parameters)
    
    if (is.null(feature_data) | !inherits(feature_data,'tbl_df')){
        assigned_data_expr <- expr(
            metaboMisc::addAssignments(!!feature_data_name,
                                       !!sym(results_name))
        )    
    } else {
        assigned_data_expr <- expr(
            assignments::assignedData(!!sym(results_name))
        )
    }
    
    command_assigned_data <- tar_tidy_eval(
        assigned_data_expr,
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    target_parameters <- tar_target_raw(
        parameters_name,
        command_parameters
    )
    
    target_results <- assignment_results_targets(
        name,
        feature_data,
        separate,
        verbose,
        tidy_eval,
        envir,
        packages,
        library,
        format,
        repository,
        error,
        memory,
        garbage_collection,
        deployment,
        priority,
        resources,
        storage,
        retrieval,
        cue
    )
    
    target_assigned_data <- tar_target_raw(
        assigned_data_name,
        command_assigned_data
    )
    
    assignment_list <- list(target_parameters,
                            target_results,
                            target_assigned_data)
    
    if (isTRUE(summary)) {
        summary_name <- paste0(name,'_summary_assignments')
        command_summary <- expr(assignments::summariseAssignments(!!sym(results_name)))
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

#' @importFrom methods new

assignment_results_targets <- function(name,
                                       feature_data,
                                       separate,
                                       verbose,
                                       tidy_eval,
                                       envir,
                                       packages,
                                       library,
                                       format,
                                       repository,
                                       error,
                                       memory,
                                       garbage_collection,
                                       deployment,
                                       priority,
                                       resources,
                                       storage,
                                       retrieval,
                                       cue){
    
    results_name <- paste0(name,'_results_molecular_formula_assignment')
    parameters_name <- paste0(name,'_parameters_molecular_formula_assignment')
    feature_data_name <- sym(paste0(name,'_results_pre_treatment'))
    
    if (isFALSE(separate)){
        
        if (is.null(feature_data)){
            results_expr <- expr(assignments::assignMFs(feature_data = !!feature_data_name,
                                                        parameters = !!sym(parameters_name),
                                                        verbose = !!verbose,
                                                        type = 'pre-treated'))
        } else {
            feature_data_name <- feature_data
            results_expr <- expr(assignments::assignMFs(feature_data = !!feature_data_name,
                                                        parameters = !!sym(parameters_name),
                                                        verbose = !!verbose))
        }
        
        command_results <- tar_tidy_eval(
            results_expr,
            envir = envir,
            tidy_eval = tidy_eval
        )
        
        target_results <- list(
            tar_target_raw(
                results_name,
                command_results,
                packages = packages,
                library = library,
                format = format,
                repository = repository,
                error = error,
                memory = memory,
                garbage_collection = garbage_collection,
                deployment = deployment,
                priority = priority,
                resources = resources,
                storage = storage,
                retrieval = retrieval,
                cue = cue
            )   
        )
    }
    
    if (isTRUE(separate)){
        object_name <- paste0(name, "_assignments_object")
        correlations_name <- paste0(name, "_assignments_correlations")
        relationships_name <- paste0(name, "_assignments_relationships")
        ai_name <- paste0(name, "_assignments_adduct_isotopes")
        
        if (is.null(feature_data)){
            object_expr <- expr(new('Assignment',
                                    !!rlang::sym(parameters_name),
                                    data = !!feature_data_name %>% 
                                        metabolyseR::dat(type = 'pre-treated')))
        } else {
            object_expr <- expr(new('Assignment',
                                    !!rlang::sym(parameters_name),
                                    data = !!feature_data)
            )
        }
        
        command_object <- tar_tidy_eval(
            object_expr,
            envir = envir, 
            tidy_eval = tidy_eval)
        command_correlations <- tar_tidy_eval(
            expr(!!sym(object_name) %>% 
                     assignments::calcCorrelations()),
            envir = envir, 
            tidy_eval = tidy_eval)
        command_relationships <- tar_tidy_eval(
            expr(!!sym(correlations_name) %>% 
                     assignments::calcRelationships()),
            envir = envir, 
            tidy_eval = tidy_eval
        )
        command_ai <- tar_tidy_eval(
            expr(!!sym(relationships_name) %>% 
                     assignments::addIsoAssign()),
            envir = envir, 
            tidy_eval = tidy_eval
        )
        command_results <- tar_tidy_eval(
            expr(!!sym(ai_name) %>% 
                     assignments::transformationAssign()),
            envir = envir, 
            tidy_eval = tidy_eval
        )
        
        target_results <- list(
            tar_target_raw(object_name,
                           command_object),
            tar_target_raw(correlations_name,
                           command_correlations),
            tar_target_raw(relationships_name,
                           command_relationships),
            tar_target_raw(ai_name,
                           command_ai),
            tar_target_raw(results_name,
                           command_results)
        )
    }
    
    return(target_results)
}