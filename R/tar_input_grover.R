#' Workflow input using a grover API
#' @description Targets for workflow inputs using a grover API
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param instrument The grover API repository instrument name.
#' @param experiment The experiment name referring to a directory available on the grover API.
#' @param grover_client S4 object of class `GroverClient` specifying the host details of the grover API.
#' @param conversion_args The file conversion arguments to be passed to the `args` argument of `grover::convertFile()`.
#' @param data_dir The path to the directory in which to output converted mzML files.
#' @return 
#' A list of target objects specifically for analysis pipeline input using the specified grover API.
#' @examples
#' ## Grover API input
#' targets::tar_dir({
#' targets::tar_script({
#'     library(hrmtargets)
#'     list(
#'         tar_input_grover(example,
#'                          'an_instrument',
#'                          'an_experiment',
#'                          grover::grover('a_host',80,'1234'))
#' )
#' })
#' targets::tar_manifest()
#' })
#' @importFrom rlang expr sym
#' @importFrom magrittr %>%
#' @importFrom tarchetypes tar_files_raw
#' @export

tar_input_grover <- function(name,
                             instrument,
                             experiment,
                             grover_client,
                             conversion_args = grover::conversionArgsPeakPick(),
                             data_dir = 'data/mzML'){
    
    if (!inherits(grover_client,'GroverClient')){
        stop('Argument `grover_client` should be of S4 class `GroverClient`.',
             call. = FALSE)
    }
    
    envir <- tar_option_get("envir")
    tidy_eval <- tar_option_get("tidy_eval")
    
    name <- tar_deparse_language(enexpr(name))
    
    raw_files_name <- paste0(name,'_raw_files')
    mzML_name <- paste0(name,'_mzML')
    raw_sample_information_name <- paste0(name,'_raw_sample_information')
    sample_information_name <- paste0(name,'_sample_information')
    
    command_raw_files <- tar_tidy_eval(
        expr(grover::listRawFiles(!!grover_client,
                                !!instrument,
                                !!experiment) %>% 
                       subset(!grepl('Ctrl',.)) %>%
                       subset(!grepl('Play',.)) %>%  
                       sort()),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    command_mzML <- tar_tidy_eval(
        expr(grover::convertFile(!!grover_client,
                                       !!instrument,
                                       !!experiment,
                                       !!sym(raw_files_name),
                                       args = !!conversion_args,
                                       outDir = !!data_dir)),
        envir = envir,
        tidy_eval = tidy_eval
        )
    
    command_raw_sample_information <- tar_tidy_eval(
        expr(grover::sampleInfo(!!grover_client,
                                !!instrument,
                                !!experiment,
                                !!sym(raw_files_name))),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    pattern_raw_files <- expr(map(!!sym(raw_files_name)))
    
    command_sample_information <- tar_tidy_eval(
        expr(metaboMisc::convertSampleInfo(!!sym(raw_sample_information_name))),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    raw_files_target <- tar_target_raw(
        raw_files_name,
        command_raw_files
    )
    
    mzML_target <- tar_target_raw(
        mzML_name,
        command_mzML,
        pattern = pattern_raw_files,
        format = 'file'
    )
    
    raw_sample_information_target <- tar_target_raw(
        raw_sample_information_name,
        command_raw_sample_information,
        pattern = pattern_raw_files
    )
    
    sample_information_target <- tar_target_raw(
        sample_information_name,
        command_sample_information
    )
    
    list(raw_files_target,
         mzML_target,
         raw_sample_information_target,
         sample_information_target)
}
