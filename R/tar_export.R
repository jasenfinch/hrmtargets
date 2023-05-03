#' Dynamic branching over exported files
#' @description
#' Dynamic branching over exported files. This has similar functionality to
#' `tarchetypes::tar_files()` except that the upstream target does not automatically
#' become invalidated each time the pipeline is run.  
#' @inheritParams tarchetypes::tar_files
#' @return A list of target objects for exporting data files.
#' @examples
#' ## File export
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'     
#'         list(
#'             tar_export(
#'                 example_export,
#'                 jfmisc::exportCSV(iris,'iris.csv')
#'             )
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_export)
#' })
#' @rdname tar_export
#' @export

tar_export_raw <- function(
        name,
        command,
        tidy_eval = targets::tar_option_get("tidy_eval"),
        packages = targets::tar_option_get("packages"),
        library = targets::tar_option_get("library"),
        format = c("file", "url", "aws_file"),
        repository = targets::tar_option_get("repository"),
        iteration = targets::tar_option_get("iteration"),
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
    targets::tar_assert_chr(name, "name must be a character.")
    targets::tar_assert_scalar(name, "name must have length 1.")
    
    targets::tar_assert_nonmissing(
        command,
        paste("target", name, "has no command.")
    )
    targets::tar_assert_scalar(
        as.expression(command),
        paste("the command of target", name, "must have length 1.")
    )
    
    command <- as.expression(command)[[1]]
    name_files <- paste0(name, "_files")
    format <- match.arg(format)
    upstream <- targets::tar_target_raw(
        name = name_files,
        command = command,
        pattern = NULL,
        packages = packages,
        library = library,
        format = "rds",
        repository = repository,
        iteration = iteration,
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
    name_files_sym <- as.symbol(name_files)
    downstream <- targets::tar_target_raw(
        name = name,
        command = as.expression(name_files_sym),
        pattern = as.expression(as.call(c(as.symbol("map"), list(name_files_sym)))),
        packages = character(0),
        library = library,
        format = format,
        repository = repository,
        iteration = iteration,
        error = error,
        memory = memory,
        garbage_collection = garbage_collection,
        deployment = "main",
        priority = priority,
        resources = resources,
        storage = "main",
        retrieval = "main",
        cue = cue
    )
    out <- list(upstream, downstream)
    names(out) <- c(name_files, name)
    out
}

#' @rdname tar_export
#' @export

tar_export <- function(
        name,
        command,
        tidy_eval = targets::tar_option_get("tidy_eval"),
        packages = targets::tar_option_get("packages"),
        library = targets::tar_option_get("library"),
        format = c("file", "url", "aws_file"),
        repository = targets::tar_option_get("repository"),
        iteration = targets::tar_option_get("iteration"),
        error = targets::tar_option_get("error"),
        memory = targets::tar_option_get("memory"),
        garbage_collection = targets::tar_option_get("garbage_collection"),
        deployment = targets::tar_option_get("deployment"),
        priority = targets::tar_option_get("priority"),
        resources = targets::tar_option_get("resources"),
        storage = targets::tar_option_get("storage"),
        retrieval = targets::tar_option_get("retrieval"),
        cue = targets::tar_option_get("cue")
) {
    name <- targets::tar_deparse_language(substitute(name))
    envir <- tar_option_get("envir")
    command <- targets::tar_tidy_eval(substitute(command), envir, tidy_eval)
    format <- match.arg(format)
    tar_export_raw(
        name = name,
        command = command,
        packages = packages,
        library = library,
        format = format,
        repository = repository,
        iteration = iteration,
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
}
