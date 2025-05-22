################################################################################
###
### Utility functions used in 'user_interface.R'.
###
### Not intended to be exported.
###
################################################################################

###-----------------------------------------------------------------------------
### * Check Args

##' Validate 'extra arguments' supplied via '...'
##'
##' The main user interface functions (e.g., \code{\link{do_global_mcmc}}) have
##' a \code{...} argument. This is so that extra arguments for doing one-country
##' runs can be passed in and the \pkg{FPEMglobal} functions re-used, rather
##' than duplicated them in the \pkg{FPEMcountry} package.
##' @param .extra_config The \code{...} as a list. Coercion to a list needs to
##'     be done by the caller.
##' @return The validated \code{...} as a list.
##' @author Mark C Wheldon
validate_extra_config <- function(.extra_config) {

    ## -------* General Checks

    ## These are the variables that the '.extra_config' argument can alter.
    extra_config_defaults <-
        list(
            one_country_run = formals(FPEMglobal:::RunMCMC)$do.country.specific.run,
            one_country_iso = formals(FPEMglobal:::RunMCMC)$iso.select,
            global_run_output_folder_path = formals(FPEMglobal:::RunMCMC)$data_global_file_path,
            use_global_run_aux_data_files = FALSE
        )

    ## Check that the elements of '.extra_config' are allowed by comparing names
    ## with those in default list above.
    if (!length(.extra_config)) {
        .extra_config = extra_config_defaults
    } else {
        if (!isTRUE(check_res_list <- checkmate::check_list(x = .extra_config, names = "named"))) {
            stop("Invalid use of '...' argument.",  "\n", check_res_list)
        }
        if (!isTRUE(check_res_list_names <-
                        checkmate::check_subset(
                                       x = names(.extra_config),
                                       choices = names(extra_config_defaults)))) {
            msg <- paste0("The following are not valid argument names: ",
                          toString(names(.extra_config)[!names(.extra_config) %in% names(extra_config_defaults)]),
                          "\n---\n",
                          "Invalid use of '...' argument.",  "\n",
                          toString(check_res_list_names))
            stop(msg)
        }
        checkmate::assert_logical(.extra_config[["one_country_run"]])
        checkmate::assert_numeric(as.numeric(.extra_config[["one_country_iso"]]), null.ok = TRUE)
        checkmate::assert_character(.extra_config[["global_run_output_folder_path"]], null.ok = TRUE)
        checkmate::assert_logical(.extra_config[["use_global_run_aux_data_files"]])
    }

    ## -------* Checks for One-Country Runs

    if (.extra_config[["one_country_run"]]) {
        msg <- "This is a one-country run"

        ## -------** Check types of elements

        checkmate::assert_directory_exists(.extra_config[["global_run_output_folder_path"]])

        ## -------** Check Files and Directories

        ## ISO is set
        if (is.null(.extra_config[["one_country_iso"]]))
            stop(msg, " but 'one_country_iso' is 'NULL'.")

        ## 'global_run_output_folder_path' is set.
        if (is.null(.extra_config[["global_run_output_folder_path"]]))
            stop(msg, " but 'global_run_output_folder_path' is 'NULL'.")

        ## Check that the global summary file exists
        global_run_summary_file_path <-
            make_global_summary_file_external_path(folder_path = .extra_config[["global_run_output_folder_path"]],
                                                   check = FALSE)
        if (!file.exists(global_run_summary_file_path))
            stop(msg, " but the global run summary file '", global_run_summary_file_path,
                 "' could not be found. If the global run has been done, you might need to run 'FPEMglobal:::SummariseGlobalRun()'")

        ## Check that the global input data directory exists and is not empty
        if (.extra_config[["use_global_run_aux_data_files"]]) {
            global_data_dir <- file.path(.extra_config[["global_run_output_folder_path"]], "data")
            if (!dir.exists(global_data_dir))
                stop(msg, " and 'global_run_output_folder_path' is 'TRUE' but the global data directory '",
                     global_data_dir,
                     "' does not exist.")
            if (!any(grepl("\\.csv", list.files(global_data_dir))))
                warning(msg, " and 'global_run_output_folder_path' is 'TRUE' but the global data directory '",
                        global_data_dir,
                        "' does not contain any '.csv' files.")
            .extra_config[["global_run_data_files_folder_path"]] <- global_data_dir
        }

        ## -------** SET global run name

        global_args_file_path <-
            file.path(.extra_config[["global_run_output_folder_path"]], "global_mcmc_args.RData")
        if (!file.exists(global_args_file_path))
            stop(msg, " but the global args file '", global_args_file_path, "' cannot be found. This is required to ascertain the run name of the global run.")
        .extra_config[["global_run_name"]] <- get_run_name_from_args(get(load(global_args_file_path)))
    }

    return(.extra_config)
}


###-----------------------------------------------------------------------------
### * Make Run Name

## Make country code always 3 digits by prefixing zeros.
expand_country_code <- function(x) {
    x <- as.character(as.numeric(x))
    paste0(paste(rep("0", 3 - nchar(x)), collapse = ""), x)
}

make_run_name <- function(marital_group, age_group, run_note = NULL,
                          run_name_override = NULL, ...) {

    ## If an override is given, just return it.
    if (!is.null(run_name_override)) return(run_name_override)

    ## Otherwise... construct run name
    run_name <- format(Sys.time(), "%y%m%d_%H%M%S")
    if (isTRUE(nchar(run_note) > 0)) run_name <- paste(run_name, run_note, sep = "_")
    run_name <- paste(run_name, age_group, marital_group, sep = "_")

    .extra_config <- validate_extra_config(list(...))
    if (.extra_config$one_country_run) {
        run_name <- paste0(run_name, "_1c-",
                          expand_country_code(.extra_config$one_country_iso))
    }
    return(run_name)
}

convert_run_name <- function(run_name, from = c("married", "unmarried", "all_women"),
             to = c("married", "unmarried", "all_women")) {
        from <- match.arg(from)
        to  <-  match.arg(to)
        gsub(pattern = from, replacement = to, x = run_name)
    }

##' Get run_name from an argument list
##'
##' Get run name from 'global_mcmc_args.RData',
##' 'post_process_args.RData', or 'combine_runs_args.RData'.
##'
##' @param args_list
##' @return Run name as a character string.
##' @author Mark Wheldon
##' @export
get_run_name_from_args <- function(args) {
    if (!is.null(args[["renamed"]]) && args[["renamed"]] && length(args[["rename_list"]])) {
        source_element <- "rename_list"
        out <- args[["rename_list"]][1]
    } else {
        out <- args[["run_name"]]
        source_element <- "run_name"
    }
    if (is.null(out))
        stop("run name cannot be determined from '",
             source_element, "' in '",
             deparse(substitute(args)), "'; result is 'NULL'.")
    else return(out)
}

## Make sure a run_name conflict does not occur when writing to existing outputs
check_run_name_conflicts <- function(run_name, output_folder_path) {
    combine_runs_filepath <-
        file.path(output_folder_path, "combine_runs_args.RData")
    if (dir.exists(output_folder_path)) {
        if (file.exists(combine_runs_filepath)) {
            existing_run_name <- get_run_name_from_args(get(load(combine_runs_filepath)))
            if (!identical(run_name, existing_run_name))
                stop("'run_name' not the same as existing run name ('",
                     existing_run_name, "').")
        } else {
            stop("'", output_folder_path, "' already exists but '",
                 combine_runs_filepath, "' does not; cannot determine 'run_name' of existing output.")
        }
    }
    return(invisible())
}

###-----------------------------------------------------------------------------
### * Make File Paths

##' Construct path to an input file
##'
##' Standardizes the way \code{input_[...]_folder_path} and
##' \code{[...]_filename} are combined to form the full path to an input file.
##' This is mainly for internal use by this and other packages.
##'
##' If \code{input_folder_path} is not \code{NULL}, it is used to form the file
##' path by prefixing it to \code{input_filename}. Otherwise, just
##' \code{input_filename} is returned.
##'
##' @param input_folder_path Path to the folder containing \code{input_filename}.
##' @param input_filename The name of the input file (incl. extension).
##' @param check (Logical) Should an error be given if the file does not exist?
##' @return The full file path.
##' @author Mark C Wheldon
##' @noRd
make_input_file_path <- function(input_folder_path = NULL, input_filename = "", check = TRUE) {
    if (!is.null(input_folder_path))
        full_path <- file.path(input_folder_path, input_filename)
    else
        full_path <- input_filename

    if (check)
        return(checkmate::assert_file_exists(full_path))
    else
        return(full_path)
}

## Thin wrapper for semantic consistency with 'make_input_aux_data_file_path()'.
make_input_data_file_path <- function(...) {
    make_input_file_path(...)
}

## Variant of 'make_input_file_path()' that allows for different input
## directories for the auxiliary files, like the region info, aggregates, etc.
make_input_aux_data_file_path <- function(input_folder_path = NULL, input_filename = "", check = TRUE,
                                          ...) {

    .extra_config <- validate_extra_config(list(...))

    if (.extra_config[["use_global_run_aux_data_files"]])
        input_folder_path <- .extra_config[["global_run_data_files_folder_path"]]

    make_input_file_path(input_folder_path = input_folder_path,
                         input_filename = input_filename, check = check)
}


##' Construct path to global summary file
##'
##' Defines the file name for, and constructs a file path pointing to, the
##' summary of a global run. This is an \filename{.rda} file which contains the
##' posterior means of hierarchical parameters. It is used as the input to a
##' one-country run. The global run output folder is passed in via the argument
##' `global_run_output_folder_path` (see \dQuote{Details}) for more information.
##'
##' \code{make_global_summary_file_external_path()} Creates the path to the
##' global summary file stored in the output folder of a global run.
##' \emph{Either} supply the explicit folder containing the summary file via the
##' \code{folder_path} argument, or supply the path to the main global run
##' output folder via the argument \code{global_run_output_folder_path} (which
##' can be part of the \code{...}).
##'
##' @param folder_path Optional path to folder containing the file. By
##'     default, only file filename will be returned.
##' @param check (Logical) Should an error be thrown if the file does not exist?
##' @return File path as a character string.
##' @author Mark C Wheldon
##'
##' @examples
##' make_global_summary_file_external_path("test", check = FALSE)
##'
##' @noRd
make_global_summary_file_external_path <-
    function(folder_path = NULL, check = !is.null(folder_path), ...) {

        ## This is hard-coded in runMCMC.R/RunMCMC()
        data_global_filename <- "data.global.rda"

    ## This function is called by 'validate_extra_config()', so there are two
    ## modes of operation:
    ##
    ## 1) The folder path is passed in explicitly. This *must* be the way
    ## 'validate_extra_config()' calls this function.
    ##
    ## 2) The folder path is read from '...'.

    if (!is.null(folder_path)) {
        ## Explicit folder path:
        full_path <- file.path(folder_path, data_global_filename)
    } else {
        ## Taken from '...'
        .extra_config <- validate_extra_config(list(...))
        full_path <-
            file.path(.extra_config[["global_run_output_folder_path"]],
                      data_global_filename)
    }

    if (check)
        return(checkmate::assert_file_exists(full_path))
    else
        return(full_path)
}

###-----------------------------------------------------------------------------
### * Copy Various Files

###-----------------------------------------------------------------------------
### ** Copy Utilities

## Wrapper to 'file.copy()'. Returns 'NA' if source file does not exist.
file_copy2 <- function(from, to, overwrite, ...) {
    if (!file.exists(from)) out <- NA
    else out <- file.copy(from = from, to = to, overwrite = overwrite, ...)
    out
}


## Check and report result of attempting to copy files.
report_file_copy <- function(succeeded, filename, from_dir, to_dir,
                             new_filename = filename,
                             log_file = file.path(to_dir, "log.txt"),
                             verbose = getOption("FPEMglobal.verbose")) {
    if (is.na(succeeded) || !is.logical(succeeded)) {
        stop("\n'", filename, "' not found in '", from_dir, "'.")
    } else {
        if (succeeded) {
            if (verbose) message("\n'", filename, "' copied from '", from_dir,
                    "', saved as '", new_filename, "'.")
            cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"),
                ": '", filename, "' copied from '", basename(from_dir),
                "', saved as '", new_filename, "'.",
                file = log_file, sep = "", append = TRUE)
        } else {
            if (verbose) message("\n'", filename, "' NOT copied from '", from_dir,
                    "', to '", new_filename, "'.")
        }
    }
}


###-----------------------------------------------------------------------------
### ** Copy Sets of Files

## Copy all .csv data files from a 'from' directory to a run directory. This
## will be commonly used to copy all .csv files in "./input" to
## "./output/<run_name>/data".
copy_csv_data_files <- function(run_name, from_dir = "input",
                            to_dir = file.path("output", run_name, "data"),
                            overwrite = FALSE,
                            verbose = getOption("FPEMglobal.verbose"),
                            ...) {

    ## -------* Check args

    .extra_config <- validate_extra_config(list(...))

    ## -------* Handle .extra_config

    if (isTRUE(.extra_config$use_global_run_aux_data_files)) {
        global_run_file_names <-
            list.files(make_input_aux_data_file_path(NULL, "", check = FALSE, ...),
                       pattern = "\\.csv")
        local_input_file_names <-
            list.files(from_dir, pattern = "\\.csv")

        local_duplicates <-
            local_input_file_names[local_input_file_names %in% global_run_file_names]
        if (length(local_duplicates))
            stop("'use_global_run_aux_data_files' is 'TRUE' but the local input directory ('",
                 from_dir,
                 "') contains the following files which were also found in the global run data directory ('",
                 .extra_config$global_run_data_files_folder_path,
                 "'): '\n",
                 toString(local_duplicates, width = 5 * 80),
                 "'.\n",
                 "\t- If you want to use the global run data files, but the local input directory contains files with the same names, create a different local input directory, e.g., './input_one_country', and copy the one-country prevalence data there.",
                 "\t- If you want to use some of the files in the local data directory and some in the global run data directory, copy all input files to the local input directory and set 'use_global_run_aux_data_files' to 'FALSE'.")
    }

    ## -------* Do the copying

    if (!dir.exists(to_dir)) dir.create(to_dir, recursive = TRUE)

    for (nm in list.files(make_input_aux_data_file_path(
                   input_folder_path = from_dir,
                   input_filename = "",
                   check = FALSE, ...),
                   pattern = "\\.csv$", recursive = FALSE)) {
        from_dir <- make_input_aux_data_file_path(input_folder_path = from_dir,
                                                  input_filename = nm,
                                                  check = FALSE, ...)
        succeeded <-
            file_copy2(from = from_dir, to = to_dir, overwrite = overwrite)
        report_file_copy(succeeded, nm, from_dir, to_dir, verbose = verbose)
    }

    ## Recurse into subdirectories
    subdirs <- list.dirs(from_dir, full.names = FALSE, recursive = FALSE)
    if (length(subdirs)) {
        for (sd in subdirs) {
            copy_csv_data_files(run_name,
                            from_dir = file.path(from_dir, sd),
                            to_dir = file.path(to_dir, sd),
                            overwrite = overwrite, ...)
        }
    }
    return(invisible(succeeded))
}


## Copy married and unmarried files to an all women run directory.
copy_uwra_mwra_files <-
    function(filename, awra_output_folder_path, mwra_uwra_output_folder_path,
             new_filename = filename,
             overwrite = FALSE,
             verbose = getOption("FPEMglobal.verbose")) {
        if (!dir.exists(mwra_uwra_output_folder_path)) stop("'", mwra_uwra_output_folder_path, "' does not exist.")
        if (!dir.exists(awra_output_folder_path)) stop("'", awra_output_folder_path, "' does not exist.")
        if (!identical(dirname(new_filename), ".")) {
            to_path <- file.path(awra_output_folder_path, dirname(new_filename))
            if (!dir.exists(to_path)) dir.create(to_path, recursive = TRUE)
            to_filename <- basename(new_filename)
        } else {
            to_path <- awra_output_folder_path
            to_filename <- new_filename
        }
        succeeded <- file_copy2(from = file.path(mwra_uwra_output_folder_path, filename),
                                to = file.path(to_path, to_filename),
                                overwrite = overwrite)
        report_file_copy(succeeded, filename, mwra_uwra_output_folder_path, awra_output_folder_path,
                         new_filename, verbose = verbose)

        return(invisible(succeeded))
    }


## Copy the 'data.global.rda' file from for a one-country run.
## Note: If this isn't a one-country run, nothing happens.
copy_global_run_summary_file <-
    function(to_dir, overwrite = FALSE, verbose = getOption("FPEMglobal.verbose"), ...) {

    .extra_config <- validate_extra_config(list(...))

    ## If not a one-country run, do nothing.
    if (!.extra_config$one_country_run) return(invisible(TRUE))

    else {
        global_summary_file_external_path <-
            make_global_summary_file_external_path(...)

        succeeded <- file_copy2(from = global_summary_file_external_path,
                                to = to_dir,
                                overwrite = overwrite)
        report_file_copy(succeeded,
                         filename = basename(global_summary_file_external_path),
                         from_dir = dirname(global_summary_file_external_path),
                         to_dir = to_dir,
                         new_filename = basename(global_summary_file_external_path),
                         verbose = verbose)
        return(invisible(succeeded))
    }
}


###-----------------------------------------------------------------------------
### * Determine Parameter Values

##' Define Default Values for MCMC Paramters According to Marital Group
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param marital_group
##' @param model_name
##' @return
##' @author Mark Wheldon
##' @noRd
marital_age_group_param_defaults <- function(marital_group, age_group, model_family, model_name) {
    if(marital_group == "married") {
        if(is.null(model_name)) {
            if(age_group %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")) {
                if(model_family == "rate") {
                    write_model_fun <- "WriteModel_MWRA_Geog_Rate_1519_InclNoData"
                } else {        #level model
                    write_model_fun <- "WriteModel_MWRA_geog_1519_InclNoData"
                }
            } else {            #other age groups
                if(model_family == "rate") {
                    write_model_fun <- "WriteModel_MWRA_Geog_Rate"
                } else {        #level model
                    write_model_fun  <-  "WriteModel_MWRA_Geog"
                }
            }
        } else {
            write_model_fun <- model_name
        }
        uwra_z_priors <- NULL
        uwra_Omega_priors <- NULL
        uwra_kappa_c_priors <- NULL
        timing_world_priors <- list(mean.TOneLevel = 1920, mean.Tworld = 1980)
        EA_bias_negative <- FALSE
        HW_bias_negative <- FALSE
    } else if(marital_group == "unmarried") {
        if(is.null(model_name))  {
            if(model_family == "rate") {
                write_model_fun <- "WriteModel_InclNoData_SA1SubIndia_Rate"
            } else {            #level model
                write_model_fun <- "WriteModel_InclNoData_SA1SubIndia"
            }
        } else {
            write_model_fun <- model_name
        }
        timing_world_priors <- list(mean.TOneLevel = 2070, mean.Tworld = 1970)
        uwra_z_priors <- 1
        uwra_Omega_priors <- 1
        uwra_kappa_c_priors <- NULL
        EA_bias_negative <- TRUE
        HW_bias_negative <- TRUE
    } else {
        stop("'marital_group' must be in 'c(\"married\", \"unmarried\")'.")
    }
    return(list(write_model_fun = write_model_fun,
                timing_world_priors = timing_world_priors,
                uwra_z_priors = uwra_z_priors,
                uwra_Omega_priors = uwra_Omega_priors,
                uwra_kappa_c_priors = uwra_kappa_c_priors,
                EA_bias_negative = EA_bias_negative,
                HW_bias_negative = HW_bias_negative))
}

###-----------------------------------------------------------------------------
### * 'extdata' Files

make_pkg_dir_entry <- function(filename, result = c("filename", "filepath")) {
    if (identical(length(result), 1L)) {
    return(list(filename = filename,
                filepath = system.file("extdata", filename, package = "FPEMglobal"))[[result]])
        } else {
    return(list(filename = filename,
                filepath = system.file("extdata", filename, package = "FPEMglobal"))[result])
        }
}

get_all_spec_agg_csv <- function(pattern = "^aggregates_special_.+\\.csv$") {
    grep(pattern, dir(system.file("extdata", package = "FPEMglobal")), value = TRUE)
}

get_all_spec_agg_names <- function(pattern = "^aggregates_special_.+\\.csv$") {
    gsub(pattern = "\\.csv", replacement = "", x = get_all_spec_agg_csv(pattern = pattern))
}
