################################################################################
###
### Utility functions used in 'user_interface.R'.
###
### Not intended to be exported.
###
################################################################################

###-----------------------------------------------------------------------------
### * Check Args

validate_extra_config <- function(.extra_config) {
    ## These are the variables that the '.extra_config' argument can alter.
    extra_config_defaults <-
        list(
            one_country_run = formals(FPEMglobal:::RunMCMC)$do.country.specific.run,
            one_country_iso = formals(FPEMglobal:::RunMCMC)$iso.select,
            global_run_output_folder_path = formals(FPEMglobal:::RunMCMC)$data_global_file_path
        )

    if (is.null(.extra_config)) {
        .extra_config = extra_config_defaults
    } else {
        checkmate::assert_list(x = .extra_config, names = "named")
        checkmate::assert_subset(
            x = names(.extra_config),
            choices = names(extra_config_defaults)
        )
        checkmate::assert_logical(.extra_config[["one_country_run"]])
        checkmate::assert_numeric(.extra_config[["one_country_iso"]])
        checkmate::assert_directory_exists(.extra_config[["global_run_output_folder_path"]])

        global_run_summary_file_path <-
            make_global_summary_file_path(.extra_config$global_run_output_folder_path, check = FALSE)
        if (!file.exists(global_run_summary_file_path))
            stop(toString("The global run summary file '", global_run_summary_file_path,
                          "' could not be found. If the global run has been done, you might need to run 'FPEMglobal:::SummariseGlobalRun()'"))
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
                          run_name_override = NULL, .extra_config = NULL) {

    ## If an override is given, just return it.
    if (!is.null(run_name_override)) return(run_name_override)

    ## Otherwise... construct run name
    run_name <- format(Sys.time(), "%y%m%d_%H%M%S")
    if (isTRUE(nchar(run_note) > 0)) run_name <- paste(run_name, run_note, sep = "_")
    run_name <- paste(run_name, age_group, marital_group, sep = "_")

    .extra_config <- validate_extra_config(.extra_config)
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
    if (!is.null(args$renamed) && args$renamed && length(args$rename_list)) {
        source_element <- "rename_list"
        out <- args$rename_list[1]
    } else {
        out <- args$run_name
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
##' @param check (Logical) Should a warning be given if the file does not exist?
##' @return The full file path.
##' @author Mark C Wheldon
##' @noRd
make_input_file_path <- function(input_folder_path = NULL, input_filename, check = TRUE) {
    if (!is.null(input_folder_path))
        full_path <- file.path(input_folder_path, input_filename)
    else
        full_path <- input_filename

    if (check)
        return(checkmate::assert_file_exists(full_path))
    else
        return(full_path)
}


##' Construct path to global summary file
##'
##' Defines the file name for, and constructs a file path pointing to, the
##' summary of a global run. This file is used as the input to a one-country
##' run. It contains the posterior means of hierarchical parameters.
##'
##' @param output_folder_path Optional path to folder containing the file. By
##'     default, only file filename will be returned.
##' @param check (Logical) Should a warning be given if the file does not exist?
##' @return File path as a character string.
##' @author Mark C Wheldon
##' @export
make_global_summary_file_path <- function(output_folder_path = NULL, check = TRUE) {
    ## This is hard-coded in runMCMC.R/RunMCMC()
    data_global_filename <- "data.global.rda"

    if (!is.null(output_folder_path))
        full_path <- file.path(output_folder_path, data_global_filename)
    else
        full_path <- data_global_filename

    if (check)
        return(checkmate::assert_file_exists(full_path))
    else
        return(full_path)
}

###-----------------------------------------------------------------------------
### * Copy Various Files

file_copy2 <- function(from, to, overwrite, ...) {
    if(!file.exists(from)) out <- NA
    else out <- file.copy(from = from, to = to, overwrite = overwrite, ...)
    out
    }

report_file_copy <- function(succeeded, filename, to_directory, from_directory,
                             new_filename = filename,
                             log_file = file.path(to_directory, "log.txt")) {
    if(is.na(succeeded) || !is.logical(succeeded)) {
        stop("\n'", filename, "' not found in '", from_directory, "'.")
    } else {
        if(succeeded) {
            message("\n'", filename, "' copied from '", from_directory,
                    "', saved as '", new_filename, "'.")
            cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"),
                ": '", filename, "' copied from '", basename(from_directory),
                "', saved as '", new_filename, "'.",
                file = log_file, sep = "", append = TRUE)
        } else {
            message("\n'", filename, "' NOT copied from '", from_directory,
                    "', to '", new_filename, "'.")
        }
    }
}

copy_data_files <- function(run_name, data_dir,
                            data_local = file.path("output", run_name, "data")) {
    if(!dir.exists(data_local)) dir.create(data_local, recursive = TRUE)
    for(nm in list.files(data_dir, pattern = "\\.csv$", recursive = TRUE)) {
        if(!is.null(data_dir)) {
            succeeded <- file_copy2(from = file.path(data_dir, nm),
                                    to = data_local,
                                   overwrite = TRUE)
        } else {
            succeeded <- file_copy2(from = nm, to = data_local,
                                   overwrite = TRUE)
        }
        report_file_copy(succeeded, nm, data_local, data_dir)
    }
    ## Subdirectories
    subdirs <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
    if (length(subdirs)) {
        for (sd in subdirs) {
            copy_data_files(run_name,
                            file.path(data_dir, sd),
                            data_local = file.path("output", run_name, "data", sd))
        }
    }
}

copy_uwra_mwra_files <-
    function(filename, awra_output_folder_path, mwra_uwra_output_folder_path,
             new_filename = filename,
             return = FALSE) {
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
                                overwrite = FALSE)
        report_file_copy(succeeded, filename, awra_output_folder_path, mwra_uwra_output_folder_path,
                         new_filename)
        if(return) return(succeeded)
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
