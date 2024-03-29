################################################################################
###
### Utility functions used in 'user_interface.R'.
###
### Not intended to be exported.
###
################################################################################

###-----------------------------------------------------------------------------
### * Make Run Name

make_run_name <- function(marital_group, age_group, run_note = NULL,
                          run_name_override = NULL) {
    if(is.null(run_name_override)) {
        run_name <- format(Sys.time(), "%y%m%d_%H%M%S")
    } else {
        run_name <- run_name_override
    }
    if(isTRUE(nchar(run_note) > 0)) run_name <- paste(run_name, run_note, sep = "_")
    run_name <- paste(run_name, age_group, marital_group, sep = "_")
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
