################################################################################
###
### Separate mcmc and summary functions.
###
################################################################################

##' Validate input file for a global run of FPEM
##'
##' The main input file for \code{\link{do_global_run}} and friends
##' must meet certain requirements to be valid. These include correct
##' column names and valid cell values. This function can be used to
##' check that a candidate \file{.csv} files satisfies these
##' requirements.
##'
##' See \dQuote{Details} in the help file for \code{\link{do_global_all_women_run}}.
##'
##' @seealso \code{link{validate_denominator_counts_file}}, \code{\link{do_global_all_women_run}}
##'
##' @inheritParams do_global_mcmc
##' @return If all checks pass, the processed input file is returned \emph{invisibly} as a data frame.
##' @author Mark Wheldon
##' @export
validate_input_file <- function(age_group = "15-49",
                                input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
                                data_csv_filename = paste0("data_cp_model_all_women_", age_group, ".csv"),
                                marital_group = c("married", "unmarried"),
                                verbose = FALSE) {

    if(!is.null(input_data_folder_path)) {
        data_csv_filename <- file.path(input_data_folder_path, data_csv_filename)
    }
    if(!file.exists(data_csv_filename)) stop("'data_csv_filename' does not exist.")

    model_family <- "rate"
    model_name <- NULL

    marital_group <- match.arg(marital_group, several.ok = TRUE)

    out_df <- data.frame()
    for (mg in marital_group) {
        if (verbose) message("\n\n--------------------\n", rep(" ", 20 - nchar(mg)), mg, "\n")

        fname <- data_csv_filename
        temp_df <- read.csv(fname)
        inunion <- switch(mg, "married" = "1", "unmarried" = "0")
        temp_df <- temp_df[as.character(temp_df$In.union) == inunion, ]
        lr_cols <- c("SE.logR.trad.nouse", "SE.logR.modern.nouse", "SE.logR.unmet.noneed")
        lr_cols_no_in <- lr_cols[!lr_cols %in% colnames(temp_df)]
        if (length(lr_cols_no_in))
            stop("Col(s) '", toString(lr_cols_no_in), "' not found in input file for marital group '", mg, "'." )
        for (lr in lr_cols) {
            if (all(is.na(temp_df[, lr])))
                stop("'", lr, "' are all 'NA' for marital group '", mg, "'." )
            else if (all(!is.finite(temp_df[, lr])))
                stop("'", lr, "' are all non-finite for marital group '", mg, "'.")
        }

        ## Check CP estimates
        marital_group_param_set <-
            marital_age_group_param_defaults(marital_group = mg, age_group, model_family,
                                             model_name)
        out_df <- rbind(out_df,
                        PreprocessData(data.csv = data_csv_filename,
                                       write.model.fun = marital_group_param_set$write_model_fun,
                                       print.messages = verbose,
                                       print.warnings = verbose,
                                       return.processed.data.frame = TRUE,
                                       marital.group = switch(mg, "married" = "MWRA", "unmarried" = "UWRA")),
                        check.names = FALSE)
    }
    message("'", data_csv_filename, "' is valid.")
    return(invisible(out_df))
}


##' Validate denominator counts file for a global run of FPEM
##'
##' The denominator counts file for \code{\link{do_global_run}} and friends
##' must meet certain requirements to be valid. These include correct
##' column names and valid cell values. This function can be used to
##' check that a candidate \file{.csv} files satisfies these
##' requirements.
##'
##' See \dQuote{Details} in the help file for \code{\link{do_global_all_women_run}}.
##'
##' @seealso \code{link{validate_input_file}}, \code{\link{do_global_all_women_run}}
##'
##' @inheritParams do_global_mcmc
##' @return The processed denominator counts file is returned \emph{invisibly} as a data frame.
##' @author Mark Wheldon
##' @export
validate_denominator_counts_file <- function(age_group = "15-49",
                                             input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
                                             denominator_counts_csv_filename = paste0("number_of_women_", age_group, ".csv"),
                                             marital_group = c("married", "unmarried"),
                                             countries_for_aggregates_csv_filename = "countries_mwra_195_pre2024.csv",
                                             output_folder_path = NULL,
                                             verbose = FALSE) {
    model_family <- "rate"
    model_name <- NULL

    marital_group <- match.arg(marital_group, several.ok = TRUE)

    ## Read in the denominator counts

    if (identical(denominator_counts_csv_filename, "res.country.rda")) {
        message("Reading denominator counts from '", file.path(output_folder_path, "res.country.rda"), "'.")
        stopifnot (!is.null(output_folder_path) || !length(dir(output_folder_path)) ||
                   !identical(length(marital_group), 1L))
        res_country_list <- get(load(file.path(output_folder_path, "res.country.rda")))
        out_df <- data.frame(ISO.code = res_country_list$iso.g,
                             Country = names(res_country_list$W.Lg.t),
                             do.call("rbind", res_country_list$W.Lg.t) * 1e3, #NB !! Multiply by 1000
                             row.names = NULL)
        colnames(out_df)[-(1:2)] <-
            as.numeric(dimnames(res_country_list$CIprop.Lg.Lcat.qt[[1]][["Total"]])[[2]])- 0.5

        if (identical(marital_group, "married")) mg_in_union <- 1
        else mg_in_union <- 0
        out_df <- cbind(out_df, In.union = mg_in_union)

    } else {
        if(!is.null(input_data_folder_path)) {
            denominator_counts_csv_filename <- file.path(input_data_folder_path, denominator_counts_csv_filename)
        }
        if(!file.exists(denominator_counts_csv_filename)) stop("'denominator_counts_csv_filename' does not exist.")
        message("Reading denominator counts from '", file.path(denominator_counts_csv_filename), "'.")

        out_df <- data.frame()
        for (mg in marital_group) {
            if (identical(marital_group, "married")) mg_in_union <- 1
            else mg_in_union <- 0
            out_df <- rbind(out_df,
                            data.frame(extractDenominators(denominator_counts_csv_filename,
                                                           in_union = mg_in_union),
                                       In.union = mg_in_union))
        }
    }

    ## Check denominator counts
    na_counts <- which(is.na(out_df), arr.ind = TRUE)
    na_counts_rows <- unique(na_counts[, "row"])
    if (length(na_counts)) {
        msg <- "The following countries have 'NA' denominator counts:"
        for (i in seq_along(na_counts_rows)) {
            msg <- paste0(msg, "\n\t",
                          out_df[na_counts_rows[i], "Country"],
                          " (",
                          toString(gsub("X", "",
                                        colnames(out_df)[na_counts[na_counts[, "row"] == na_counts_rows[i], "col"]])),
                          ").")
        }
        stop(msg)
    }

    ## Check aggregates
    if (is.character(countries_for_aggregates_csv_filename)) {
        if(!is.null(input_data_folder_path)) {
            countries_for_aggregates_csv_filename <-
                file.path(input_data_folder_path, countries_for_aggregates_csv_filename)
        }
        if(!file.exists(countries_for_aggregates_csv_filename))
            stop("'countries_for_aggregates_csv_filename' does not exist.")
        countries_for_aggregates <- read.csv(countries_for_aggregates_csv_filename, row.names = NULL)
        isos_not_in_data <-
            unique(countries_for_aggregates$ISO.Code)[!unique(countries_for_aggregates$ISO.Code) %in% out_df$ISO.code]
        if (length(isos_not_in_data))
            stop("ISOs ",
                    toString(isos_not_in_data),
                 " are listed in 'countries_for_aggregates_csv_filename' but are not in denominator counts file.")
        out_df_sum <- cbind(out_df[, c("ISO.code", "Country", "In.union")],
                            sum = rowSums(out_df[, which(!colnames(out_df) %in% c("ISO.code", "Country", "In.union"))]))
        out_df_sum_zero <- out_df_sum$ISO.code[which(out_df_sum$sum <= 0)]
        isos_zero_pop <-
            unique(countries_for_aggregates$ISO.Code)[unique(countries_for_aggregates$ISO.Code) %in% out_df_sum_zero]
        if (length(isos_zero_pop)) {
            msg <- paste0("The following countries appear in the file '",
                 countries_for_aggregates_csv_filename,
                 "' but their denominator counts are all zero:\n", toString(out_df_sum[out_df_sum_zero, "Country"]))
            if (grepl("res\\.country\\.rda", denominator_counts_csv_filename))
                msg <- paste0(msg, "\nNote: denominator counts were read from '",
                              denominator_counts_csv_filename,
                              "'. If you are using new denominator counts in a .csv file you will need to delete 'res.country.rda' and re-run 'post_process_mcmc'.")
            stop(msg)
            ## NOTE: Output might be truncated (see ?stop).
        }
    }
    message("'", denominator_counts_csv_filename, "' is valid.")
    return(invisible(out_df))
}



##' Generate MCMC chains for global run of FPEM
##'
##' This function generates an MCMC sample for a global run of FPEM for a single
##' marital group. No post-processing or results generation is done; only the
##' chains are produced. The recommended way to use this function is via a call
##' to \code{\link{do_global_run}} or \code{\link{do_global_all_women_run}}.
##'
##' See \dQuote{Details} in the help file for \code{\link{do_global_all_women_run}}.
##'
##' @param run_desc Character. Brief note to be appended to the
##'     auto-generated \code{run_name}. Ignored if
##'     \code{run_name_override} is non-\code{NULL}.
##' @param run_name_override Character. User defined run name to
##'     override default generation. \code{run_desc} is ignored if
##'     this is non-\code{NULL}.
##' @param marital_group Character. The marital group for which a run
##'     of the model is desired.
##' @param age_group Character. The age group for which a run of the
##'     model is desired, specified in the format \dQuote{xx-yy},
##'     where \dQuote{xx} is the start age, \dQuote{yy} is the end
##'     age, e.g., \code{"15-49"} or \code{"15-19"}. This is used to
##'     form the run name if \code{run_name_override} is \code{NULL},
##'     to name of the file containing prevalence data if
##'     \code{data_csv_filename} is \code{NULL}, and to select rows
##'     from the denominator counts file if post-processing is done
##'     (see \code{\link{post_process_mcmc}}).
##' @param estimation_iterations Numeric. Number of MCMC iterations
##'     that should be \emph{saved}. This is \emph{before}
##'     \code{thinning}.
##' @param burn_in_iterations Numeric. Number of MCMC iterations that
##'     should be run as burn-in before starting to save them.
##' @param steps_before_progress_report Numeric. The number of times
##'     progress should reported during MCMC sampling.
##' @param thinning Numeric. The actual number of iterations saved is
##'     \eqn{\frac{\code{estimation_iterations}}{\code{thinning}}}{\code{estimation_iterations}/\code{thinning}}.
##' @param chain_nums Numeric. The number of MCMC chains to run,
##'     \emph{as a sequence}. E.g., for three chains use
##'     \code{1:3}. You need to run at least two chains for
##'     post-processing to be successful.
##' @param set_seed_chains Set the random seed passed to
##'     \code{JAGS}. For multiple chains, the seed for each is
##'     \code{set_seed_chains} multiplied by the chain number.
##' @param run_in_parallel Logical. Determines if MCMC chains are run
##'     in parallel. Parallel running requires package
##' #ifdef windows
##' \pkg{doParallel}.
##' #endif
##' #ifndef windows
##'
##' \pkg{doParallel} or \pkg{doMC}.
##' #endif
##' Defaults to serial running if \code{run_in_parallel = TRUE} but the package
##' is not available.
##' @param input_data_folder_path File path to folder containing
##'     \emph{all} input data (except any map shapefiles). If
##'     \code{NULL} the value of \code{data_csv_filename}, etc., will
##'     be passed to \code{\link{file.path}} as-is. Otherwise,
##'     \code{file.path(input_data_folder_path, data_csv_filename)}
##'     will be passed. The default value points to the data directory
##'     supplied with the package.
##' @param data_csv_filename Filename of the \file{.csv} file
##'     containing country-level prevalence data. See
##'     \dQuote{Details}.
##' @param region_information_csv_filename Filename of the \file{.csv}
##'     file containing classifications of countries in sub-regions,
##'     regions, etc. See \dQuote{Details}.
##' @param output_folder_path Filepath to directory where outputs
##'     should be saved. If \code{NULL}, defaults to
##'     \code{file.path("output", run_name)}.
##' @param include_AR Logical; should the auto-regressive component of
##'     the model be estimated. Used mainly for testing.
##' @param verbose Logical; print lots and lots of messages about
##'     progress?
##' @return A name for the run returned invisibly as a character
##'     string. MCMC chains are saved to the directory
##'     \file{\code{output_folder_path}/temp.JAGSobjects}. They need
##'     to be post-processed with \code{\link{post_process_mcmc}}. The
##'     run name (and path to outputs, if not the default) must be
##'     passed to \code{\link{post_process_mcmc}} to locate find the
##'     saved chains for processing. Run names for married and
##'     unmarried runs must also be passed to
##'     \code{\link{combine_runs}} to generate all women MCMC results.
##' @author Mark Wheldon, Andrew Tait
##' @seealso \code{\link{do_global_run}} (which calls this function)
##'     to generate MCMC results for married or unmarried women,
##'     post-process, and produce results all in one call;
##'     \code{\link{combine_runs}} to create all women results from
##'     married and unmarried women runs;
##'     \code{\link{do_global_all_women_run}} to do married,
##'     unmarried, \emph{and all women runs}, and produce results, all
##'     in one call.
##' @examples vignette("FPEMglobal_Intro")
##' @export
do_global_mcmc <- function(run_desc = "",
                           run_name_override = NULL,
                           marital_group = c("married", "unmarried"),
                           age_group = "15-49",
                           estimation_iterations = 3,
                           burn_in_iterations = 1,
                           steps_before_progress_report = 4,
                           thinning = 2,
                           chain_nums = 1:3,
                           set_seed_chains = 1,
                           run_in_parallel = isTRUE(length(chain_nums) > 1),
                           input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
                           data_csv_filename = paste0("data_cp_model_all_women_", age_group, ".csv"),
                           region_information_csv_filename = "country_and_area_classification_pre2024.csv",
                           output_folder_path = NULL,
                           include_AR = TRUE,
                           verbose = FALSE) {

    ##---------------------------------------------------------------------
    ## Run name and output paths

    marital_group <- match.arg(marital_group)

    if(!is.null(run_name_override)) run_name <- run_name_override
    else run_name <- make_run_name(marital_group, age_group, run_desc)

    message("\nThis run has 'run_name': ", run_name)

    if(is.null(output_folder_path)) output_folder_path <- file.path("output", run_name)

    if(!dir.exists(output_folder_path)) {
        dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)
    } else {
        if(any(grepl("^mcmc\\.info(\\.[0-9]+\\.|\\.)rda$", dir(output_folder_path)),
               na.rm = TRUE) ||
           file.exists(file.path(output_folder_path, "mcmc.meta.rda")) ||
           file.exists(file.path(output_folder_path, "mcmc.array.rda"))) {
            stop("Directory '", output_folder_path, "' already exists with some MCMC output files. Change the run name, output folder path, or delete the existing run and start again.")
        }
    }

    ##---------------------------------------------------------------------
    ## Make paths to input data

    if(!is.null(input_data_folder_path)) {
        data_csv_filename <- file.path(input_data_folder_path, data_csv_filename)
        region_information_csv_filename <-
            file.path(input_data_folder_path, region_information_csv_filename)
    }
    if(!file.exists(data_csv_filename)) stop("'data_csv_filename' ",
                                             data_csv_filename,
                                             " does not exist.")
    if(!file.exists(region_information_csv_filename))
        stop("'region_information_csv_filename' ",
             region_information_csv_filename,
             " does not exist.")

    ##---------------------------------------------------------------------
    ## Parallelization mechanism

    if (run_in_parallel) {
        if (requireNamespace("doMC", quietly = TRUE)) {
            doMC::registerDoMC(min(parallel::detectCores(), length(chain_nums)))
            message("Running with ", foreach::getDoParWorkers(), " core(s)")
        } else {
            if (requireNamespace("doParallel", quietly = TRUE)) {
                conn_tries <- 0
                ## Try to handle non-stoppage of older clusters that might have used up all available cores.
                while(conn_tries < 5) {
                    conn_tries <- conn_tries + 1
                    try_cl <- try(cl <- parallel::makeCluster(min(parallel::detectCores(), length(chain_nums))))
                    if (identical(class(try_cl), "try-error")) {
                        message("'parallel::makeCluster' failed at try ", conn_tries, " of 5:\n\t",
                                paste(try_cl), "\n\n")
                        if (conn_tries < 5) {
                            message("Date-time is ", format(System.time()), "\n",
                                    "Sleeping for 30 minutes; will retry at ", format(System.time()) + (30 * 60))
                        }
                        else message("Tried 5 times; quitting.")
                    } else break()
                }
                doParallel::registerDoParallel(cl)
                on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
                message("Running with ", foreach::getDoParWorkers(), " core(s)")
            } else {
                warning("Package 'doParallel' not installed; chains will be run in serial.")
            }
        }
    }

    ##---------------------------------------------------------------------
    ## Marital group specific arguments

    model_family <- "rate"
    model_name <- NULL

    marital_group_param_set <-
        marital_age_group_param_defaults(marital_group, age_group, model_family,
                                         model_name)

    ##---------------------------------------------------------------------
    ## Log

    msg <- paste0("Starting MCMC sampler for run ", run_name)
    message(msg)

    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##---------------------------------------------------------------------
    ## Save the values of function arguments so same arguments can be used for validations

    global_mcmc_args <- c(mget(names(formals(do_global_mcmc))), marital_group_param_set,
                          list(run_name = run_name))
    save(global_mcmc_args, file = file.path(output_folder_path, "global_mcmc_args.RData"))

    ##-----------------------------------------------------------------------------
    ## Copy to Output

    output_data_folder_path <- file.path(output_folder_path, "data")
    copy_data_files(run_name = run_name, data_dir = input_data_folder_path,
                    data_local = output_data_folder_path)

    ##---------------------------------------------------------------------
    ## Make MCMC chains

    RunMCMC(
        N.ITER = estimation_iterations,
        N.BURNIN = burn_in_iterations,
        N.STEPS = steps_before_progress_report,
        N.THIN = thinning,
        run.on.server = run_in_parallel,
        run.name =  run_name,
        data.csv = data_csv_filename,
        regioninfo.csv = region_information_csv_filename,
        output.dir = output_folder_path,
        ChainNums = chain_nums,
        disagg.RN.PMA = TRUE,
        write.model.fun = marital_group_param_set$write_model_fun,
        include.AR = include_AR,
        seed.MCMC = set_seed_chains,
        marital.group = switch(marital_group, "married" = "MWRA", "unmarried" = "UWRA"),
        age.group = age_group,
        uwra.z.priors = marital_group_param_set$uwra_z_priors,
        uwra.Omega.priors = marital_group_param_set$uwra_Omega_priors,
        uwra.kappa.c.priors = marital_group_param_set$uwra_kappa_c_priors,
        include.c.no.data = ModelFunctionInclNoData(marital_group_param_set$write_model_fun),
        timing.world.priors = marital_group_param_set$timing_world_priors,
        EA.bias.negative = marital_group_param_set$EA_bias_negative,
        HW.bias.negative = marital_group_param_set$HW_bias_negative,
        sink.seed.logfile = FALSE,
        verbose = verbose
    )

    ## LOG
    msg <- paste0("MCMC sampling completed for run ", run_name)
    message(msg)

    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##---------------------------------------------------------------------
    ## Return

    return(invisible(run_name))

}


##' Add an MCMC chain to a global run of FPEM
##'
##' At least two chains are required to post-process the results of a run of the
##' FPEM model. Multiple chains can be run by \code{\link{do_global_mcmc}} but, if additional chains are desired,
##' this function can be used to add them. There must be at least one chain
##' already started (identified via \code{run_name}).
##'
##' @param run_name The name of the run to add a chain to.
##' @param chain_nums number identifying chains to add. Unlike
##'     \code{\link{do_global_mcmc}}, this can be a scalar. It must \emph{not}
##'     be the number identifying a chain already created. See \dQuote{Description}.
##' @inheritParams do_global_mcmc
##' @return A name for the run is returned invisibly as a character string. MCMC chains are
##'     saved to \file{output_folder_path/temp.JAGSobjects}. They need to
##'     be post-processed with \code{\link{post_process_mcmc}}. The run name
##'     must be passed to \code{\link{post_process_mcmc}} to locate find the
##'     saved chains for processing. Run names for married and unmarried runs
##'     must also be passed to \code{\link{combine_runs}} to generate all
##'     women MCMC results.
##' @author Mark Wheldon, Andrew Tait
##' @examples vignette("FPEMglobal_Intro")
##' @export
add_global_mcmc <- function(run_name,
                            chain_nums = 2,
                            output_folder_path = file.path("output", run_name),
                            run_in_parallel = isTRUE(length(chain_nums) > 1),
                            verbose = FALSE) {

    if (is.list(run_name)) stop("'run_name' is a list; choose a single run to add to.")

    ##---------------------------------------------------------------------
    ## Meta Info

    load(file.path(output_folder_path, "mcmc.meta.rda"), verbose = verbose)

    if (sum(is.element(chain_nums, mcmc.meta$general$ChainNums))>0){
        chain_nums <- setdiff(chain_nums, mcmc.meta$general$ChainNums)
        if (sum(chain_nums)==0){
            stop("MCMC run(s) for 'chain_nums' = ", chain_nums, " and 'run_name' = ", run_name
                ," already exist(s)!", "\n")
        }
    }

    ##---------------------------------------------------------------------
    ## LOG

    msg <- paste0("Adding MCMC chains to run ", run_name)
    message(msg)

    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##---------------------------------------------------------------------
    ## Parallelization mechanism

    if (run_in_parallel) {
        if (requireNamespace("doMC", quietly = TRUE)) {
            doMC::registerDoMC(min(parallel::detectCores(), length(chain_nums)))
            message("Running with ", foreach::getDoParWorkers(), " core(s)")
        } else {
            if (requireNamespace("doParallel", quietly = TRUE)) {
                cl <- parallel::makeCluster(min(parallel::detectCores(), length(chain_nums)))
                doParallel::registerDoParallel(cl)
                on.exit(parallel::stopCluster(cl), add = TRUE)
                message("Running with ", foreach::getDoParWorkers(), " core(s)")
            } else {
                warning("Package 'doMC' not installed; chains will be run in serial.")
            }
        }
    }

    ##---------------------------------------------------------------------
    ## Add Chains

    AddMCMCChain(run.name = run_name,
                 ChainNums = chain_nums,
                 write.model.fun = mcmc.meta$general$write.model.fun,
                 run.on.server = run_in_parallel
                 )
    ## LOG
    msg <- paste0("Finished adding MCMC chains to run ", run_name)
    message(msg)
    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    if (file.exists(file.path(output_folder_path, "post_process_args.RData")))
        warning("Chains have been added but any existing results have *not* been updated; you will need to re-run 'post_process_mcmc' *and* 'make_results' to update those.")

    return(invisible(run_name))
}

##' Post process MCMC chains from a global run of FPEM
##'
##' MCMC chains from a global run of FPEM for either married or
##' unmarried women (via \code{\link{do_global_mcmc}}) must be
##' post-processed before summary results (tables, plots) can be
##' produced. This function does the post-processing and saves the
##' results in \code{output_folder_path}. You need at least two chains
##' for post-processing to work. This function does not apply to all
##' women runs; see \code{\link{combine_runs}}.
##'
##' The counts of women by marital status, age, and year in
##' \code{denominator_counts_csv_filename} are used to convert prevalence
##' proportions to counts of women by contraceptive use status. See
##' \code{system.file("extdata", "data_cp_model_all_women_15-49.csv", package = "FPEMglobal")}
##' for an example of how the file should be formatted. Assume all columns are
##' required.
##'
##' By default, results are produced at the country level and for geographic
##' country aggregates. These are the sub-regions and regions of the \dfn{UN
##' M49} classification (\cite{UN DESA Statistics Division, 2017}).
##' For additional aggregates, argument
##' \code{special_aggregates_name} may be used. The argument takes the
##' \emph{name} of the aggregate. There must be a corresponding \file{.csv} file
##' in \code{input_data_folder_path}. The aggregate \dQuote{WHO_regions} is
##' included with the package; see \code{system.file("extdata", "WHO_regions.csv", package = "FPEMglobal")}
##' for the required format. Assum all columns are required.
##'
##' @references UN DESA Statistics Division,
##'     (2017) \emph{Standard Country or Area Codes for Statistical
##'     Use (M49)}. United Nations, Department of Economic and Social
##'     Affairs.  \url{https://unstats.un.org/unsd/methodology/m49/}
##' @param run_name The name of the run to post-process.
##' @param output_folder_path
##' @param input_data_folder_path File path to folder containing
##'     \emph{all} input data (except any map shapefiles). If
##'     \code{NULL} the value is taken from
##'     \code{file.path(output_folder_path, "global_mcmc_args.RData")}
##'     if that file exists, otherwise \code{file.path(output_folder_path, "data")}.
##' @param denominator_counts_csv_filename Name of the \file{.csv}
##'     file containing estimates and projections of the number of
##'     women by marital status, age, and year. See \dQuote{Details}.
##' @param countries_for_aggregates_csv_filename Name of the
##'     \file{.csv} file listing countries that will be used in constructing country aggregates.
##' @param start_year Estimates and projections are produced for a
##'     specified time interval. This is the start year of that
##'     interval.
##' @param end_year Estimates and projections are produced for a
##'     specified time interval. This is the end year of that
##'     interval.
##' @param years_change A two-column matrix giving the year pairs (as
##'     rows) between which probabilistic estimates of changes in the
##'     indicators are desired.
##' @param years_change2 A three-column matrix giving the year triples
##'     (as rows) among which to compute probabilistic estimates of
##'     change-in-changes.
##' @param model_diagnostics Logical; should convergence diagnostics
##'     and WAIC be computed? These are not re-done if the folder \sQuote{\code{output_folder_path}/convergence} exists.
##' @param make_any_aggregates Logical. Should country aggregates of
##'     any kind (including default aggregates) be produced?
##' @param special_aggregates_name Character vector of names
##'     (\emph{not} filenames) of any speical aggregates
##'     desired. There must be a corresponding file with name
##'     \file{\code{special_aggregates_name}.csv} in
##'     \code{input_data_folder_path} that defines the special
##'     aggregates. See \dQuote{Details}.
##' @param summarize_global_run Logical; should the model summary for one-country runs be produced?
##' @param age_ratios_age_total_run_name Run name of the 15--49 run to use as the denominator for age ratios. Calculate ratios of users in a subset age range (e.g., 15--19) to users in the total age range (15--49) from this run. Requires a completed 15--49 run.
##' @param age_ratios_age_total_output_folder_path Alternative way of specifying the run to use to make age ratios (see \code{age_ratios_age_total_run_name}. File path to output
##'     directory of the 15--49 run to use to make age ratios.
##' @param age_ratios_age_total_denominator_counts_csv_filename Name of
##'     the \file{.csv} file containing estimates and projections of
##'     the number of women by marital status, age, and year, for the
##'     age group 15--49. Only used if \code{make_age_ratios} is
##'     \code{TRUE}. Searched for in \code{age_ratios_age_total_denominator_counts_folder_path}.
##' @param age_ratios_age_total_denominator_counts_folder_path Path to
##'     \code{age_ratios_age_total_denominator_counts_csv_filename}. If
##'     \code{NULL}, defaults to
##'     \code{file.path(age_ratios_age_total_output_folder_path, "data")}.
##' @inheritParams do_global_mcmc
##' @return The run name (invisibly). The function is mainly called
##'     for its side effects.
##' @author Mark Wheldon, Andrew Tait
##' @seealso \code{\link{do_global_run}} (which calls this function)
##'     to generate MCMC results for married or unmarried women,
##'     post-process, and produce results all in one call;
##'     \code{\link{combine_runs}} to create all women results
##'     from married and unmarried women runs;
##'     \code{\link{do_global_all_women_run}} to do married,
##'     unmarried, \emph{and all women runs}, and produce results, all
##'     in one call.
##' @examples vignette("FPEMglobal_Intro")
##' @export
post_process_mcmc <- function(run_name = NULL,
                              output_folder_path = file.path("output", run_name),
                              input_data_folder_path = NULL,
                              denominator_counts_csv_filename = NULL,
                              countries_for_aggregates_csv_filename = "countries_mwra_195_pre2024.csv",
                              start_year = 1970.5,
                              end_year = 2030.5,
                              years_change = matrix(c(
                                  1990.5, 2000.5,
                                  2000.5, 2019.5,
                                  2019.5, 2030.5,
                                  2012.5, 2019.5,
                                  2012.5, 2019.5,
                                  2012.5, 2020.5),
                                  ncol = 2, byrow = TRUE),
                              years_change2 = matrix(
                                  c(2005.5, 2010.5, 2015.5,
                                    2000.5, 2005.5, 2010.5,
                                    1995.5, 2000.5, 2005.5,
                                    1990.5, 1995.5, 2000.5,
                                    1990.5, 2000.5, 2010.5,
                                    2000.5, 2010.5, 2019.5), ncol = 3, byrow = TRUE),
                              model_diagnostics = TRUE,
                              make_any_aggregates = TRUE,
                              special_aggregates_name = NULL,
                              summarize_global_run = TRUE,
                              age_ratios_age_total_run_name = NULL,
                              age_ratios_age_total_output_folder_path = NULL,
                              age_ratios_age_total_denominator_counts_csv_filename = "number_of_women_15-49.csv",
                              age_ratios_age_total_denominator_counts_folder_path = NULL,
                              verbose = FALSE) {

    ##----------------------------------------------------------------------------
    ## Meta Info

    if (is.null(output_folder_path) && is.null(run_name))
        stop("'output_folder_path' or 'run_name' must be specified.")

    if (is.null(output_folder_path)) output_folder_path <- file.path("output", run_name)

    global_mcmc_args_filepath <- file.path(output_folder_path, "global_mcmc_args.RData")
    if (file.exists(global_mcmc_args_filepath)) {
        load(global_mcmc_args_filepath, verbose = verbose)
    } else stop("Cannot find '", global_mcmc_args_filepath, "'.")

    if (is.null(run_name)) run_name <- get_run_name_from_args(get(load(global_mcmc_args_filepath)))

    load(file.path(output_folder_path, "mcmc.meta.rda"), verbose = verbose)
    write_model_function <- mcmc.meta$general$write.model.fun

    msg <- paste0("Post-processing run ", run_name)
    message(msg)

    ## LOG
    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    if(is.null(input_data_folder_path))
        input_data_folder_path <- file.path(output_folder_path, "data")

    ## Make filepaths that need 'age_group'
    if(is.null(denominator_counts_csv_filename)) {
        denominator_counts_csv_filename <-
            paste0("number_of_women_", mcmc.meta$general$age.group, ".csv")
    }

    ## Make paths to input data
    if(!is.null(input_data_folder_path)) {
        countries_for_aggregates_csv_filename <-
            file.path(input_data_folder_path, countries_for_aggregates_csv_filename)
        denominator_counts_csv_filename <-
            file.path(input_data_folder_path, denominator_counts_csv_filename)
    }
    if(!file.exists(countries_for_aggregates_csv_filename))
        stop("can't find ", countries_for_aggregates_csv_filename)
    if(!file.exists(denominator_counts_csv_filename))
        stop("can't find ", denominator_counts_csv_filename)

    ## All women run?
    if(isTRUE(mcmc.meta$general$all.women.run.copy)) {
        warning("'post_process_mcmc' was called on results for all women. This function only applies to runs for married or unmarried women. Did you mean to call 'combine_runs'?. Exiting .. ")
        return(invisible())
    }

    ## Validate denominators
    if (file.exists(file.path(output_folder_path, "res.country.rda")))
        denom_file_to_validate <- "res.country.rda"
    else denom_file_to_validate <- denominator_counts_csv_filename
    validate_denominator_counts_file(age_group = NULL,
                                     input_data_folder_path = NULL,
                                     denominator_counts_csv_filename = denom_file_to_validate,
                                     output_folder_path = output_folder_path,
                                     marital_group = switch(mcmc.meta$general$marital.group,
                                                            "MWRA" = "married",
                                                            "UWRA" = "unmarried",
                                                            "AWRA" = c("married", "unmarried")),
                                     countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename)

    ## Age ratios
    if(!is.null(age_ratios_age_total_run_name) || !is.null(age_ratios_age_total_output_folder_path)) {
        if(identical(mcmc.meta$general$age.group, "15-49")) {
            make_age_ratios <- FALSE
            age_ratios_age_total_run_name <- NULL
            age_ratios_age_total_output_folder_path <- NULL
            message("\nAge ratios not created for age group '15-49'.")
        } else {
            make_age_ratios <- TRUE
        }
    } else {
        make_age_ratios <- FALSE
    }

    if(make_age_ratios) {
        if(is.null(age_ratios_age_total_output_folder_path)) {
            age_ratios_age_total_output_folder_path <-
                file.path("output", age_ratios_age_total_run_name)
        }
        if(is.null(age_ratios_age_total_denominator_counts_folder_path)) {
            age_ratios_age_total_denominator_counts_folder_path <-
                file.path(age_ratios_age_total_output_folder_path, "data")
        }

    if(!dir.exists(age_ratios_age_total_output_folder_path))
        stop("'age_ratios_age_total_output_folder_path' does not exist ('",
             age_ratios_age_total_output_folder_path,
             "')")
    if(!file.exists(file.path(age_ratios_age_total_denominator_counts_folder_path,
                              age_ratios_age_total_denominator_counts_csv_filename)))
        stop("'age_ratios_age_total_output_folder_path' does not exist ('",
             age_ratios_age_total_output_folder_path,
             "')")
    }

    ## Special aggregates
    for (name.agg in special_aggregates_name) {
        if(!is.null(input_data_folder_path)) {
            file.agg <- file.path(input_data_folder_path, paste0(name.agg, ".csv"))
        } else {
            file.agg <- paste0(name.agg, ".csv")
        }
        if(!file.exists(file.agg)) stop("'", file.agg, "' does not exist.")
    }

    ##---------------------------------------------------------------------
    ## Save the values of function arguments for making results

    post_process_args <- mget(names(formals(post_process_mcmc)))
    save(post_process_args, file = file.path(output_folder_path, "post_process_args.RData"))


    ##----------------------------------------------------------------------------
    ## Construct output

    ## Make MCMC array

    if (file.exists(file.path(output_folder_path, "temp.JAGSobjects")) &&
        !file.exists(file.path(output_folder_path, "mcmc.array.rda"))) {

        message("\nConstructing MCMC Array from 'temp.JAGSobjects'. You can delete those folders when done.")

        ConstructMCMCArray(run.name = run_name, output.dir = output_folder_path)
    } else {
        message("\nMCMC Array *NOT* constructed; either 'temp.JAGSobjects' does not exist or 'mcmc.array.rda' already exists.")
    }

    ## Make country trajectories

    if (!file.exists(file.path(output_folder_path, "res.country.rda"))
        || !file.exists(file.path(output_folder_path, "res.aggregate.rda"))
        || !file.exists(file.path(output_folder_path, "par.ciq.rda"))) {

        message("\nConstructing output objects, including standard aggregates.")

        if(!file.exists(file.path(output_folder_path, "mcmc.array.rda"))) {
            if (!dir.exists(file.path(output_folder_path, "countrytrajectories")) &&
                !length(dir(file.path(output_folder_path, "countrytrajectories"))) > 1)
                stop("'mcmc.array.rda' was not created. Did you run enough chains?")
        }

        ConstructOutput(
            run.name = run_name,
            WRA.csv = denominator_counts_csv_filename,
            countries.to.include.in.aggregates.csv = countries_for_aggregates_csv_filename,
            output.dir = output_folder_path,
            start.year = start_year,
            end.year = end_year,
            years.change = years_change,
            make.any.aggregates = make_any_aggregates,
            verbose = verbose
        )
    } else {
        message("\nOutput objects *NOT* constructed; 'res.country.rda', 'res.aggregate.rda' and 'par.ciq.rda' all already exist.")
    }

    ## Special aggregates

    if (!is.null(special_aggregates_name) && make_any_aggregates) {

        for (name.agg in special_aggregates_name) {

            if(!is.null(input_data_folder_path)) {
                file.agg <- file.path(input_data_folder_path, paste0(name.agg, ".csv"))
            } else {
                file.agg <- paste0(name.agg, ".csv")
            }

            res.fname <- file.path(output_folder_path, paste0(name.agg, ".rda"))
            if(file.exists(res.fname)) {
                message("\nOutput object for special aggregate '", name.agg, "' *NOT* created; '",
                        res.fname, "' already exists.")
                next()
            }

            message("\nMaking aggregates for ", name.agg, " from ", file.agg, ".")

            res.new <- GetAggregates(run.name = run_name,
                                     output.dir = output_folder_path,
                                     file.aggregates = file.agg,
                                     years.change = years_change,
                                     years.change2 = years_change2,
                                     countries.to.include.in.aggregates.csv = countries_for_aggregates_csv_filename,
                                     verbose = verbose
                                     )
            save(res.new, file = res.fname)

            ## Copy spec aggregate files
            output_data_folder_path <- file.path(output_folder_path, "data")
            if(!dir.exists(output_data_folder_path))
                dir.create(output_data_folder_path, recursive = TRUE, showWarnings = FALSE)
            copy_uwra_mwra_files(basename(file.agg),
                                 awra_output_folder_path = output_data_folder_path, #<- TO DIRECTORY
                                 mwra_uwra_output_folder_path = input_data_folder_path, #<- FROM DIRECTORY
                                 )
        }
    }

    ## Age ratios

    if(make_age_ratios) {

        if(!file.exists(file.path(output_folder_path, "res.country.age.ratio.rda"))) {

            message("\nMaking country age ratios")

            ConstructAgeRatios(age.subset.output.dir = output_folder_path,
                               age.total.output.dir = age_ratios_age_total_output_folder_path,
                               age.ratio.output.dir = output_folder_path,
                               est.years = NULL,
                               run.name = run_name,
                               age.subset.WRA.csv = denominator_counts_csv_filename,
                               age.total.WRA.csv = file.path(age_ratios_age_total_denominator_counts_folder_path,
                                                             age_ratios_age_total_denominator_counts_csv_filename),
                               years.change = years_change,
                               years.change2 = years_change2,
                               verbose = verbose)

        } else {
            warning("'res.country.age.ratio.rda' already exists. Age ratios for countries not re-created.")
        }
    }

    ##----------------------------------------------------------------------------
    ## Summarize global run

    if (summarize_global_run) {
        message("\nSummarizing global run.")

        if (file.exists(file.path(output_folder_path, "mcmc.array.rda"))) {
            SummariseGlobalRun(         #This creates (sufficient?) input for one-country run.
                run.name = run_name,
                output.dir = output_folder_path,
                write.model.fun = write_model_function
            )
        } else {
            warning("Summarization of global run failed: '",
                    file.path(output_folder_path, "mcmc.array.rda"),
                    "' mcmc.array.rda' does not exist.")
        }
    }

    ##----------------------------------------------------------------------------
    ## Check convergence

    if(model_diagnostics) {
        if(dir.exists(file.path(output_folder_path, "convergence"))) {
            message("\nModel diagnostics will *NOT* be re-done; they already exist. Delete '",
                    file.path(output_folder_path, "convergence"),
                    "' and re-run if you want new ones.")
        } else {

            message("\nChecking convergence.")

            if (file.exists(file.path(output_folder_path, "mcmc.array.rda"))) {

                CheckConvergence(
                    run.name = run_name,
                    output.dir = output_folder_path,
                    check.convergence = TRUE,
                    png.traceplots = TRUE,
                    fig.dir =file.path(output_folder_path, "convergence"),
                    sink.convergence.log = FALSE
                )
            } else {
                warning("Checking convergence failed: '",
                        file.path(output_folder_path, "mcmc.array.rda"),
                        "' mcmc.array.rda' does not exist.")
            }
        }
    }

    ##----------------------------------------------------------------------------
    ## LOG

    msg <- paste0("Finished post-processing run ", run_name)
    message(msg)
    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##----------------------------------------------------------------------------
    ## Return

    return(invisible(run_name))

}


##' Summarize a post-processed global run of FPEM
##'
##' Once post-processed, a global model can be used to generate
##' results in the form of summary figures and tables of key
##' indicators. Summaries are in the form of quantiles of posterior marginal
##' distributions, and means for some indicators.
##'
##' The indicators for which these summaries are generated are, by
##' country and country aggregate, by year:
##' \itemize{
##'   \item Contraceptive prevalence, any method (CP)
##'   \item Contraceptive prevalence, traditional methods (CP modern)
##'   \item Contraceptive prevalence, modern methods (CP traditional)
##'   \item Unmet need for (modern) contraception (unmet need)
##'   \item CP + Unmet need
##'   \item Met demand for any method of contraception
##'   \item Met demand for modern methods of contraception
##'   \item Ratio of CP modern to CP
##'   \item Probability that met demand for modern methods is greater than 75 percent
##' }
##'
##' @section Age ratios for aggregates:
##'     \code{\link{post_process_mcmc}} generates trajectories for the
##'     age ratios for countries for married, unmarried, and all
##'     women. However, \code{\link{combine_runs}}, not
##'     \code{\link{post_process_mcmc}}, produces the trajectories for
##'     age ratios for country aggregates, including those for married
##'     and unmarried women. When \code{\link{make_results}} is
##'     applied to an all women run, an attempt is made to find the
##'     corresponding
##'
##' @param countries_in_CI_plots_csv_filename Name of \file{.csv} file
##'     that lists the countries to be included in the main
##'     country-level indicator plots. These are the plots saved in
##'     \file{\code{output_folder_path}/fig/\var{[run
##'     name]}CIs.pdf}. The format is the same as
##'     \code{countries_for_aggregates_csv_filename}. The file is
##'     looked for in \code{input_data_folder_path}. Countries appear
##'     in the \file{.pdf} in the same order as they are listed in
##'     \code{countries_in_CI_plots_csv_filename}.
##' @param plot_diagnostic_CI_plots Logical. Produce diagnostic
##'     versions of the main country-level indicator plots. These have
##'     zoomed y-axes.
##' @param make_all_bar_charts Logical. Produce barcharts? If
##'     \code{NULL} but \code{plot_barchart_years} is non-\code{NULL},
##'     is reset to \code{TRUE}.
##' @param plot_CI_changes_years Vector of length two (if longer, only
##'     the first and last elements are used). Declares the years to
##'     be used to make the \dQuote{fish bone} plots, i.e., the plots
##'     appearing in files
##'     \file{\code{output_folder_path}/fig/\var{[run
##'     name]}_CIspropsubregional_modern_UNPD.pdf}. These must be in
##'     the range of \code{start_year} and \code{end_year} passed to
##'     \code{\link{post_process_mcmc}}.
##' @param plot_barchart_years Vector of years for which bar charts
##'     should be produced. These are saved to
##'     \file{\code{output_folder_path}/fig/barchart}. These must be
##'     in the range of \code{start_year} and \code{end_year} passed
##'     to \code{\link{post_process_mcmc}}.
##' @param plot_maps_shapefile_folder Path to directory containing
##'     shapefiles for maps. Only needed if \code{plot_maps} is
##'     \code{TRUE}. This is \emph{not} looked for in
##'     \code{input_data_folder_path}; it should be a full path to the
##'     shape files.
##' @param plot_maps_years Vector of years for which maps are desired;
##'     the full set of maps are produced for each of the years
##'     listed. These must be in the range of \code{start_year} and
##'     \code{end_year} passed to \code{\link{post_process_mcmc}}.
##' @param plot_parameters Logical. Should posterior quantiles of
##'     model parameters be added to the plots in legend boxes?
##' @param adjust_medians Logical. Should adjusted medians outputs be
##'     produced in addition to unadjusted outputs?
##' @param special_aggregates_name \emph{name} for special aggregates,
##'     if any. The default, \code{NULL}, means no special aggregates
##'     are produced. Note: this is \emph{not} a filename, although a
##'     corresponding file named
##'     \file{\code{special_aggregates_name}.csv} must be present in
##'     \code{input_data_folder_path}.
##' @param make_age_ratios Logical. Should results for age-ratios be
##'     made? If \code{NULL} this is set based on the presence of
##'     files \dQuote{res.country.age.ratio.rda} or
##'     \dQuote{res.country.all.women.age.ratio.rda} in
##'     \code{output_folder_path}.
##' @param validation_keep_all Logical. Should validation results be
##'     saved? If \code{TRUE}, a list with elements \code{CI.df} and
##'     \code{Error.df} is saved to
##'     \file{\code{output_folder_path}/validn_repts/validation.results.rda}.
##' @param validation_return_res_as_df Logical; should the validation
##'     results be returned as a data frame? If \code{FALSE} the
##'     function returns the \code{run_name} invisibly as a character
##'     string.
##' @param married_women_run_name Run name of a married women
##'     run. Relevant if \code{\link{make_results}} is being run on an
##'     all women run and age ratios for country aggregates are
##'     wanted. If this argument is \code{NULL} (default), and
##'     \code{all_women} is \code{TRUE} (explicitly or implicitly), an
##'     attempt is made to find this folder by substituting
##'     \dQuote{all_women} with \dQuote{married} in the
##'     \code{run_name}.
##' @param unmarried_women_run_name Run name of a unmarried women
##'     run. See \code{married_women_run_name}.
##' @param married_women_output_folder_path Path to directory
##'     containing outputs for a married women run. See
##'     \code{married_women_run_name}.
##' @param unmarried_women_output_folder_path Path to directory
##'     containing outputs for a unmarried women run. See
##'     \code{married_women_output_folder_path}.
##' @param verbose
##' @inheritParams do_global_mcmc
##' @inheritParams post_process_mcmc
##' @return Either \code{run_name} invisibly as a character string or,
##'     if \code{isTRUE(validation_return_res_as_df)}, a data frame
##'     (non-invisibly) containing validation results.
##'
##' @author Mark Wheldon, Andrew Tait
##'
##' @seealso \code{\link{do_global_run}} (which calls this function)
##'     to generate MCMC results for married or unmarried women,
##'     post-process, and produce results all in one call;
##'     \code{\link{combine_runs}} to create all women results
##'     from married and unmarried women runs;
##'     \code{\link{do_global_all_women_run}} to do married, unmarried,
##'     \emph{and all women runs}, and produce results, all in one
##'     call.
##' @examples vignette("FPEMglobal_Intro")
##'
##' @export
make_results <- function(run_name = NULL,
                         output_folder_path = file.path("output", run_name),
                         input_data_folder_path = NULL,
                         countries_in_CI_plots_csv_filename = "countries_mwra_195_pre2024.csv",
                         CI_plots_years = NULL,
                         plot_diagnostic_CI_plots = FALSE,
                         make_all_bar_charts = NULL,
                         plot_barchart_years = NULL,
                         data_info_plot_years = c(1990, 2000, 2010),
                         plot_CI_changes_years = NULL,
                         plot_maps_shapefile_folder = NULL,
                         plot_maps_years = NULL,
                         plot_prior_post = NULL,
                         plot_parameters = NULL,
                         tabulate_changes = TRUE,
                         adjust_medians = TRUE,
                         make_any_aggregates = TRUE,
                         special_aggregates_name = NULL,
                         make_age_ratios = NULL,
                         validation_keep_all = TRUE,
                         validation_return_res_as_df = FALSE,
                         all_women = NULL,
                         verbose = FALSE) {

    ##----------------------------------------------------------------------------
    ## Meta information
    ##----------------------------------------------------------------------------

    ## Output folder path
    if (is.null(output_folder_path) && is.null(run_name))
        stop("'output_folder_path' or 'run_name' must be specified.")

    if (is.null(output_folder_path)) output_folder_path <- file.path("output", run_name)

    ## MCMC meta
    load(file.path(output_folder_path, "mcmc.meta.rda"), verbose = verbose)
    data_raw <- mcmc.meta$data.raw

    ## Validation run?
    validation_run <- !is.null(mcmc.meta$validation.list)

    ## All women run?
    if(is.null(all_women)) all_women <- isTRUE(mcmc.meta$general$all.women.run.copy)

    ## Unmarried women run?
    unmarried <-
        (identical(mcmc.meta$general$marital.group, "UWRA") && !all_women)

    ## Extra info
    if (!all_women) {
        global_mcmc_args_filepath <- file.path(output_folder_path, "global_mcmc_args.RData")
        if (file.exists(global_mcmc_args_filepath)) {
            load(global_mcmc_args_filepath, verbose = verbose)
        } else stop("Cannot find '", global_mcmc_args_filepath, "'.")
        post_process_args_filepath <- file.path(output_folder_path, "post_process_args.RData")
    } else {
        post_process_args_filepath <- file.path(output_folder_path, "combine_runs_args.RData")
    }

    ## Run name
    if (is.null(run_name)) run_name <- get_run_name_from_args(get(load(post_process_args_filepath)))

    ## Make paths to input data
    if(is.null(input_data_folder_path))
        input_data_folder_path <- file.path(output_folder_path, "data")

    ## Countries to plot
    if(!is.null(input_data_folder_path) && !is.null(countries_in_CI_plots_csv_filename)) {
        countries_in_CI_plots_csv_filename <-
            file.path(input_data_folder_path, countries_in_CI_plots_csv_filename)
    }
    if(!file.exists(countries_in_CI_plots_csv_filename)) {
        msg <- paste0("can't find 'countries_in_CI_plots_csv_filename' (",
                      countries_in_CI_plots_csv_filename, ")")
        if (all_women)
            msg <- paste0(msg, ". Has it been copied from the married or unmarried output's 'data' directory?")
        stop(msg)
    }

    ## Years to plot
    if(any(sapply(list(plot_barchart_years, plot_CI_changes_years, plot_maps_years),
                  "is.null"))) {
        if(file.exists(post_process_args_filepath)) {
            post_process_args <- get(load(post_process_args_filepath, verbose = verbose))
            if(is.null(plot_barchart_years)) {
                plot_barchart_years <-
                    c(floor(post_process_args$start_year),
                      floor(median(c(post_process_args$start_year, post_process_args$end_year))),
                      floor(post_process_args$end_year))
                message("\n'plot_barchart_years' deduced from '", post_process_args_filepath, "'.")
            }
            if(is.null(plot_CI_changes_years)) {
                plot_CI_changes_years <-
                    c(floor(post_process_args$start_year), floor(post_process_args$end_year))
                message("\n'plot_CI_changes_years' deduced from '", post_process_args_filepath, "'.")
            }
            if(is.null(plot_maps_years) && !is.null(plot_maps_shapefile_folder)) {
                plot_maps_years <-
                    floor(median(c(post_process_args$start_year, post_process_args$end_year)))
                message("\n'plot_maps_years' taken from '", post_process_args_filepath, "'.")
            }
        } else warning("'", post_process_args_filepath, "' does not exist: cannot determine at least one of 'plot_barchart_years', 'plot_CI_changes_years', or 'plot_maps_years'. Specify them as arguments.")
    }

    if (!is.null(CI_plots_years)) {
        if (!identical(length(CI_plots_years), 2L) || !all(sapply(CI_plots_years, function(z) is.numeric(z) || is.null(z)))) {
            stop("'CI_plots_years' must be a numeric vector of length 2, or a list of length 2 with numeric or 'NULL' elements.")
        }
        CI_plots_start <- CI_plots_years[[1]]
        if (!is.null(CI_plots_start) &&
            identical(as.numeric(CI_plots_start), as.numeric(floor(CI_plots_start))))
            CI_plots_start <- CI_plots_start + 0.5

        CI_plots_end <- CI_plots_years[[2]]
        if (!is.null(CI_plots_end) &&
            identical(as.numeric(CI_plots_end), as.numeric(floor(CI_plots_end))))
            CI_plots_end <- CI_plots_end + 0.5
    } else {
        CI_plots_start <- NULL
        CI_plots_end <- NULL
    }

    ## Make age ratios?
    if(is.null(make_age_ratios)) {
        if(!mcmc.meta$general$age.group == "15-49") {
            if(!all_women) {
                if(file.exists(file.path(output_folder_path, "res.country.age.ratio.rda"))) {
                    make_age_ratios <- TRUE
                } else {
                    make_age_ratios <- FALSE
                }
            } else if(file.exists(file.path(output_folder_path,
                                            "res.country.all.women.age.ratio.rda"))) {
                make_age_ratios <- TRUE
            } else {
                make_age_ratios <- FALSE
            }
        } else {
            make_age_ratios <- FALSE
        }
    } else if(!is.logical(make_age_ratios)) {
        stop("'make_age_ratios' must be 'NULL', 'TRUE', or 'FALSE'.")
    } else if(make_age_ratios) {
        if(mcmc.meta$general$age.group == "15-49") {
            warning("'make_age_ratios' is 'TRUE' but 'age_group' is '15-49'. Age ratios not created.")
            make_age_ratios <- FALSE
        } else if(!all_women) {
            if(!file.exists(file.path(output_folder_path, "res.country.age.ratio.rda"))) {
                warning("'make_age_ratios' is 'TRUE' but 'res.country.age.ratio.rda' is not in 'output_folder_path'. Age ratios not created.")
                make_age_ratios <- FALSE
            }
        } else {
            if(!file.exists(file.path(output_folder_path, "res.country.all.women.age.ratio.rda"))) {
                warning("'make_age_ratios' is 'TRUE' but 'res.country.all.women.age.ratio.rda' is not in 'output_folder_path'. Age ratios not created")
                make_age_ratios <- FALSE
            }
        }
    }

    ##----------------------------------------------------------------------------
    ## Adjusted medians??
    ##----------------------------------------------------------------------------

    if(adjust_medians) adjust_medians_method <- "mod_tot_unmet" #only one

    if(all_women && adjust_medians) {

        adj_m_stop_msg <- function(mar_gp, fn, spec_agg = "") {
            paste0("Adjusted median results for ", spec_agg, mar_gp, " women were not found in '",
                   output_folder_path,
                   "' ('", file.path(output_folder_path, fn),
                   "' not found). Make sure adjusted medians were produced for the ",
                   mar_gp, " women run ('post_process_mcmc' then 'make_results') AND you re-ran 'combine_runs()'. Otherwise set 'adjust_medians' to 'FALSE'.")
        }

        ## Countries

        adj_med_fn <-
            makeFileName(paste0("res.country.adj-", adjust_medians_method, ".rda"))
        uwra_adj_med_fn <- paste0("uwra_", adj_med_fn)
        mwra_adj_med_fn <- paste0("mwra_", adj_med_fn)

        if(!file.exists(file.path(output_folder_path, mwra_adj_med_fn)))
            stop(adj_m_stop_msg("married", mwra_adj_med_fn))

        if(!file.exists(file.path(output_folder_path, uwra_adj_med_fn)))
            stop(adj_m_stop_msg("unmarried", uwra_adj_med_fn))

        if (make_any_aggregates) {

            ## UNPD aggregates

            adj_med_fn_UNPDaggregate <-
                makeFileName(paste0("res.UNPDaggregate.adj-", adjust_medians_method, ".rda"))
            uwra_adj_med_fn_UNPDaggregate <- paste0("uwra_", adj_med_fn_UNPDaggregate)
            mwra_adj_med_fn_UNPDaggregate <- paste0("mwra_", adj_med_fn_UNPDaggregate)

            if(!file.exists(file.path(output_folder_path, mwra_adj_med_fn_UNPDaggregate)))
                stop(adj_m_stop_msg("married", mwra_adj_med_fn_UNPDaggregate))

            if(!file.exists(file.path(output_folder_path, uwra_adj_med_fn_UNPDaggregate)))
                stop(adj_m_stop_msg("unmarried", uwra_adj_med_fn_UNPDaggregate))

            ## Special aggregates

            if(!is.null(special_aggregates_name) && make_any_aggregates) {
                for (name.agg in special_aggregates_name) {

                    adj_med_fn_spec_agg <-
                        makeFileName(paste0(name.agg, "-", adjust_medians_method, ".rda"))
                    uwra_adj_med_fn_spec_agg <- paste0("uwra_", adj_med_fn_spec_agg)
                    mwra_adj_med_fn_spec_agg <- paste0("mwra_", adj_med_fn_spec_agg)

                    if(!file.exists(file.path(output_folder_path, mwra_adj_med_fn_spec_agg)))
                        stop(adj_m_stop_msg("married", mwra_adj_med_fn_spec_agg,
                                            spec_agg = paste0("special aggregate '",
                                                              name.agg,
                                                              "' ")))

                    if(!file.exists(file.path(output_folder_path, uwra_adj_med_fn_spec_agg)))
                        stop(adj_m_stop_msg("unmarried", uwra_adj_med_fn_spec_agg,
                                            spec_agg = paste0("special aggregate '",
                                                              name.agg,
                                                              "' ")))
                }
            }
        }
    }

    ##----------------------------------------------------------------------------
    ## Special aggregates
    ##----------------------------------------------------------------------------

    if (!is.null(special_aggregates_name) && make_any_aggregates) {
        for (name.agg in special_aggregates_name) {
            if(!all_women) {
                file.agg.rda <- file.path(output_folder_path, paste0(name.agg, ".rda"))
                                #^ Produced by 'post_process_mcmc()'
                if(!file.exists(file.agg.rda)) {
                    stop("Special aggregate file '", basename(file.agg.rda),
                         "' were not found in '", output_folder_path,
                         "'; the aggregate '", name.agg,
                         "'cannot be created without it. Make sure you have re-run 'post_process_mcmc()' with 'special_aggregates_name' specified correctly.")
                }
            } else {
                file.agg.rda <- file.path(output_folder_path, paste0(name.agg, ".all.women.rda"))
                                #^ Produced by 'combine_runs()'
                if(!file.exists(file.agg.rda)) {
                    stop("Special aggregate file '", basename(file.agg.rda),
                         "' were not found in '", output_folder_path,
                         "'; the aggregate '", name.agg,
                         "'cannot be created without it. Make sure the special aggregates were produced for the unmarried and married runs and you have re-run 'combine_runs()' with 'special_aggregates_name' specified correctly.")
                }
            }
        }
    }

    ##---------------------------------------------------------------------
    ## Save the values of function arguments for making results
    ##---------------------------------------------------------------------

    make_results_args <- mget(names(formals(make_results)))
    save(make_results_args, file = file.path(output_folder_path, "make_results_args.RData"))

    ##----------------------------------------------------------------------------
    ## Log
    ##----------------------------------------------------------------------------

    msg <- paste0("Making results for run ", run_name)
    message(msg)
    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##----------------------------------------------------------------------------
    ## Validation Run
    ##----------------------------------------------------------------------------

    if(validation_run) {

        fig_dir <- file.path(output_folder_path, "validn_repts")
        if(!dir.exists(fig_dir)) {
            dir.create(file.path(output_folder_path, "validn_repts"), recursive = TRUE, showWarnings = FALSE)
        }

        res_as_df <- PlotValidationResults(run.name = run_name
                                          ,output.dir = output_folder_path
                                          ,fig.dir = fig_dir,
                                          ,keep.all = validation_keep_all,
                                           UWRA = unmarried
                                           )

        ## LOG
        msg <- paste0("Finished making results for run ", run_name)
        message(msg)
        cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
            msg,
            file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

        if(validation_return_res_as_df) {
            return(res_as_df)
        } else {
            return(invisible(run_name))
        }

    } else {

        ##----------------------------------------------------------------------------
        ## Non-Validation
        ##----------------------------------------------------------------------------

        ##............................................................................
        ## Create plots

        if(!all_women) {

            ## Not yet implementend for all women

            ## Barcharts

            if(is.null(plot_barchart_years) || any(is.na(plot_barchart_years))) {
                if(isTRUE(make_all_bar_charts))
                    warning("'make_all_bar_charts' is 'TRUE' but 'plot_barchart_years' is 'NULL'. Bar charts will not be produced.")
                make_all_bar_charts <- FALSE
            } else {
                if(is.null(make_all_bar_charts)) make_all_bar_charts <- TRUE
            }

            if (make_all_bar_charts && !make_any_aggregates) {
                warning("'make_all_bar_charts' is 'TRUE' but 'make_any_aggregates' is 'FALSE'. Bar charts will not be produced.")
                make_all_bar_charts <- FALSE
            }

            if(make_all_bar_charts) {
                GetAllBarCharts(run.name = run_name, output.dir = output_folder_path,
                                fig.dir = file.path(output_folder_path, "fig", "barchart"),
                                years = plot_barchart_years)

                BarChartSubregion(
                    run.name = run_name,
                    output.dir = output_folder_path,
                    fig.dir = file.path(output_folder_path, "fig", "barchart"),
                    year = plot_barchart_years[length(plot_barchart_years)],
                    leave.out.less.than = 5*1E5,
                    x.axis.scale = 1E5
                )
            }
        }

        ## Data information plots

        if(!all_women) {

            ## Not yet implementend for all women

            data_info_folder_path <- file.path(output_folder_path, "fig", "data_info")

            if (!dir.exists(data_info_folder_path)) {
                dir.create(data_info_folder_path, recursive = TRUE, showWarnings = FALSE)
            }

            pdf(file = file.path(data_info_folder_path, paste0(run_name, "_datainfo_total.pdf")),
                width = 6, height = 12)
            PlotDataAvailability(data = data_raw$data, country.info = data_raw$country.info, summarize.unmet = FALSE,
                                 years = data_info_plot_years)
            dev.off()

            pdf(file.path(data_info_folder_path, paste0(run_name, "_datainfo_unmet.pdf")),
                width = 6, height = 12)
            PlotDataAvailability(data = data_raw$data, country.info = data_raw$country.info, summarize.unmet = TRUE,
                                 years = data_info_plot_years)
            dev.off()

        }

        ## CI plots

        ci_fig_folder_path <- file.path(output_folder_path, "fig", "CI")
        if (!dir.exists(ci_fig_folder_path)) {
            dir.create(ci_fig_folder_path, recursive = TRUE, showWarnings = FALSE)
        }

        if (is.null(plot_prior_post) && !all_women) plot_prior_post <- TRUE
        else plot_prior_post <- FALSE

        if (is.null(plot_parameters) && !all_women) plot_parameters <- TRUE
        else plot_parameters <- FALSE

        PlotResults(all.women = all_women,
                    UWRA = unmarried,
                    run.name = run_name,
                    output.dir = output_folder_path,
                    fig.dir = ci_fig_folder_path,
                    plot.ind.country.results = FALSE,
                    start.year = CI_plots_start,
                    end.year = CI_plots_end,
                    make.any.aggregates = make_any_aggregates,
                    layout.style = "UNPD",
                    cex.symbols = list(SS = 1.5, add.info = 4, no.info = 2),
                    non.std.symbol = 24,
                    select.c.csv = countries_in_CI_plots_csv_filename,
                    all.womenize.fig.name = FALSE,
                    hide.CP.tot.lt.1pc = TRUE,
                    plot.prior.post = plot_prior_post,
                    plot.parameters = plot_parameters
                    )

        if(plot_diagnostic_CI_plots) {
            PlotResults(all.women = all_women,
                        UWRA = unmarried,
                        run.name = run_name,
                        output.dir = output_folder_path,
                        fig.dir = ci_fig_folder_path,
                        plot.ind.country.results = FALSE,
                    start.year = CI_plots_start,
                    end.year = CI_plots_end,
                        make.any.aggregates = make_any_aggregates,
                        layout.style = "diagnostic",
                        cex.symbols = list(SS = 1.5, add.info = 4, no.info = 2),
                        non.std.symbol = 24,
                        select.c.csv = countries_in_CI_plots_csv_filename,
                        all.womenize.fig.name = FALSE,
                        hide.CP.tot.lt.1pc = TRUE,
                        plot.prior.post = plot_prior_post,
                        plot.parameters = plot_parameters
                        )
        }

        if(!is.null(plot_CI_changes_years) && make_any_aggregates) {

            CIPropChangesSubregions(
                run.name = run_name,
                output.dir = output_folder_path,
                fig.dir = ci_fig_folder_path,
                start.year = plot_CI_changes_years[1],
                end.year = plot_CI_changes_years[length(plot_CI_changes_years)],
                method.to.plot = "any",
                aggregates.to.plot = c("UNPD"),
                all.women = all_women,
                all.womenize.fig.name = FALSE
            )

            CIPropChangesSubregions(
                run.name = run_name,
                output.dir = output_folder_path,
                fig.dir = ci_fig_folder_path,
                start.year = plot_CI_changes_years[1],
                end.year = plot_CI_changes_years[length(plot_CI_changes_years)],
                method.to.plot = "any",
                aggregates.to.plot = c("WB"),
                all.women = all_women,
                all.womenize.fig.name = FALSE
            )

            CIPropChangesSubregions(
                run.name = run_name,
                output.dir = output_folder_path,
                fig.dir = ci_fig_folder_path,
                start.year = plot_CI_changes_years[1],
                end.year = plot_CI_changes_years[length(plot_CI_changes_years)],
                method.to.plot = "modern",
                aggregates.to.plot = c("UNPD"),
                all.women = all_women,
                all.womenize.fig.name = FALSE
            )

            CIPropChangesSubregions(
                run.name = run_name,
                output.dir = output_folder_path,
                fig.dir = ci_fig_folder_path,
                start.year = plot_CI_changes_years[1],
                end.year = plot_CI_changes_years[length(plot_CI_changes_years)],
                method.to.plot = "modern",
                aggregates.to.plot = c("WB"),
                all.women = all_women,
                all.womenize.fig.name = FALSE
            )

            CIPropChangesSubregions(
                run.name = run_name,
                output.dir = output_folder_path,
                fig.dir = ci_fig_folder_path,
                start.year = plot_CI_changes_years[1],
                end.year = plot_CI_changes_years[length(plot_CI_changes_years)],
                method.to.plot = "demand-satisfied-modern",
                aggregates.to.plot = c("UNPD"),
                all.women = all_women,
                all.womenize.fig.name = FALSE
            )

            CIPropChangesSubregions(
                run.name = run_name,
                output.dir = output_folder_path,
                fig.dir = ci_fig_folder_path,
                start.year = plot_CI_changes_years[1],
                end.year = plot_CI_changes_years[length(plot_CI_changes_years)],
                method.to.plot = "demand-satisfied-modern",
                aggregates.to.plot = c("WB"),
                all.women = all_women,
                all.womenize.fig.name = FALSE
            )
        } else {
            warning("'plot_CI_changes_years' is 'NULL': No CI changes plots produced.")
        }

        ## Maps

        if((is.null(plot_maps_shapefile_folder) && !is.null(plot_maps_years)) ||
           (!is.null(plot_maps_shapefile_folder) && is.null(plot_maps_years))) {
            warning("Both 'plot_maps_shapefile_folder' and 'plot_maps_years' must be supplied to produced maps.")

        } else {

            plot_maps <- !is.null(plot_maps_shapefile_folder) && !is.null(plot_maps_years)

            if (plot_maps) {
                if(!(requireNamespace("sp", quietly = TRUE) &&
                     requireNamespace("rgdal", quietly = TRUE) &&
                     requireNamespace("RColorBrewer", quietly = TRUE))) {
                    plot_maps <- FALSE
                    warning("Packages 'RColorBrewer', 'sp' and 'rgdal' are required to produce maps but at least some are not installed. Maps are not produced.")
                }
            }
            if(plot_maps) {
                if(!dir.exists(plot_maps_shapefile_folder))
                    stop("'plot_maps_shapefile_folder' '", plot_maps_shapefile_folder,
                         "' does not exist. Set 'plot_maps_shapefile_folder' to 'NULL' if you do not have shapefiles or do not want maps. NB: This only checks for the existence of the directory, not the validity of the shapefiles. If the shapefiles are invalid ")
                map_fig_folder_path <- file.path(output_folder_path, "fig", "maps")
                if (!dir.exists(map_fig_folder_path)) {
                    dir.create(map_fig_folder_path, recursive = TRUE, showWarnings = FALSE)
                }

                for(mapy in plot_maps_years) {

                    if(!all_women) {
                        MapCPIndicator(UWRA = unmarried,
                                       run.name = run_name,
                                       output.dir = output_folder_path,
                                       categories = seq(from = 0, to = 100, length.out = 6),
                                       shapefile.dir = plot_maps_shapefile_folder,
                                       fig.dir = map_fig_folder_path,
                                       est.year = mapy,
                                       all.women = all_women,
                                       all.womenize.fig.name = FALSE,
                                       verbose = verbose
                                       )

                        MapCPIndicator(indicator = "Modern", UWRA = unmarried,
                                       run.name = run_name,
                                       output.dir = output_folder_path,
                                       categories = c(0, 1, 5, 10, 20, 100),
                                       shapefile.dir = plot_maps_shapefile_folder,
                                       fig.dir = map_fig_folder_path,
                                       est.year = mapy,
                                       all.women = all_women,
                                       all.womenize.fig.name = FALSE,
                                       verbose = verbose
                                       )

                        MapCPIndicator(indicator = "Total Demand", UWRA = unmarried,
                                       run.name = run_name,
                                       output.dir = output_folder_path,
                                       categories = c(0, 2, 5, 20, 40, 100),
                                       shapefile.dir = plot_maps_shapefile_folder,
                                       fig.dir = map_fig_folder_path,
                                       est.year = mapy,
                                       rev.col = TRUE,
                                       all.women = all_women,
                                       all.womenize.fig.name = FALSE,
                                       verbose = verbose
                                       )

                    } else { # ALL WOMEN

                        MapCPIndicator(run.name = run_name,
                                       output.dir = output_folder_path,
                                       categories = seq(from = 0, to = 100, length.out = 6),
                                       shapefile.dir = plot_maps_shapefile_folder,
                                       fig.dir = map_fig_folder_path,
                                       est.year = mapy,
                                       all.women = all_women,
                                       all.womenize.fig.name = FALSE,
                                       verbose = verbose
                                       )

                        MapCPIndicator(indicator = "Modern",
                                       run.name = run_name,
                                       output.dir = output_folder_path,
                                       categories = c(0, 1, 5, 10, 20, 100),
                                       shapefile.dir = plot_maps_shapefile_folder,
                                       fig.dir = map_fig_folder_path,
                                       est.year = mapy,
                                       all.women = all_women,
                                       all.womenize.fig.name = FALSE,
                                       verbose = verbose
                                       )

                        MapCPIndicator(indicator = "Total Demand",
                                       run.name = run_name,
                                       output.dir = output_folder_path,
                                       categories = c(0, 1, 5, 10, 20, 100),
                                       shapefile.dir = plot_maps_shapefile_folder,
                                       fig.dir = map_fig_folder_path,
                                       est.year = mapy,
                                       rev.col = TRUE,
                                       all.women = all_women,
                                       all.womenize.fig.name = FALSE,
                                       verbose = verbose
                                       )
                    }
                }
            }
        }

        ##............................................................................
        ## Create tables

        table_folder_name <- "table"

        ## Unadjusted

        if(adjust_medians) {
            unadjusted_table_folder_path <-
                file.path(output_folder_path, table_folder_name, "orig")
        } else {
            unadjusted_table_folder_path <-
                file.path(output_folder_path, table_folder_name)
        }
        dir.create(unadjusted_table_folder_path, recursive = TRUE, showWarnings = FALSE)

        if(!all_women) {

            GetTablesRes(
                run.name = run_name,
                output.dir = output_folder_path,
                name.res = "Country",
                table.dir = unadjusted_table_folder_path
            )

            if (tabulate_changes) {
                GetTablesChange(
                    run.name = run_name,
                    output.dir = output_folder_path,
                    name.res = "Country",
                    table.dir = unadjusted_table_folder_path,
                    change.in.changes = TRUE
                )
            }

            if (make_any_aggregates) {
                GetTablesRes(
                    run.name = run_name,
                    output.dir = output_folder_path,
                    name.res = "UNPDaggregate",
                    table.dir = unadjusted_table_folder_path
                )

                if (tabulate_changes) {
                    GetTablesChange(
                        run.name = run_name,
                        output.dir = output_folder_path,
                        name.res = "UNPDaggregate",
                        table.dir = unadjusted_table_folder_path,
                        change.in.changes = TRUE
                    )
                }
            }

        } else {

            GetTablesResAllWomen(
                run.name = run_name,
                output.dir = output_folder_path,
                name.res = "Country",
                table.dir = unadjusted_table_folder_path,
                all.womenize.table.name = FALSE
            )

            if (tabulate_changes) {
                GetTablesChangeAllWomen(
                    run.name = run_name,
                    output.dir = output_folder_path,
                    name.res = "Country",
                    table.dir = unadjusted_table_folder_path,
                    all.womenize.table.name = FALSE
                )
            }

            if (make_any_aggregates) {
                GetTablesResAllWomen(
                    run.name = run_name,
                    output.dir = output_folder_path,
                    name.res = "UNPDaggregate",
                    table.dir = unadjusted_table_folder_path,
                    all.womenize.table.name = FALSE
                )

                if (tabulate_changes) {
                    GetTablesChangeAllWomen(
                        run.name = run_name,
                        output.dir = output_folder_path,
                        name.res = "UNPDaggregate",
                        table.dir = unadjusted_table_folder_path,
                        all.womenize.table.name = FALSE
                    )
                }
            }

        }

        ##............................................................................
        ## Adjust medians

        if(adjust_medians) {

            adjust_medians_method <- "mod_tot_unmet"

            ## Adjustments (demographic consistency)

            adjusted_table_folder_path <-
                file.path(output_folder_path, table_folder_name, "adj")
            dir.create(adjusted_table_folder_path, recursive = TRUE, showWarnings = FALSE)

            compare_adjusted_table_folder_path <-
                file.path(output_folder_path, table_folder_name, "cf_adj_orig")
            dir.create(compare_adjusted_table_folder_path, recursive = TRUE, showWarnings = FALSE)

            compare_adjusted_fig_folder_path <-
                file.path(output_folder_path, "fig", "cf_adj_orig")
            dir.create(compare_adjusted_fig_folder_path, recursive = TRUE, showWarnings = FALSE)

            if(identical(adjust_medians_method, "mod_tot_unmet")) {

                if(!all_women) { ## MWRA and UWRA

                    res_country_adj <- AdjustMedians(
                        run.name = run_name,
                        output.dir = output_folder_path,
                        adj.method = adjust_medians_method
                    )
                    save(res_country_adj,
                         file = file.path(output_folder_path,
                                          makeFileName(paste0("res.country.adj-",
                                                              adjust_medians_method,
                                                              ".rda"))))

                    GetTablesRes(
                        run.name = run_name,
                        output.dir = output_folder_path,
                        name.res = "Country",
                        table.dir = adjusted_table_folder_path,
                        res = res_country_adj,
                        adjusted.medians = TRUE,
                        adj.method = adjust_medians_method
                    )

                    CompareAdjMedians(run.name = run_name,
                                      output.dir = output_folder_path,
                                      res.adj = res_country_adj,
                                      tabulate = TRUE,
                                      table.dir = compare_adjusted_table_folder_path,
                                      plot = FALSE,
                                      plot.dir = compare_adjusted_fig_folder_path,
                                      verbose = verbose)

                    if (make_any_aggregates) {

                        ## Aggregates: For 'mod_tot_unmet', aggregates are adjusted independently of countries.

                        res_country_adj_agg <-
                            AdjustMedians(
                                run.name = run_name,
                                name.res = "UNPDaggregate",
                                output.dir = output_folder_path,
                                adj.method = adjust_medians_method
                            )
                        save(res_country_adj_agg,
                             file = file.path(output_folder_path,
                                              makeFileName(paste0("res.UNPDaggregate.adj-",
                                                                  adjust_medians_method,
                                                                  ".rda"))))

                        GetTablesRes(
                            run.name = run_name,
                            output.dir = output_folder_path,
                            name.res = "UNPDaggregate",
                            table.dir = adjusted_table_folder_path,
                            res = res_country_adj_agg,
                            adjusted.medians = TRUE,
                            adj.method = adjust_medians_method
                        )

                        CompareAdjMedians(run.name = run_name,
                                          output.dir = output_folder_path,
                                          name.res = "UNPDaggregate",
                                          res.adj = res_country_adj_agg,
                                          tabulate = TRUE,
                                          table.dir = compare_adjusted_table_folder_path,
                                          plot = FALSE,
                                          plot.dir = compare_adjusted_fig_folder_path,
                                          verbose = verbose)

                    }

                } else { ## ALL WOMEN

                    ## Countries

                    res_country_adj_all_women <-
                        ConstructAdjMediansAllWomen(uwra.adj.med = file.path(output_folder_path, uwra_adj_med_fn),
                                                    mwra.adj.med = file.path(output_folder_path, mwra_adj_med_fn)
                                                    )
                    save(res_country_adj_all_women,
                         file = file.path(output_folder_path,
                                          makeFileName(paste0("res.country.all.women.adj-",
                                                              adjust_medians_method,
                                                              ".rda"))))

                    GetTablesResAllWomen(run.name = run_name, name.res = "Country"
                                        ,table.dir = adjusted_table_folder_path
                                        ,res = res_country_adj_all_women
                                        ,adjusted.medians = TRUE
                                        ,adj.method = adjust_medians_method
                                        ,all.womenize.table.name = FALSE
                                         )

                    CompareAdjMedians(run.name = run_name,
                                      output.dir = output_folder_path,
                                      res.adj = res_country_adj_all_women,
                                      tabulate = TRUE,
                                      plot = FALSE,
                                      all.women = all_women,
                                      all.womenize.table.name = FALSE,
                                      verbose = verbose)
                    ## Aggregates

                    if (make_any_aggregates) {

                        res_country_adj_all_women_agg <-
                            ConstructAdjMediansAllWomen(uwra.adj.med = file.path(output_folder_path, uwra_adj_med_fn_UNPDaggregate),
                                                        mwra.adj.med = file.path(output_folder_path, mwra_adj_med_fn_UNPDaggregate)
                                                        )

                        GetTablesResAllWomen(run.name = run_name, name.res = "UNPDaggregate"
                                            ,table.dir = adjusted_table_folder_path
                                            ,res = res_country_adj_all_women_agg #aggregate
                                            ,adjusted.medians = TRUE
                                            ,adj.method = adjust_medians_method
                                            ,all.womenize.table.name = FALSE
                                             )

                        CompareAdjMedians(run.name = run_name,
                                          output.dir = output_folder_path,
                                          res.adj = res_country_adj_all_women_agg,
                                          name.res = "UNPDaggregate",
                                          tabulate = TRUE,
                                          plot = FALSE,
                                          all.women = all_women,
                                          all.womenize.table.name = FALSE,
                                          verbose = verbose)

                    }
                }
            }
        }

        ##............................................................................
        ## Special aggregates

        ## Extra Aggregates

        if (!is.null(special_aggregates_name) && make_any_aggregates) {

            for (name.agg in special_aggregates_name) {

                if(!all_women) {
                    file.agg.rda <- file.path(output_folder_path, paste0(name.agg, ".rda"))
                                #^ Produced by 'post_process_mcmc()'
                } else {
                    file.agg.rda <- file.path(output_folder_path, paste0(name.agg, ".all.women.rda"))
                                #^ Produced by 'combine_runs()'
                }

                new_agg <- load(file.agg.rda, verbose = verbose)
                res_new <- get(new_agg)

                message("\nMaking aggregates for ", name.agg, " from ", file.agg.rda, ".")

                if(!all_women) {

                    ## Tables
                    GetTablesRes(run.name = run_name, res = res_new, name.res = name.agg
                                ,table.dir = unadjusted_table_folder_path)
                    if (tabulate_changes) {
                        GetTablesChange(run.name = run_name, res = res_new, name.res = name.agg
                                       ,table.dir = unadjusted_table_folder_path
                                       ,change.in.changes = TRUE)
                    }

                    if(adjust_medians) {

                        res_country_adj_agg <- AdjustMedians(
                            run.name = run_name,
                            name.res = name.agg,
                            output.dir = output_folder_path,
                            adj.method = adjust_medians_method
                        )
                        save(res_country_adj_agg,
                             file = file.path(output_folder_path,
                                              makeFileName(paste0(name.agg, "-",
                                                                  adjust_medians_method,
                                                                  ".rda"))))

                        GetTablesRes(run.name = run_name, name.res = name.agg
                                    ,table.dir = adjusted_table_folder_path
                                    ,res = res_country_adj_agg #aggregate
                                    ,adjusted.medians = TRUE
                                    ,adj.method = adjust_medians_method
                                     )
                    }

                } else { ## All Women

                    ## Tables
                    GetTablesResAllWomen(run.name = run_name
                                        ,name.res = name.agg
                                        ,output.dir = output_folder_path
                                        ,table.dir = unadjusted_table_folder_path
                                        ,res = res_new
                                        ,adjusted.medians = FALSE
                                        ,all.womenize.table.name = FALSE)
                    if (tabulate_changes) {
                        GetTablesChangeAllWomen(run.name = run_name
                                               ,name.res = name.agg
                                               ,output.dir = output_folder_path
                                               ,table.dir = unadjusted_table_folder_path
                                               ,res = res_new
                                               ,all.womenize.table.name = FALSE)
                    }

                    if(adjust_medians) {

                        adj_med_fn_spec_agg <-
                            makeFileName(paste0(name.agg, "-", adjust_medians_method, ".rda"))
                        uwra_adj_med_fn_spec_agg <- paste0("uwra_", adj_med_fn_spec_agg)
                        mwra_adj_med_fn_spec_agg <- paste0("mwra_", adj_med_fn_spec_agg)

                        res_country_adj_all_women_agg <-
                            ConstructAdjMediansAllWomen(uwra.adj.med = file.path(output_folder_path,uwra_adj_med_fn_spec_agg),
                                                        mwra.adj.med = file.path(output_folder_path,mwra_adj_med_fn_spec_agg)
                                                        )

                        GetTablesResAllWomen(run.name = run_name
                                            ,name.res = name.agg
                                            ,table.dir = adjusted_table_folder_path
                                            ,res = res_country_adj_all_women_agg
                                            ,adjusted.medians = TRUE
                                            ,adj.method = adjust_medians_method
                                            ,all.womenize.table.name = FALSE
                                             )

                        cat("\n'name.agg' is '", name.agg, "'.\n", sep = "")
                    }
                }
            }
        }

        ##............................................................................
        ## Age Ratios

        if(make_age_ratios) {

            ## Countries

            message("\nMaking country age ratios")

            GetTablesAgeRatios(run.name = run_name,
                               output.dir = output_folder_path,
                               table.dir = unadjusted_table_folder_path,
                               res = NULL,
                               name.res = "Country",
                               fp2020.69.only = FALSE,
                               all.women = all_women,
                               all.womenize.table.name = FALSE,
                               adjusted.medians = FALSE) #Not yet implemented.

            if (tabulate_changes) {
                GetTablesChangeAgeRatios(run.name = run_name,
                                         output.dir = output_folder_path,
                                         table.dir = unadjusted_table_folder_path,
                                         name.res = "Country",
                                         fp2020.69.only = FALSE,
                                         all.women = all_women,
                                         all.womenize.table.name = FALSE,
                                         adjusted.medians = FALSE, #Not yet implemented
                                         adj.method = NULL         #Not yet implemented
                                         )
            }

            ## Aggregates only possible if `combine_runs()` has been run

            ## UNPD Aggregates

            if(file.exists(file.path(output_folder_path, "res.aggregate.age.ratio.rda"))) {

                message("\nMaking aggregate age ratios")

                GetTablesAgeRatios(run.name = run_name,
                                   output.dir = output_folder_path,
                                   table.dir = unadjusted_table_folder_path,
                                   name.res = "UNPDaggregate",
                                   fp2020.69.only = FALSE,
                                   all.women = all_women,
                                   all.womenize.table.name = FALSE,
                                   adjusted.medians = FALSE) #Not yet implemented.
                if (tabulate_changes) {
                    GetTablesChangeAgeRatios(run.name = run_name,
                                             output.dir = output_folder_path,
                                             table.dir = unadjusted_table_folder_path,
                                             name.res = "UNPDaggregate",
                                             all.women = all_women,
                                             all.womenize.table.name = FALSE,
                                             fp2020.69.only = FALSE,
                                             adjusted.medians = FALSE, #Not yet implemented
                                             )
                }

                if(all_women) message("\nYou can now re-run 'make_results' for married and unmarried women to get age ratios for UNPD and WB aggregates.")

            } else message("\n'", file.path(output_folder_path, "res.aggregate.age.ratio.rda"), "' does not exist: age ratios for UNPD aggregates not created.\n**Note: `combine_runs()` must be run before any age ratios for aggregates can be made.")

            ## Special aggregates

            if (!is.null(special_aggregates_name) && make_any_aggregates) {

                for (name.agg in special_aggregates_name) {

                    file.agg.rda <- file.path(output_folder_path, paste0(name.agg, ".age.ratio.rda"))
                                #^ Produced by 'post_process_mcmc()'

                    if(file.exists(file.agg.rda)) {

                        message("\nMaking age ratio of aggregates for ", name.agg, " from ", file.agg.rda, ".")

                        GetTablesAgeRatios(run.name = run_name,
                                           output.dir = output_folder_path,
                                           table.dir =  file.path(output_folder_path, "table", "orig"),
                                           name.res = name.agg,
                                           fp2020.69.only = FALSE,
                                           all.women = all_women,
                                           all.womenize.table.name = FALSE,
                                           adjusted.medians = FALSE) #Not yet implemented.

                        if (tabulate_changes) {
                            GetTablesChangeAgeRatios(run.name = run_name,
                                                     output.dir = output_folder_path,
                                                     table.dir =  file.path(output_folder_path, "table", "orig"),
                                                     name.res = name.agg,
                                                     all.women = all_women,
                                                     all.womenize.table.name = FALSE,
                                                     fp2020.69.only = FALSE,
                                                     adjusted.medians = FALSE, #Not yet implemented
                                                     )
                        }

                        if(all_women) message("\nYou can now re-run 'make_results' for married and unmarried women to get age ratios for special aggregates.")

                    } else message("'", file.path(output_folder_path, paste0(name.agg, ".age.ratio.rda")), "' does not exist: age ratios for special aggregates not created.\n**Note: `combine_runs()` must be run before any age ratios for aggregates can be made.")
                }
            }
        }

        ##----------------------------------------------------------------------------
        ## Log
        ##----------------------------------------------------------------------------

        msg <- paste0("Finished making results for run ", run_name)
        message(msg)
        cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
            msg,
            file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

        ##----------------------------------------------------------------------------
        ## Return
        ##----------------------------------------------------------------------------

        return(invisible(run_name))
    }
}


################################################################################
###
### Wrap up do_mcmc, post_process, and make_results
###
################################################################################


##' Run the global FPEM model for married or unmarried women
##'
##' This generates the MCMC chains using \code{\link{do_global_mcmc}},
##' post-processes them using \code{\link{post_process_mcmc}} and
##' produces plots and tables using \code{\link{make_results}}.
##'
##' See \dQuote{Details} in the help file for \code{\link{do_global_all_women_run}}.
##'
##' @param make_any_results Logical. Should tables and plots be produced? If
##'     \code{FALSE}, the arguments that pertain to specific plots or tables are
##'     ineffective.
##' @inheritParams do_global_mcmc
##' @inheritParams post_process_mcmc
##' @inheritParams make_results
##' @return A name for the run, \code{run_name}, returned invisibly as a
##'     character string.
##' @author Mark Wheldon, Andrew Tait
##'
##' @references
##' Kettunen, J. et al. (2012) Genome-wide association
##' study identifies multiple loci influencing human serum metabolite
##' levels. Nat Genet advance online publication.
##' \url{http://dx.doi.org/10.1038/ng.1073.}
##'
##' @seealso \code{\link{combine_runs}} to create all women
##'     results from married and unmarried women runs;
##'     \code{\link{do_global_all_women_run}} to do married, unmarried,
##'     \emph{and all women runs}, and produce results, all in one call.
##' @examples vignette("FPEMglobal_Intro")
##'
##' @export
do_global_run <- function(## Describe the run
                          run_desc = "",
                          marital_group = c("married", "unmarried"),
                          age_group = "15-49",
                          estimation_iterations = 3,
                          burn_in_iterations = 1,
                          steps_before_progress_report = 4,
                          thinning = 2,
                          chain_nums = 1:3,
                          set_seed_chains = 1,
                          run_in_parallel = isTRUE(length(chain_nums) > 1),
                          input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
                          data_csv_filename = paste0("data_cp_model_all_women_", age_group, ".csv"),
                          region_information_csv_filename = "country_and_area_classification_pre2024.csv",
                          denominator_counts_csv_filename = paste0("number_of_women_", age_group, ".csv"),
                          countries_for_aggregates_csv_filename = "countries_mwra_195_pre2024.csv",
                          countries_in_CI_plots_csv_filename = "countries_mwra_195_pre2024.csv",
                          special_aggregates_name = NULL,
                          output_folder_path = NULL,
                          start_year = 1970.5,
                          end_year = 2030.5,
                          years_change = matrix(c(
                              1990.5, 2000.5,
                              2000.5, 2018.5,
                              2018.5, 2030.5,
                              2012.5, 2018.5,
                              2012.5, 2020.5,
                              2012.5, 2017.5),
                              ncol = 2, byrow = TRUE),
                          years_change2 = matrix(c(2005.5, 2010.5, 2015.5,
                                                   2000.5, 2005.5, 2010.5,
                                                   1995.5, 2000.5, 2005.5,
                                                   1990.5, 1995.5, 2000.5,
                                                   1990.5, 2000.5, 2010.5),
                                                 ncol = 3, byrow = TRUE),
                          make_any_results = TRUE,
                          plot_barchart_years = c(floor(start_year), floor(median(c(start_year, end_year))), floor(end_year)),
                          plot_CI_changes_years = c(floor(start_year), floor(end_year)),
                          make_all_bar_charts = TRUE,
                          plot_maps_shapefile_folder = NULL,
                          plot_maps_years = floor(median(c(start_year, end_year))),
                          data_info_plot_years = c(1990, 2000, 2010),
                          make_any_aggregates = TRUE,
                          adjust_medians = TRUE,
                          age_ratios_age_total_run_name = NULL,
                          age_ratios_age_total_output_folder_path = NULL,
                          age_ratios_age_total_denominator_counts_csv_filename = "number_of_women_15-49.csv",
                          age_ratios_age_total_denominator_counts_folder_path = NULL,
                          run_name_override = NULL,
                          model_diagnostics = TRUE,
                          include_AR = TRUE,
                          verbose = FALSE) {

    ##-----------------------------------------------------------------------------
    ## Set-up

    if (identical(length(chain_nums), 1L))
        message("Only a single chain has been requested; post-processing will *not* be done and results will *not* be produced.")

    marital_group <- match.arg(marital_group)

    if(!is.null(run_name_override)) run_name <- run_name_override
    else run_name <- make_run_name(marital_group, age_group, run_desc)

    if(is.null(output_folder_path)) output_folder_path <- file.path("output", run_name)

    ## Default for 'output_data_folder_path' needs to be defined here because
    ## 'post_process_mcmc' needs it.
    output_data_folder_path <- file.path(output_folder_path, "data")

    ##-----------------------------------------------------------------------------
    ## Check input files

    ## Denominators
    if (!is.null(input_data_folder_path)) {
        verifyDenominators(x = file.path(input_data_folder_path, denominator_counts_csv_filename),
                           in_union = which(c("unmarried", "married") == marital_group) - 1)
    } else {
        verifyDenominators(x = file.path(denominator_counts_csv_filename),
                           in_union = which(c("unmarried", "married") == marital_group) - 1)
    }

    ## Validate denominators. Checks that denominators needed for aggregates are present.
    validate_denominator_counts_file(age_group = age_group,
                                     input_data_folder_path = input_data_folder_path,
                                     denominator_counts_csv_filename = denominator_counts_csv_filename,
                                     marital_group = switch(marital_group,
                                                            "MWRA" = "married",
                                                            "UWRA" = "unmarried",
                                                            "AWRA" = c("married", "unmarried")),
                                     countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename)

    ## If model does not handle missing data must not have missing inputs.
    tmp_model_name <- marital_age_group_param_defaults(marital_group = marital_group,
                                                       age_group = age_group, model_family = "rate",
                                                       model_name = NULL)$write_model_fun
    if (!ModelFunctionInclNoData(tmp_model_name)) {
        if (!is.null(input_data_folder_path)) {
            denom_isos <-
                unique(read.csv(file = file.path(input_data_folder_path, countries_for_aggregates_csv_filename))$ISO.Code)
            num_isos <-
                unique(read.csv(file = file.path(input_data_folder_path, data_csv_filename))$ISO.code)
        } else {
            denom_isos <-
                unique(read.csv(file = countries_for_aggregates_csv_filename)$ISO.Code)
            num_isos <-
                unique(read.csv(file = data_csv_filename)$ISO.code)
        }
        not_in_num <- setdiff(denom_isos, num_isos)
        if (length(not_in_num))
            stop("File 'countries_for_aggregates_csv_filename' ('",
                 countries_for_aggregates_csv_filename,
                 "') contains countries not in the main input file ('",
                 data_csv_filename,
                 "'), but model '", tmp_model_name, "' does not produce estimates for countries with no data.")
    }

    ## Maps

    if((is.null(plot_maps_shapefile_folder) && !is.null(plot_maps_years)) ||
       (!is.null(plot_maps_shapefile_folder) && is.null(plot_maps_years))) {
        message("Both 'plot_maps_shapefile_folder' and 'plot_maps_years' must be supplied to produced maps. Maps will *not* be produced.")
        warning("Maps not produced.")

    } else {

        plot_maps <- !is.null(plot_maps_shapefile_folder) && !is.null(plot_maps_years)

        if (plot_maps) {
            if(!(requireNamespace("sp", quietly = TRUE) &&
                 requireNamespace("rgdal", quietly = TRUE) &&
                 requireNamespace("RColorBrewer", quietly = TRUE))) {
                plot_maps <- FALSE
                message("Packages 'RColorBrewer', 'sp' and 'rgdal' are required to produce maps but at least some are not installed. Maps will *not* be produced.")
                warning("Maps not produced.")
            }
        }
        if(plot_maps) {
            if(!dir.exists(plot_maps_shapefile_folder))
                stop("'plot_maps_shapefile_folder' '", plot_maps_shapefile_folder,
                     "' does not exist. Set 'plot_maps_shapefile_folder' to 'NULL' if you do not have shapefiles or do not want maps. NB: This only checks the existence of the directory, not the validity of the shapefiles.")
            map_fig_folder_path <- file.path(output_folder_path, "fig", "maps")
            if (!dir.exists(map_fig_folder_path)) {
                dir.create(map_fig_folder_path, recursive = TRUE, showWarnings = FALSE)
            }
        }
    }

    ## Age ratios
    if(identical(age_group, "15-49")) {
        make_age_ratios <- FALSE
        age_ratios_age_total_run_name <- NULL
        age_ratios_age_total_output_folder_path <- NULL
        age_ratios_age_total_denominator_counts_folder_path <- NULL
        message("Age ratios not created for age group '15-49'.")
    } else {
        if(!is.null(age_ratios_age_total_run_name) ||
           !is.null(age_ratios_age_total_output_folder_path)) {
            make_age_ratios <- TRUE
        } else {
            make_age_ratios <- FALSE
        }
    }
    if(make_age_ratios) {
        if(is.null(age_ratios_age_total_output_folder_path)) {
            age_ratios_age_total_output_folder_path <-
                file.path("output", age_ratios_age_total_run_name)
        }
        if(is.null(age_ratios_age_total_denominator_counts_folder_path)) {
            age_ratios_age_total_denominator_counts_folder_path <-
                file.path(age_ratios_age_total_output_folder_path, "data")
        }

        if(!dir.exists(age_ratios_age_total_output_folder_path))
            stop("'age_ratios_age_total_output_folder_path' does not exist ('",
                 age_ratios_age_total_output_folder_path,
                 "')")
        if(!length(dir(age_ratios_age_total_output_folder_path)))
            stop("'age_ratios_age_total_output_folder_path' is empty ('",
                 age_ratios_age_total_output_folder_path,
                 "')")
        if(!dir.exists(file.path(age_ratios_age_total_output_folder_path, "countrytrajectories")))
            stop("'Directory 'countrytrajectories' not found in 'age_ratios_age_total_output_folder_path' ('",
                 file.path(age_ratios_age_total_output_folder_path, "countrytrajectories"),
                 "'")
        if(!file.exists(file.path(age_ratios_age_total_denominator_counts_folder_path,
                                  age_ratios_age_total_denominator_counts_csv_filename)))
            stop("'age_ratios_age_total_denominator_counts_csv_filename' not found in 'age_ratios_age_total_denominator_counts_folder_path' ('",
                 file.path(age_ratios_age_total_denominator_counts_folder_path,
                           age_ratios_age_total_denominator_counts_csv_filename),
                 "')")
    }

    ##-----------------------------------------------------------------------------
    ## MCMC Chains

    run_name <-
        do_global_mcmc(run_desc = run_desc,
                       run_name_override = run_name, #use run_name created above.
                       marital_group = marital_group,
                       age_group = age_group,
                       estimation_iterations = estimation_iterations,
                       burn_in_iterations = burn_in_iterations,
                       steps_before_progress_report = steps_before_progress_report,
                       thinning = thinning,
                       chain_nums = chain_nums,
                       set_seed_chains = set_seed_chains,
                       run_in_parallel = run_in_parallel,
                       input_data_folder_path = input_data_folder_path,
                       data_csv_filename = data_csv_filename,
                       region_information_csv_filename = region_information_csv_filename,
                       output_folder_path = output_folder_path,
                       include_AR = include_AR,
                       verbose = verbose)

    ##-----------------------------------------------------------------------------
    ## STOP if only one chain

    if (identical(length(chain_nums), 1L)) {
        warning("Post-processing and results *not* available with a single chain.")
        return(invisible(run_name))
    }

    ##
    ##-----------------------------------------------------------------------------

    ## More than one chain >> continue >>

    ##-----------------------------------------------------------------------------
    ## Post-Process

    ## Meta Info
    load(file.path(output_folder_path, "mcmc.meta.rda"), verbose = verbose)

    post_process_mcmc(run_name = run_name,
                      input_data_folder_path = output_data_folder_path,
                      denominator_counts_csv_filename = denominator_counts_csv_filename,
                      countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
                      start_year = start_year,
                      end_year = end_year,
                      years_change = years_change,
                      model_diagnostics = model_diagnostics,
                      make_any_aggregates = make_any_aggregates,
                      special_aggregates_name = special_aggregates_name,
                      age_ratios_age_total_run_name = age_ratios_age_total_run_name,
                      age_ratios_age_total_output_folder_path = age_ratios_age_total_output_folder_path,
                      age_ratios_age_total_denominator_counts_csv_filename = age_ratios_age_total_denominator_counts_csv_filename,
                      age_ratios_age_total_denominator_counts_folder_path = age_ratios_age_total_denominator_counts_folder_path,
                      verbose = verbose)

    ##-----------------------------------------------------------------------------
    ## Plots, Tables

    if(make_any_results) {

        make_results(run_name = run_name,
                     input_data_folder_path = output_data_folder_path,
                     countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
                     plot_CI_changes_years = plot_CI_changes_years,
                     plot_barchart_years = plot_barchart_years,
                     make_all_bar_charts = make_all_bar_charts,
                     plot_maps_shapefile_folder = plot_maps_shapefile_folder,
                     plot_maps_years = plot_maps_years,
                     data_info_plot_years = data_info_plot_years,
                     adjust_medians = adjust_medians,
                     make_any_aggregates = make_any_aggregates,
                     special_aggregates_name = special_aggregates_name,
                     make_age_ratios = make_age_ratios,
                     verbose = verbose)

    }

    ##-----------------------------------------------------------------------------
    ## Finish

    ## if(!interactive()) copy_Rout_files(run_name = run_name)

    return(invisible(run_name))
}


################################################################################
###
### Combine runs to make all women outputs
###
################################################################################

##' Make all women results from married and unmarried women FPEM runs
##'
##' Combines MCMC outputs from married and unmarried women FPEM runs
##' to make MCMC outputs for all women. The MCMC outputs must have
##' been produced by \code{\link{do_global_mcmc}} or
##' \code{\link{do_global_run}} (the former is sufficient).
##'
##' Married \emph{and} unmarried women runs must have been produced
##' previously by, e.g,. \code{\link{do_global_run}}. They are
##' identified by their output directory filepaths
##' (\code{married_women_run_output_folder_path} and
##' \code{unmarried_women_run_output_folder_path}). Files from the
##' married and unmarried runs that are needed to make all women
##' results are copied to \code{output_folder_path} (and
##' sub-directories) and noted in files called \file{log.txt} in the
##' respective directories. The file
##' \file{\code{output_folder_path}/log.txt} provides a record of
##' which married and unmarried women runs were combined.
##' @param married_women_run_name Run name of the married women run to
##'     be combined. Ignored if
##'     \code{married_women_run_output_folder_path} is
##'     \code{NULL}. Otherwise, the output folder will be assumed to
##'     be \code{file.path("output", married_women_run_name)}.
##' @param married_women_run_output_folder_path Path to the folder
##'     containing results for the married women run to be combined.
##' @param unmarried_women_run_output_folder_path Path to the folder
##'     containing results for the unmarried women run to be combined.
##' @param unmarried_women_run_name Same as
##'     \code{married_women_run_name} but for the unmarried women run.
##' @param unmarried_women_run_data_folder_path Path to the folder
##'     containing results for the unmarried women run to be
##'     combined. (Only used if \code{special_aggregates_name} is
##'     non-\code{NULL}).
##' @param adjust_medians
##' @param countries_in_CI_plots_csv_filename Name of \file{.csv} file
##'     that lists the countries to be included in the main
##'     country-level indicator plots. See \code{\link{make_results}}
##'     for further details. \code{combine_runs} will copy this file
##'     to \file{\code{output_folder_path}/data} so it is available
##'     for \code{make_results}.
##' @param age_ratios_age_total_unmarried_run_name Run name of the
##'     unmarried 15--49 run to use as the denominator for age ratios.
##' @param age_ratios_age_total_married_run_name Run name of the
##'     married 15--49 run to use as the denominator for age ratios.
##' @param age_ratios_age_total_all_women_run_name Run name of the all
##'     women 15--49 run to use as the denominator for age ratios.
##' @param age_ratios_age_total_unmarried_output_folder_path File path
##'     to output directory of the unmarried 15--49 run to use to make
##'     age ratios.
##' @param age_ratios_age_total_married_output_folder_path File path
##'     to output directory of the married 15--49 run to use to make
##'     age ratios.
##' @param age_ratios_age_total_all_women_output_folder_path File path
##'     to output directory of the all women 15--49 run to use to make
##'     age ratios.
##' @inheritParams do_global_run
##' @inheritParams post_process_mcmc
##' @return A name for the run returned invisibly as a character
##'     string. Results are saved to \file{\code{output_folder_path}}.
##' @author Mark Wheldon, Andrew Tait.
##'
##' @seealso \code{\link{do_global_all_women_run}} to do married, unmarried,
##'     \emph{and all women runs}, and produce results, all in one
##'     call.
##' @examples
##' vignette("FPEMglobal_Intro").
##'
##' @export
combine_runs <- function(## Describe the run
                         run_desc = "",
                         output_folder_path = NULL,
                         married_women_run_name = NULL,
                         married_women_run_output_folder_path = NULL,
                         unmarried_women_run_name = NULL,
                         unmarried_women_run_output_folder_path = NULL,
                         unmarried_women_run_data_folder_path = file.path(unmarried_women_run_output_folder_path, "data"),
                         region_information_csv_filename = NULL,
                         special_aggregates_name = NULL,
                         denominator_counts_csv_filename = NULL,
                         countries_for_aggregates_csv_filename = NULL,
                         countries_in_CI_plots_csv_filename = NULL,
                         start_year = 1970.5,
                         end_year = 2030.5,
                         years_change = matrix(c(
                             1990.5, 2000.5,
                             2000.5, 2019.5,
                             2019.5, 2030.5,
                             2012.5, 2019.5,
                             2012.5, 2019.5,
                             2012.5, 2020.5),
                             ncol = 2, byrow = TRUE),
                         years_change2 = matrix(
                             c(2005.5, 2010.5, 2015.5,
                               2000.5, 2005.5, 2010.5,
                               1995.5, 2000.5, 2005.5,
                               1990.5, 1995.5, 2000.5,
                               1990.5, 2000.5, 2010.5,
                               2000.5, 2010.5, 2019.5), ncol = 3, byrow = TRUE),
                         make_any_aggregates = TRUE,
                         adjust_medians = TRUE,
                         age_ratios_age_total_unmarried_run_name = NULL,
                         age_ratios_age_total_married_run_name = NULL,
                         age_ratios_age_total_all_women_run_name = NULL,
                         age_ratios_age_total_unmarried_output_folder_path = NULL,
                         age_ratios_age_total_married_output_folder_path = NULL,
                         age_ratios_age_total_all_women_output_folder_path = NULL,
                         age_ratios_age_total_denominator_counts_csv_filename = denominator_counts_csv_filename,
                         age_ratios_age_total_denominator_counts_folder_path = NULL,
                         run_name_override = NULL,
                         verbose = FALSE) {

    ##----------------------------------------------------------------------------
    ## Meta information
    ##----------------------------------------------------------------------------

    ## Married and Unmarried
    if(is.null(married_women_run_output_folder_path)) {
        if(!is.null(married_women_run_name)) {
            married_women_run_output_folder_path <- file.path("output", married_women_run_name)
        } else {
            stop("'married_women_run_output_folder_path' or 'married_women_run_name' must be specified.")
        }
    } else {
        if (is.null(married_women_run_name))
            married_women_run_name <- get_run_name_from_args(get(load(file.path(married_women_run_output_folder_path,
                                                                                "post_process_args.RData"))))
    }

    if(is.null(unmarried_women_run_output_folder_path)) {
        if(!is.null(unmarried_women_run_name)) {
            unmarried_women_run_output_folder_path <- file.path("output", unmarried_women_run_name)
        } else {
            stop("'unmarried_women_run_output_folder_path' or 'unmarried_women_run_name' must be specified.")
        }
    } else {
        if (is.null(unmarried_women_run_name))
            unmarried_women_run_name <- get_run_name_from_args(get(load(file.path(unmarried_women_run_output_folder_path,
                                                                                  "post_process_args.RData"))))
    }

    load(file.path(unmarried_women_run_output_folder_path, "mcmc.meta.rda"), verbose = verbose)

    ## Age group
    age_group <- mcmc.meta$general$age.group

    ##--------------------------------------------------------------------------
    ## run_name and output folder
    ##--------------------------------------------------------------------------

    ## Four ways:
    ##
    ## 1. Leave 'run_name' and 'output_folder_path' 'NULL'. Both will
    ## be set by default. 'run_name' will be created with the
    ## datetime, 'output_folder_path' will be
    ## '"output/'run_name'"'. If the output already exists, this will
    ## throw an error if the 'run_name's do not match.
    ##
    ## 2. Specify only 'run_name_override'. 'output_folder_path' will be
    ## '"output/'run_name_override'"'.
    ##
    ## 3. Specify only 'output_folder_path'. If you're adding to an
    ## existing run, 'run_name' will be taken from the
    ## 'combine_runs_args.RData' file. If not, it will be set
    ## automatically (see option 1).
    ##
    ## 4. Specify both 'run_name_override' and 'output_folder_path'. If you are
    ## adding to an existing run this will throw an error.

    if (is.null(run_name_override) && is.null(output_folder_path)) {
        run_name <- make_run_name("all_women", age_group, run_desc)
        output_folder_path <- file.path("output", run_name)
        if (dir.exists(output_folder_path))
            check_run_name_conflicts(run_name, output_folder_path)
        else dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)

    } else if (!is.null(run_name_override) && is.null(output_folder_path)) {
        run_name <- run_name_override
        output_folder_path <- file.path("output", run_name)
        if (dir.exists(output_folder_path))
            check_run_name_conflicts(run_name, output_folder_path)
        else dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)

    } else if (is.null(run_name_override) && !is.null(output_folder_path)) {
        combine_runs_filepath <-
            file.path(output_folder_path, "combine_runs_args.RData")
        if (file.exists(combine_runs_filepath))
            run_name <- get_run_name_from_args(get(load(combine_runs_filepath)))
        else run_name <- make_run_name("all_women", age_group, run_desc)
        if (dir.exists(output_folder_path))
            check_run_name_conflicts(run_name, output_folder_path)
        else dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)

    } else if (!is.null(run_name_override) && !is.null(output_folder_path)) {
        run_name <- run_name_override
        combine_runs_filepath <-
            file.path(output_folder_path, "combine_runs_args.RData")
        if (dir.exists(output_folder_path))
            check_run_name_conflicts(run_name, output_folder_path)
        else dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)
    }

    message("This run has 'run_name': ", run_name, ".")

    ## Output Data Folder
    data_folder_path <- file.path(output_folder_path, "data")
    dir.create(data_folder_path, showWarnings = FALSE)

    ## Make filepaths that need 'age_group'
    if(is.null(denominator_counts_csv_filename)) {
        denominator_counts_csv_filename <-
            paste0("number_of_women_", mcmc.meta$general$age.group, ".csv")
    }

    ##----------------------------------------------------------------------------
    ## Age ratios
    ##----------------------------------------------------------------------------

    ## Age ratios
    if((!is.null(age_ratios_age_total_married_run_name) ||
        !is.null(age_ratios_age_total_married_output_folder_path)) &&
       (!is.null(age_ratios_age_total_unmarried_run_name) ||
        !is.null(age_ratios_age_total_unmarried_output_folder_path)) &&
       (!is.null(age_ratios_age_total_all_women_run_name) ||
        !is.null(age_ratios_age_total_all_women_output_folder_path))
       ) {
        if(identical(mcmc.meta$general$age.group, "15-49")) {
            make_age_ratios <- FALSE
            age_ratios_age_total_married_run_name <- NULL
            age_ratios_age_total_married_output_folder_path <- NULL
            age_ratios_age_total_unmarried_run_name <- NULL
            age_ratios_age_total_unmarried_output_folder_path <- NULL
            age_ratios_age_total_all_women_run_name <- NULL
            age_ratios_age_total_all_women_output_folder_path <- NULL
            message("Age ratios not created for age group '15-49'.")
        } else {
            make_age_ratios <- TRUE
        }
    } else {
        make_age_ratios <- FALSE
    }

    if(make_age_ratios) {

        if(is.null(age_ratios_age_total_married_output_folder_path)) {
            age_ratios_age_total_married_output_folder_path <-
                file.path("output", age_ratios_age_total_married_run_name)
        }

        if(is.null(age_ratios_age_total_unmarried_output_folder_path)) {
            age_ratios_age_total_unmarried_output_folder_path <-
                file.path("output", age_ratios_age_total_unmarried_run_name)
        }

        if(is.null(age_ratios_age_total_all_women_output_folder_path)) {
            age_ratios_age_total_all_women_output_folder_path <-
                file.path("output", age_ratios_age_total_all_women_run_name)
        }

        if(is.null(age_ratios_age_total_denominator_counts_folder_path)) {
            age_ratios_age_total_denominator_counts_folder_path <-
                file.path(age_ratios_age_total_married_output_folder_path, "data")
        }

        if(!dir.exists(age_ratios_age_total_married_output_folder_path))
            stop("'age_ratios_age_total_married_output_folder_path' does not exist ('",
                 age_ratios_age_total_married_output_folder_path,
                 "')")
        if(!length(dir(age_ratios_age_total_married_output_folder_path)))
            stop("'age_ratios_age_total_married_output_folder_path' is empty ('",
                 age_ratios_age_total_married_output_folder_path,
                 "')")
        if(!dir.exists(file.path(age_ratios_age_total_married_output_folder_path, "countrytrajectories")))
            stop("'Directory 'countrytrajectories' not found in 'age_ratios_age_total_married_output_folder_path' ('",
                 file.path(age_ratios_age_total_married_output_folder_path, "countrytrajectories"),
                 "'")

        if(!dir.exists(age_ratios_age_total_unmarried_output_folder_path))
            stop("'age_ratios_age_total_unmarried_output_folder_path' does not exist ('",
                 age_ratios_age_total_unmarried_output_folder_path,
                 "')")
        if(!length(dir(age_ratios_age_total_unmarried_output_folder_path)))
            stop("'age_ratios_age_total_unmarried_output_folder_path' is empty ('",
                 age_ratios_age_total_unmarried_output_folder_path,
                 "')")
        if(!dir.exists(file.path(age_ratios_age_total_unmarried_output_folder_path, "countrytrajectories")))
            stop("'Directory 'countrytrajectories' not found in 'age_ratios_age_total_unmarried_output_folder_path' ('",
                 file.path(age_ratios_age_total_unmarried_output_folder_path, "countrytrajectories"),
                 "'")

        if(!dir.exists(age_ratios_age_total_all_women_output_folder_path))
            stop("'age_ratios_age_total_all_women_output_folder_path' does not exist ('",
                 age_ratios_age_total_all_women_output_folder_path,
                 "')")
        if(!length(dir(age_ratios_age_total_all_women_output_folder_path)))
            stop("'age_ratios_age_total_all_women_output_folder_path' is empty ('",
                 age_ratios_age_total_all_women_output_folder_path,
                 "')")
        if(!dir.exists(file.path(age_ratios_age_total_all_women_output_folder_path, "countrytrajectories")))
            stop("'Directory 'countrytrajectories' not found in 'age_ratios_age_total_all_women_output_folder_path' ('",
                 file.path(age_ratios_age_total_all_women_output_folder_path, "countrytrajectories"),
                 "'")
        if(make_any_aggregates) {
            if(!dir.exists(file.path(age_ratios_age_total_all_women_output_folder_path, "aggregatetrajectories")))
                stop("'Directory 'aggregatetrajectories' not found in 'age_ratios_age_total_all_women_output_folder_path' ('",
                     file.path(age_ratios_age_total_all_women_output_folder_path, "aggregatetrajectories"),
                     "'")
        }
    }

    ##---------------------------------------------------------------------
    ## Save the values of function arguments for make_results()
    ##----------------------------------------------------------------------------

    combine_runs_args <- c(mget(names(formals(combine_runs))),
                           list(run_name = run_name))
    save(combine_runs_args, file = file.path(output_folder_path, "combine_runs_args.RData"))

    ##----------------------------------------------------------------------------
    ## LOG
    ##----------------------------------------------------------------------------

    msg <- paste0("Combining runs in ", married_women_run_output_folder_path, " and ", unmarried_women_run_output_folder_path)
    message(msg)
    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##----------------------------------------------------------------------------
    ## Get Filenames
    ##----------------------------------------------------------------------------

    ## Global MCMC Args

    load(file.path(unmarried_women_run_output_folder_path, "global_mcmc_args.RData"))
    global_mcmc_args_uwra <- global_mcmc_args

    if (is.null(region_information_csv_filename)) {
        region_information_csv_filename <-
            gsub(paste0("^", global_mcmc_args_uwra$input_data_folder_path, "/"),
                 "",
                 global_mcmc_args_uwra$region_information_csv_filename)
    }

    ## Post-process Args

    load(file.path(unmarried_women_run_output_folder_path, "post_process_args.RData"))
    post_process_args_uwra <- post_process_args

    if (is.null(countries_for_aggregates_csv_filename)) {
        countries_for_aggregates_csv_filename <-
            gsub(paste0("^", post_process_args_uwra$input_data_folder_path, "/"),
                 "",
                 post_process_args_uwra$countries_for_aggregates_csv_filename)
    }

    ## Make Results

    make_res_args_file_path <-
        file.path(unmarried_women_run_output_folder_path, "make_results_args.RData")

    if (file.exists(make_res_args_file_path)) {
        load(make_res_args_file_path)
        make_results_args_uwra <- make_results_args
        if (is.null(countries_in_CI_plots_csv_filename)) {
            countries_in_CI_plots_csv_filename <- make_results_args_uwra$countries_in_CI_plots_csv_filename
        }
    } else {
        if (is.null(countries_in_CI_plots_csv_filename)) {
            countries_in_CI_plots_csv_filename <- countries_for_aggregates_csv_filename
        }
    }

    ##----------------------------------------------------------------------------
    ## Copy files needed by plotting and tabulation functions.
    ##----------------------------------------------------------------------------

    ## Do it here because the output directory is created by 'ConstructOutputAllWomen()'.

    copy_uwra_mwra_files("mcmc.meta.rda", output_folder_path, unmarried_women_run_output_folder_path)
    copy_uwra_mwra_files("par.ciq.rda", output_folder_path, unmarried_women_run_output_folder_path)

    if(!is.null(denominator_counts_csv_filename)) {
        copy_uwra_mwra_files(denominator_counts_csv_filename,
                             data_folder_path,
                             file.path(unmarried_women_run_output_folder_path, "data"))
    }
    if(!is.null(region_information_csv_filename)) {
        copy_uwra_mwra_files(region_information_csv_filename,
                             data_folder_path,
                             file.path(unmarried_women_run_output_folder_path, "data"))
    }
    if(!is.null(countries_for_aggregates_csv_filename)) {
        copy_uwra_mwra_files(countries_for_aggregates_csv_filename,
                             data_folder_path,
                             file.path(unmarried_women_run_output_folder_path, "data"))
    }
    if(!is.null(countries_in_CI_plots_csv_filename)) {
        copy_uwra_mwra_files(countries_in_CI_plots_csv_filename,
                             data_folder_path,
                             file.path(unmarried_women_run_output_folder_path, "data"))
    }

    ## Add a label in 'mcmc.meta.rda' to mark this as an all women copy
    load(file.path(output_folder_path, "mcmc.meta.rda"), verbose = verbose)
                                # The copy of 'mcmc.meta.rda', not the
                                # original from the unmarried women
                                # run.
    mcmc.meta$general$all.women.run.copy <- TRUE
    save(mcmc.meta, file = file.path(output_folder_path, "mcmc.meta.rda"))

    ##--------------------------------------------------------------------------
    ## Adjust medians
    ##--------------------------------------------------------------------------

    ## Check that unmarried, married results for adj medians are
    ## available. Country, UNPD aggregates and special aggregates.

    if(adjust_medians) {

        adjust_medians_method <- "mod_tot_unmet"

        sapply(c(married_women_run_output_folder_path,
                 unmarried_women_run_output_folder_path),
               function(z) {
            fn <- file.path(z, makeFileName(paste0("res.country.adj-",
                                                   adjust_medians_method, ".rda")))
            if(!file.exists(fn)) {
                stop("'", fn, "' does not exist. Did you run 'make_results' with 'adjust_medians = TRUE'? If you don't want adjusted medians, set 'adjust_medians' to 'FALSE'.")
            } else {
                return(invisible())
            }
            if (make_any_aggregates) {
                fn2 <- file.path(z, makeFileName(paste0("res.UNPDaggregate.adj-",
                                                        adjust_medians_method, ".rda")))
                if(!file.exists(fn2)) {
                    stop("'", fn2, "' does not exist. Did you run 'make_results' with 'adjust_medians = TRUE'? If you don't want adjusted medians, set 'adjust_medians' to 'FALSE'.")
                } else {
                    return(invisible())
                }
            }
        })

        ## Married
        copy_uwra_mwra_files(makeFileName(paste0("res.country.adj-", adjust_medians_method, ".rda")),
                             output_folder_path,
                             married_women_run_output_folder_path,
                             new_filename = makeFileName(paste0("mwra_", "res.country.adj-",
                                                                adjust_medians_method, ".rda"))
                             )
        if (make_any_aggregates) {
            copy_uwra_mwra_files(makeFileName(paste0("res.UNPDaggregate.adj-", adjust_medians_method, ".rda")),
                                 output_folder_path,
                                 married_women_run_output_folder_path,
                                 new_filename = makeFileName(paste0("mwra_", "res.UNPDaggregate.adj-",
                                                                    adjust_medians_method, ".rda"))
                                 )
        }

        ## Unmarried
        copy_uwra_mwra_files(makeFileName(paste0("res.country.adj-", adjust_medians_method, ".rda")),
                             output_folder_path,
                             unmarried_women_run_output_folder_path,
                             new_filename = makeFileName(paste0("uwra_", "res.country.adj-",
                                                                adjust_medians_method, ".rda"))
                             )
        if (make_any_aggregates) {
            copy_uwra_mwra_files(makeFileName(paste0("res.UNPDaggregate.adj-", adjust_medians_method, ".rda")),
                                 output_folder_path,
                                 unmarried_women_run_output_folder_path,
                                 new_filename = makeFileName(paste0("uwra_", "res.UNPDaggregate.adj-",
                                                                    adjust_medians_method, ".rda"))
                                 )
        }

        if(!is.null(special_aggregates_name) && make_any_aggregates) {
            ## NOTE: Just copy the uwra and mwra special aggregate
            ## adusted median files. The adjusted medians themselves
            ## are made by make_results().
            for (name.agg in special_aggregates_name) {
                file_agg <-
                    makeFileName(paste0(name.agg, "-", adjust_medians_method, ".rda"))
                copy_uwra_mwra_files(file_agg,
                                     output_folder_path,
                                     married_women_run_output_folder_path,
                                     new_filename = makeFileName(paste0("mwra_", file_agg)))
                copy_uwra_mwra_files(file_agg,
                                     output_folder_path,
                                     unmarried_women_run_output_folder_path,
                                     new_filename = makeFileName(paste0("uwra_", file_agg)))
            }
        }
    }

    ##----------------------------------------------------------------------------
    ## Validate denominators. Checks that denominators needed for aggregates are present.
    ##----------------------------------------------------------------------------

    validate_denominator_counts_file(age_group = age_group,
                                     input_data_folder_path = data_folder_path,
                                     denominator_counts_csv_filename = "res.country.rda",
                                     output_folder_path = married_women_run_output_folder_path,
                                     marital_group = "married",
                                     countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename)

    validate_denominator_counts_file(age_group = age_group,
                                     input_data_folder_path = data_folder_path,
                                     denominator_counts_csv_filename = "res.country.rda",
                                     output_folder_path = unmarried_women_run_output_folder_path,
                                     marital_group = "unmarried",
                                     countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename)

    ##--------------------------------------------------------------------------
    ## Construct output for all women
    ##--------------------------------------------------------------------------

    for(fn in c("res.country.all.women.rda", "res.aggregate.all.women.rda")) {
        if(file.exists(file.path(output_folder_path, fn))) {
            message("\n'", file.path(output_folder_path, fn), "' already exists; it will *NOT* be re-created.")
        }
    }

    ConstructOutputAllWomen(run.name = run_name,
                            uwra.output.dir = unmarried_women_run_output_folder_path,
                            mwra.output.dir = married_women_run_output_folder_path,
                            awra.output.dir = output_folder_path,
                            est.years = NULL,
                            WRA.csv = file.path(data_folder_path,
                                                denominator_counts_csv_filename), #denominator counts for WRA
                            years.change = years_change, ##<< Matrix with 2 columns, with column 1
                            years.change2 = years_change2, ##<< Matrix with 3 columns, with column 1
                            make.any.aggregates = make_any_aggregates,
                            countries.to.include.in.aggregates.csv = file.path(data_folder_path,
                                                                               countries_for_aggregates_csv_filename),
                            verbose = verbose,
                            output_exists_warnings = FALSE
                            )

    ##--------------------------------------------------------------------------
    ## Special aggregates
    ##--------------------------------------------------------------------------

    if (!is.null(special_aggregates_name) && make_any_aggregates) {

        for (name.agg in special_aggregates_name) {

            file_agg <- paste0(name.agg, ".csv")

            copy_uwra_mwra_files(file_agg,
                                 data_folder_path,
                                 unmarried_women_run_data_folder_path)

            message("\nMaking aggregates for ", name.agg, " from ", file_agg, ".")

            res.new <-
                GetAggregatesAllWomen(run.name = run_name,
                                      uwra.output.dir = unmarried_women_run_output_folder_path,
                                      mwra.output.dir = married_women_run_output_folder_path,
                                      awra.output.dir = output_folder_path,
                                      WRA.csv = file.path(data_folder_path,
                                                          denominator_counts_csv_filename),
                                      file.aggregates = file.path(data_folder_path, file_agg),
                                      years.change = years_change,
                                      years.change2 = years_change2,
                                      countries.to.include.in.aggregates.csv = file.path(data_folder_path,
                                                                                         countries_for_aggregates_csv_filename),
                                      verbose = verbose
                                      )

            res.fname <- file.path(output_folder_path, paste0(name.agg, ".all.women.rda"))
            save(res.new, file = res.fname)

        }
    }

    ##--------------------------------------------------------------------------
    ## Age ratios (incl. for aggregates)
    ##--------------------------------------------------------------------------

    if(make_age_ratios) {

        message("\nConstructing country age ratios for all women")
        if(file.exists(file.path(output_folder_path, "res.country.all.women.age.ratio.rda"))) {
            message("\n'", file.path(output_folder_path, "res.country.all.women.age.ratio.rda"),
                    "' already exists; it will *NOT* be re-created.")
        }
        ConstructAgeRatiosAllWomen(age.subset.output.dir = output_folder_path,
                                   age.total.output.dir = age_ratios_age_total_all_women_output_folder_path,
                                   age.ratio.output.dir = output_folder_path,
                                   est.years = NULL,
                                   run.name = run_name,
                                   years.change = years_change,
                                   years.change2 = years_change2,
                                   output_exists_warnings = FALSE
                                   )

        if (make_any_aggregates) {

            GetAggregatesAgeRatios(age.subset.uwra.output.dir = unmarried_women_run_output_folder_path,
                                   age.subset.mwra.output.dir = married_women_run_output_folder_path,
                                   age.subset.awra.output.dir = output_folder_path,
                                   age.total.uwra.output.dir = age_ratios_age_total_unmarried_output_folder_path,
                                   age.total.mwra.output.dir = age_ratios_age_total_married_output_folder_path,
                                   age.total.awra.output.dir = age_ratios_age_total_all_women_output_folder_path,
                                   run.name = run_name,
                                   age.subset.WRA.csv = file.path(data_folder_path,
                                                                  denominator_counts_csv_filename),
                                   age.total.WRA.csv = file.path(age_ratios_age_total_denominator_counts_folder_path,
                                                                 age_ratios_age_total_denominator_counts_csv_filename),
                                   est.years = NULL,
                                   years.change = years_change,
                                   years.change2 = years_change2,
                                   verbose = verbose,
                                   output_exists_messages = FALSE
                                   )

            if (!is.null(special_aggregates_name) && make_any_aggregates) {

                for (name.agg in special_aggregates_name) {

                    file_agg <- paste0(name.agg, ".csv")

                    GetAggregatesAgeRatios(age.subset.uwra.output.dir = unmarried_women_run_output_folder_path,
                                           age.subset.mwra.output.dir = married_women_run_output_folder_path,
                                           age.subset.awra.output.dir = output_folder_path,
                                           age.total.uwra.output.dir = age_ratios_age_total_unmarried_output_folder_path,
                                           age.total.mwra.output.dir = age_ratios_age_total_married_output_folder_path,
                                           age.total.awra.output.dir = age_ratios_age_total_all_women_output_folder_path,
                                           run.name = run_name,
                                           file.aggregates = file.path(data_folder_path, file_agg),
                                           age.subset.WRA.csv = file.path(data_folder_path,
                                                                          denominator_counts_csv_filename),
                                           age.total.WRA.csv = file.path(age_ratios_age_total_denominator_counts_folder_path,
                                                                         age_ratios_age_total_denominator_counts_csv_filename),
                                           est.years = NULL,
                                           years.change = years_change,
                                           years.change2 = years_change2,
                                           verbose = verbose,
                                           output_exists_messages = FALSE
                                           )
                }

            }
        }
    }

    ## if(!interactive()) copy_Rout_files(run_name = run_name)

    ##----------------------------------------------------------------------------
    ## LOG
    ##----------------------------------------------------------------------------

    msg <- paste0("Finished combining runs ", married_women_run_output_folder_path, " and ", unmarried_women_run_output_folder_path)
    message(msg)
    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##----------------------------------------------------------------------------
    ## Return
    ##----------------------------------------------------------------------------

    return(invisible(run_name))
}


################################################################################
###
### Big wrapper to do married, unmarried, post-process, and combine
###
################################################################################


##' Do Complete Run of FPEM
##'
##' Do married and unmarried runs of the global model, and produce
##' results. Then combine to produce all women results.
##'
##' A \dQuote{run name} is assigned to each run of the model to identify it. The
##' run name is used to name the directory in which outputs are stored and the
##' filenames of certain results such as tables and plots. By default, it is
##' auto-generated by concatenating the date-time
##' (via \code{format(Sys.time(), "\%y\%m\%d_\%H\%M\%S")}),
##' marital group, age group and (optionally)
##' \code{run_desc}. If \code{run_desc} does not afford enough control,
##' \code{run_name_override} can be used to completely override
##' auto-generation. The function returns the run name as a character string.
##'
##' See \code{system.file("extdata", "data_cp_model_all_women_15-49.csv")} for
##' how the prevalence data input should be formatted. Assume all columns are
##' required.
##'
##' See \code{system.file("extdata", "country_and_area_classification_pre2024.csv")} for
##' how the country classification file should be formatted. Assume all columns
##' are required.
##'
##' Typical values of the \acronym{MCMC} control parameters for a
##' \dQuote{full} model run are: \describe{
##' \item{\code{estimation_iterations}}{\code{ceiling(5e5 / \var{nchains})}, where \var{nchains} is the number of chains (i.e., \code{length(chain_nums)}).}
##' \item{\code{burn_in_iterations}}{2e4}
##' \item{\code{thinning}}{30}
##' }
##'
##' @param denominator_counts_csv_filename File path. Filepath to \file{.csv}
##'     file with denominator counts (married and unmarried) for this
##'     \code{age_group}. If \code{NULL}, defaults to \code{paste0("women_",
##'     \code{age_group}, ".csv")}.
##' @param age_ratios_age_total_run_name_prefix Run name prefix for married, unmarried, and all women runs. Results will be searched for in \file{output/\code{age_ratios_age_total_run_name_prefix}_\var{[marital_group]}_15-49}.
##' @inheritParams do_global_mcmc
##' @inheritParams post_process_mcmc
##' @inheritParams make_results
##' @inheritParams combine_runs
##' @return A \emph{list} of run names for married, unmarried, and all women
##'     runs, returned invisibly. Results are saved to
##'     \file{\code{output_folder_path}}.
##' @author Mark Wheldon, Andrew Tait
##'
##' @seealso \code{\link{do_global_run}} for just married or unmarried women runs.
##'
##' @examples vignette("FPEMglobal_Intro")
##' @export
do_global_all_women_run <- function(## Describe the run
                                    run_desc = "",
                                    age_group = "15-49",
                                    estimation_iterations = 3,
                                    burn_in_iterations = 1,
                                    steps_before_progress_report = 4,
                                    thinning = 2,
                                    chain_nums = 1:3,
                                    set_seed_chains = 1,
                                    run_in_parallel = isTRUE(length(chain_nums) > 1),
                                    input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
                                    data_csv_filename = paste0("data_cp_model_all_women_",
                                                               age_group, ".csv"),
                                    region_information_csv_filename = "country_and_area_classification_pre2024.csv",
                                    denominator_counts_csv_filename = paste0("number_of_women_", age_group, ".csv"),
                                    countries_for_aggregates_csv_filename = "countries_mwra_195_pre2024.csv",
                                    countries_in_CI_plots_csv_filename = "countries_mwra_195_pre2024.csv",
                                    special_aggregates_name = NULL,
                                    start_year = 1970.5,
                                    end_year = 2030.5,
                                    years_change = matrix(c(
                                        1990.5, 2000.5,
                                        2000.5, 2018.5,
                                        2018.5, 2030.5,
                                        2012.5, 2018.5,
                                        2012.5, 2020.5,
                                        2012.5, 2017.5), ncol = 2, byrow = TRUE),
                                    years_change2 = matrix(c(2005.5, 2010.5, 2015.5,
                                                             2000.5, 2005.5, 2010.5,
                                                             1995.5, 2000.5, 2005.5,
                                                             1990.5, 1995.5, 2000.5,
                                                             1990.5, 2000.5, 2010.5), ncol = 3, byrow = TRUE),
                                    plot_barchart_years = c(floor(start_year), floor(median(c(start_year, end_year))), floor(end_year)),
                                    plot_CI_changes_years = c(floor(start_year), floor(end_year)),
                                    make_any_aggregates = TRUE,
                                    make_all_bar_charts = TRUE,
                                    plot_maps_shapefile_folder = NULL,
                                    plot_maps_years = floor(median(c(start_year, end_year))),
                                    data_info_plot_years = c(1990, 2000, 2010),
                                    adjust_medians = TRUE,
                                    run_name_override_married = NULL,
                                    run_name_override_unmarried = NULL,
                                    run_name_override_all_women = NULL,
                                    model_diagnostics = TRUE,
                                    include_AR = TRUE,
                                    age_ratios_age_total_run_name_prefix = NULL,
                                    age_ratios_age_total_married_run_name = NULL,
                                    age_ratios_age_total_unmarried_run_name = NULL,
                                    age_ratios_age_total_all_women_run_name = NULL,
                                    age_ratios_age_total_married_output_folder_path = NULL,
                                    age_ratios_age_total_unmarried_output_folder_path = NULL,
                                    age_ratios_age_total_all_women_output_folder_path = NULL,
                                    age_ratios_age_total_denominator_counts_csv_filename = "number_of_women_15-49.csv",
                                    age_ratios_age_total_denominator_counts_folder_path = NULL,
                                    verbose = FALSE) {

    ##---------------------------------------------------------------------
    ## Run Names with Common Time Stamp

    if (identical(length(chain_nums), 1L)) {
        message("Only a single chain has been requested; all women results will *not* be created.")
    }

    systime <- format(Sys.time(), "%y%m%d_%H%M%S")

    if(is.null(run_name_override_married)) {
        run_name_override_married <- make_run_name("married", age_group, run_desc)
    }
    if(is.null(run_name_override_unmarried)) {
        run_name_override_unmarried <- make_run_name("unmarried", age_group, run_desc)
    }
    if(is.null(run_name_override_all_women)) {
        run_name_override_all_women <- make_run_name("all_women", age_group, run_desc)
    }

    ##---------------------------------------------------------------------
    ## Check output doesn't already exist

    for(rn in c(run_name_override_married, run_name_override_unmarried,
                run_name_override_all_women)) {
        ofp <- file.path("output", rn)

        if(dir.exists(ofp)) {
            if(any(grepl("^mcmc\\.info(\\.[0-9]+\\.|\\.)rda$", dir(ofp)),
                   na.rm = TRUE) ||
               file.exists(file.path(ofp, "mcmc.meta.rda")) ||
               file.exists(file.path(ofp, "mcmc.array.rda"))) {
                stop("Directory '", ofp, "' already exists with some MCMC output files. Change the run name, output folder path, or delete the existing run and start again.")
            }
        }
    }

    ##----------------------------------------------------------------------
    ## Age ratios

    if(identical(age_group, "15-49")) {
        make_age_ratios <- FALSE
        age_ratios_age_total_run_name_prefix <- NULL
        age_ratios_age_total_married_run_name <- NULL
        age_ratios_age_total_married_output_folder_path <- NULL
        age_ratios_age_total_unmarried_run_name <- NULL
        age_ratios_age_total_unmarried_output_folder_path <- NULL
        age_ratios_age_total_all_women_run_name <- NULL
        age_ratios_age_total_all_women_output_folder_path <- NULL
        message("Age ratios not created for age group '15-49'.")
    } else {

        if(!is.null(age_ratios_age_total_run_name_prefix)) {
            if(is.null(age_ratios_age_total_married_run_name)) {
                age_ratios_age_total_married_run_name <-
                    make_run_name(marital_group = "married",
                                  age_group = "15-49",
                                  run_name_override = age_ratios_age_total_run_name_prefix
                                  )
            }
            if(is.null(age_ratios_age_total_unmarried_run_name)) {
                age_ratios_age_total_unmarried_run_name <-
                    make_run_name(marital_group = "unmarried",
                                  age_group = "15-49",
                                  run_name_override = age_ratios_age_total_run_name_prefix
                                  )
            }
            if(is.null(age_ratios_age_total_all_women_run_name)) {
                age_ratios_age_total_all_women_run_name <-
                    make_run_name(marital_group = "all_women",
                                  age_group = "15-49",
                                  run_name_override = age_ratios_age_total_run_name_prefix
                                  )
            }
        }

        if((!is.null(age_ratios_age_total_married_run_name) &&
            !is.null(age_ratios_age_total_unmarried_run_name) &&
            !is.null(age_ratios_age_total_all_women_run_name)
        ) || (
            !is.null(age_ratios_age_total_married_output_folder_path) &&
            !is.null(age_ratios_age_total_unmarried_output_folder_path) &&
            !is.null(age_ratios_age_total_all_women_output_folder_path)
        )) {
            make_age_ratios <- TRUE
        } else {
            make_age_ratios <- FALSE
        }

        if(make_age_ratios) {
            if(is.null(age_ratios_age_total_married_output_folder_path)) {
                age_ratios_age_total_married_output_folder_path <-
                    file.path("output", age_ratios_age_total_married_run_name)
            }

            if(is.null(age_ratios_age_total_unmarried_output_folder_path)) {
                age_ratios_age_total_unmarried_output_folder_path <-
                    file.path("output", age_ratios_age_total_unmarried_run_name)
            }

            if(is.null(age_ratios_age_total_all_women_output_folder_path)) {
                age_ratios_age_total_all_women_output_folder_path <-
                    file.path("output", age_ratios_age_total_all_women_run_name)
            }
            if(is.null(age_ratios_age_total_denominator_counts_folder_path)) {
                age_ratios_age_total_denominator_counts_folder_path <-
                    file.path(age_ratios_age_total_unmarried_output_folder_path, "data")
            }

            if(!dir.exists(age_ratios_age_total_married_output_folder_path))
                stop("'age_ratios_age_total_married_output_folder_path' does not exist ('",
                     age_ratios_age_total_married_output_folder_path,
                     "')")
            if(!length(dir(age_ratios_age_total_married_output_folder_path)))
                stop("'age_ratios_age_total_married_output_folder_path' is empty ('",
                     age_ratios_age_total_married_output_folder_path,
                     "')")
            if(!dir.exists(file.path(age_ratios_age_total_married_output_folder_path, "countrytrajectories")))
                stop("'Directory 'countrytrajectories' not found in 'age_ratios_age_total_married_output_folder_path' ('",
                     file.path(age_ratios_age_total_married_output_folder_path, "countrytrajectories"),
                     "'")

            if(!dir.exists(age_ratios_age_total_unmarried_output_folder_path))
                stop("'age_ratios_age_total_unmarried_output_folder_path' does not exist ('",
                     age_ratios_age_total_unmarried_output_folder_path,
                     "')")
            if(!length(dir(age_ratios_age_total_unmarried_output_folder_path)))
                stop("'age_ratios_age_total_unmarried_output_folder_path' is empty ('",
                     age_ratios_age_total_unmarried_output_folder_path,
                     "')")
            if(!dir.exists(file.path(age_ratios_age_total_unmarried_output_folder_path, "countrytrajectories")))
                stop("'Directory 'countrytrajectories' not found in 'age_ratios_age_total_unmarried_output_folder_path' ('",
                     file.path(age_ratios_age_total_unmarried_output_folder_path, "countrytrajectories"),
                     "'")

            if(!dir.exists(age_ratios_age_total_all_women_output_folder_path))
                stop("'age_ratios_age_total_all_women_output_folder_path' does not exist ('",
                     age_ratios_age_total_all_women_output_folder_path,
                     "')")
            if(!length(dir(age_ratios_age_total_all_women_output_folder_path)))
                stop("'age_ratios_age_total_all_women_output_folder_path' is empty ('",
                     age_ratios_age_total_all_women_output_folder_path,
                     "')")
            if(!dir.exists(file.path(age_ratios_age_total_all_women_output_folder_path, "countrytrajectories")))
                stop("'Directory 'countrytrajectories' not found in 'age_ratios_age_total_all_women_output_folder_path' ('",
                     file.path(age_ratios_age_total_all_women_output_folder_path, "countrytrajectories"),
                     "'")
            if(make_any_aggregates) {
                if(!dir.exists(file.path(age_ratios_age_total_all_women_output_folder_path, "aggregatetrajectories")))
                    stop("'Directory 'aggregatetrajectories' not found in 'age_ratios_age_total_all_women_output_folder_path' ('",
                         file.path(age_ratios_age_total_all_women_output_folder_path, "aggregatetrajectories"),
                         "'")
            }

            if(!file.exists(file.path(age_ratios_age_total_denominator_counts_folder_path,
                                      age_ratios_age_total_denominator_counts_csv_filename)))
                stop("'age_ratios_age_total_denominator_counts_folder_path/age_ratios_age_total_denominator_counts_csv_filename' does not exist ('",
                     file.path(age_ratios_age_total_denominator_counts_folder_path,
                               age_ratios_age_total_denominator_counts_csv_filename),
                     "')")
        }
    }

    ## --------------------------------------------------------------------
    ## Married

    message("\n\n--------------------------------------------------------------------------------\n                               MARRIED WOMEN RUN\n--------------------------------------------------------------------------------\n")

    married_run_name <-
        do_global_run(## Describe the run
            run_desc = run_desc,
            marital_group = "married",
            age_group = age_group,
            ## MCMC parameters
            estimation_iterations = estimation_iterations,
            burn_in_iterations = burn_in_iterations,
            steps_before_progress_report = steps_before_progress_report,
            thinning = thinning,
            chain_nums = chain_nums,
            set_seed_chains = set_seed_chains,
            run_in_parallel = run_in_parallel,
            ## Inputs
            input_data_folder_path = input_data_folder_path,
            data_csv_filename = data_csv_filename,
            region_information_csv_filename = region_information_csv_filename,
            denominator_counts_csv_filename = denominator_counts_csv_filename,
            countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
            countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
            make_any_aggregates = make_any_aggregates,
            special_aggregates_name = special_aggregates_name,
            ## Outputs
            start_year = start_year,
            end_year = end_year,
            years_change = years_change,
            years_change2 = years_change2,
            plot_barchart_years = plot_barchart_years,
            plot_CI_changes_years = plot_CI_changes_years,
            make_all_bar_charts = make_all_bar_charts,
            plot_maps_shapefile_folder = plot_maps_shapefile_folder,
            plot_maps_years = plot_maps_years,
            data_info_plot_years = data_info_plot_years,
            adjust_medians = adjust_medians,
            age_ratios_age_total_run_name = age_ratios_age_total_married_run_name,
            age_ratios_age_total_output_folder_path = age_ratios_age_total_married_output_folder_path,
            age_ratios_age_total_denominator_counts_csv_filename = age_ratios_age_total_denominator_counts_csv_filename,
            age_ratios_age_total_denominator_counts_folder_path = age_ratios_age_total_denominator_counts_folder_path,
            ## Advanced
            run_name_override = run_name_override_married,
            model_diagnostics = model_diagnostics,
            include_AR = include_AR,
            verbose = verbose)

    ## --------------------------------------------------------------------
    ## Unmarried

    message("\n\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n                              UNMARRIED WOMEN RUN\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

    unmarried_run_name <-
        do_global_run(## Describe the run
            run_desc = run_desc,
            marital_group = "unmarried",
            age_group = age_group,
            ## MCMC parameters
            estimation_iterations = estimation_iterations,
            burn_in_iterations = burn_in_iterations,
            steps_before_progress_report = steps_before_progress_report,
            thinning = thinning,
            chain_nums = chain_nums,
            set_seed_chains = set_seed_chains,
            run_in_parallel = run_in_parallel,
            ## Inputs
            input_data_folder_path = input_data_folder_path,
            data_csv_filename = data_csv_filename,
            region_information_csv_filename = region_information_csv_filename,
            denominator_counts_csv_filename = denominator_counts_csv_filename,
            countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
            countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
            make_any_aggregates = make_any_aggregates,
            special_aggregates_name = special_aggregates_name,
            ## Outputs
            start_year = start_year,
            end_year = end_year,
            years_change = years_change,
            years_change2 = years_change2,
            plot_barchart_years = plot_barchart_years,
            plot_CI_changes_years = plot_CI_changes_years,
            plot_maps_shapefile_folder = plot_maps_shapefile_folder,
            plot_maps_years = plot_maps_years,
            data_info_plot_years = data_info_plot_years,
            adjust_medians = adjust_medians,
            age_ratios_age_total_run_name = age_ratios_age_total_unmarried_run_name,
            age_ratios_age_total_output_folder_path = age_ratios_age_total_unmarried_output_folder_path,
            age_ratios_age_total_denominator_counts_csv_filename = age_ratios_age_total_denominator_counts_csv_filename,
            age_ratios_age_total_denominator_counts_folder_path = age_ratios_age_total_denominator_counts_folder_path,
            ## Advanced
            run_name_override = run_name_override_unmarried,
            model_diagnostics = model_diagnostics,
            include_AR = include_AR,
            verbose = verbose)

    ##-----------------------------------------------------------------------------
    ## STOP if only one chain

    if (identical(length(chain_nums), 1L)) {
        warning("All women results *not* available with a single chain.")
        return(invisible())
    }

    ##
    ##-----------------------------------------------------------------------------

    ## More than one chain >> continue >>

    ## --------------------------------------------------------------------
    ## All Women

    message("\n\n================================================================================\n                              ALL WOMEN WOMEN RUN\n================================================================================\n")

    all_women_run_name <-
        combine_runs (## Describe the run
            run_desc = run_desc,
            ## Inputs
            married_women_run_output_folder_path = file.path("output", married_run_name),
            unmarried_women_run_output_folder_path = file.path("output", unmarried_run_name),
            unmarried_women_run_data_folder_path = input_data_folder_path,
            denominator_counts_csv_filename = denominator_counts_csv_filename,
            region_information_csv_filename = region_information_csv_filename,
            countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
            countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
            special_aggregates_name = special_aggregates_name,
            ## Outputs
            start_year = start_year,
            end_year = end_year,
            years_change = years_change,
            years_change2 = years_change2,
            make_any_aggregates = make_any_aggregates,
            adjust_medians = adjust_medians,
            age_ratios_age_total_unmarried_run_name = age_ratios_age_total_unmarried_run_name,
            age_ratios_age_total_married_run_name = age_ratios_age_total_married_run_name,
            age_ratios_age_total_all_women_run_name = age_ratios_age_total_all_women_run_name,
            age_ratios_age_total_unmarried_output_folder_path = age_ratios_age_total_unmarried_output_folder_path,
            age_ratios_age_total_married_output_folder_path = age_ratios_age_total_married_output_folder_path,
            age_ratios_age_total_all_women_output_folder_path = age_ratios_age_total_all_women_output_folder_path,
            age_ratios_age_total_denominator_counts_csv_filename = age_ratios_age_total_denominator_counts_csv_filename,
            age_ratios_age_total_denominator_counts_folder_path = age_ratios_age_total_denominator_counts_folder_path,
            ## Advanced
            run_name_override = run_name_override_all_women,
            verbose = verbose)

    make_results(run_name = all_women_run_name,
                 input_data_folder_path = file.path("output", unmarried_run_name, "data"),
                 countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
                 plot_CI_changes_years = plot_CI_changes_years,
                 make_all_bar_charts = make_all_bar_charts,
                 plot_barchart_years = plot_barchart_years,
                 plot_maps_shapefile_folder = plot_maps_shapefile_folder,
                 plot_maps_years = plot_maps_years,
                 data_info_plot_years = data_info_plot_years,
                 adjust_medians = adjust_medians,
                 make_any_aggregates = make_any_aggregates,
                 special_aggregates_name = special_aggregates_name,
                 make_age_ratios = make_age_ratios,
                 verbose = verbose)

    ## --------------------------------------------------------------------
    ## Age Ratios

    if(make_age_ratios) {

        message("\n\n................................................................................\n                      AGE RATIOS FOR MARRIED AND UNMARRIED\n................................................................................\n")

        ## Need to run 'make_results()' again for age ratios for
        ## married and unmarried. Not ideal but ...

        make_results(run_name = married_run_name,
                     input_data_folder_path = file.path("output", married_run_name, "data"),
                     countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
                     plot_CI_changes_years = plot_CI_changes_years,
                     make_all_bar_charts = make_all_bar_charts,
                     plot_barchart_years = plot_barchart_years,
                     plot_maps_shapefile_folder = plot_maps_shapefile_folder,
                     plot_maps_years = plot_maps_years,
                     data_info_plot_years = data_info_plot_years,
                     adjust_medians = adjust_medians,
                     make_any_aggregates = make_any_aggregates,
                     special_aggregates_name = special_aggregates_name,
                     make_age_ratios = make_age_ratios,
                     verbose = verbose)

        make_results(run_name = unmarried_run_name,
                     input_data_folder_path = file.path("output", unmarried_run_name, "data"),
                     countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
                     plot_CI_changes_years = plot_CI_changes_years,
                     make_all_bar_charts = make_all_bar_charts,
                     plot_barchart_years = plot_barchart_years,
                     plot_maps_shapefile_folder = plot_maps_shapefile_folder,
                     plot_maps_years = plot_maps_years,
                     data_info_plot_years = data_info_plot_years,
                     adjust_medians = adjust_medians,
                     make_any_aggregates = make_any_aggregates,
                     special_aggregates_name = special_aggregates_name,
                     make_age_ratios = make_age_ratios,
                     verbose = verbose)

    }

    return(invisible(list(married_run_name = married_run_name,
                          unmarried_run_name = unmarried_run_name,
                          all_women_run_name = all_women_run_name)))
}


################################################################################
###
### VALIDATIONS
###
################################################################################

##' Generate MCMC sample for an out-of-sample validation of an FPEM global run
##'
##' Generates the MCMC output of an out-of-sample validation of a completed
##' global run of an FPEM model. No post-processing or results generation is done; only the
##' chains are produced. The recommended way to use this function is via a call
##' to \code{\link{do_global_validation_run}}. See the section \dQuote{See Also} below.
##'
##' See \dQuote{Details} in the help file for
##' \code{\link{do_global_validation_run}}.
##' @param run_name_to_validate Name of completed global run to
##'     validate.
##' @param run_name_to_validate_output_folder_path
##' @param exclude_unmet_only Logical; do validtion exercise leaving
##'     out unmet need observations? See details.
##' @param exclude_unmet_only_test_prop Numeric; the proportion of
##'     observations to be left out for an unmet need validation run.
##' @param at_random Logical; do validtion exercise leaving out
##'     observations at random? See details.
##' @param at_random_min_c Minimum number of data points per country
##'     to ensure are left in training set.
##' @param at_random_test_prop Proportion of obs used for test set
##'     when \code{at_random} is \code{TRUE}.
##' @param at_end Logical; do validtion exercise leaving out all
##'     observations after \code{year_cutoff}. See details.
##' @param at_end_not_1_obs_c Logical; should obs that are the only
##'     one for their particular country be retained in the training
##'     set?
##' @param at_random_no_data Logical; do validation exercise where all
##'     observations for a randomly selected set of countries are left
##'     out at random?
##' @param at_random_no_data_strata Column in the country
##'     classifications file (used in the run being validated) to
##'     stratify on if \code{at_random_no_data = TRUE}. If \code{NULL}
##'     (default) do not stratify.
##' @param at_random_no_data_test_prop Numeric; the proportion of
##'     observations to be left out when \code{at_random_no_data =
##'     TRUE}.
##' @param leave_iso_out Logical; do validation exercise where all
##'     data for only one country is left out?
##' @param leave_iso_out_iso_test Three-digit country ISO code,
##'     numeric or character, designating the country to leave out if
##'     \code{leave_iso_out = TRUE}.
##' @param year_cutoff The cut-off year to use to separate test from
##'     training set if \code{at_end = TRUE}. All observations with
##'     observation year equal to or greater than \code{year_cutoff}
##'     are put in the test set.
##' @param seed_validation Random seed used, among other things, in
##'     selection of countries to leave out.
##' @param generate_new_set Logical; generate a new training set in
##'     validation exercise?
##' @param run_name_to_validate_output_folder File path to results of
##'     run \code{run_name_to_validate}.
##' @return A name for the run returned invisibly as a character
##'     string. MCMC chains are saved to
##'     \file{\code{output_folder_path}/temp.JAGSobjects}. They need
##'     to be post-processed with \code{\link{post_process_mcmc}}. The
##'     run name must be passed to \code{\link{post_process_mcmc}} to
##'     locate find the saved chains for processing. Run names for
##'     married and unmarried runs must also be passed to
##'     \code{\link{combine_runs}} to generate all women MCMC results.
##' @author Mark Wheldon
##' @seealso \code{\link{do_global_validation_run}} which calls this
##'     function to generate MCMC results for a validation exercise on
##'     a married or unmarried women run, post-processes it, and
##'     produces results all in one call.
##' @inheritParams do_global_mcmc
##' @inheritParams post_process_mcmc
##' @examples vignette("FPEMglobal_Intro")
##' @export
do_global_validation_mcmc <-
    function(run_desc = "",
             run_name_override = NULL,
             run_name_to_validate = NULL,
             run_name_to_validate_output_folder_path = file.path("output", run_name_to_validate),
             exclude_unmet_only = FALSE,
             exclude_unmet_only_test_prop = 0.2,
             at_random = FALSE,
             at_random_min_c = 1,
             at_random_test_prop = 0.2,
             at_end = FALSE,
             at_end_not_1_obs_c = FALSE,
             at_random_no_data = FALSE,
             at_random_no_data_strata = NULL,
             at_random_no_data_test_prop = 0.2,
             leave_iso_out = FALSE,
             leave_iso_out_iso_test = NULL,
             year_cutoff = 2005,
             seed_validation = 12345,
             generate_new_set = TRUE,
             estimation_iterations = 3,
             burn_in_iterations = 1,
             steps_before_progress_report = 4,
             thinning = 2,
             chain_nums = 1:3,
             run_in_parallel = isTRUE(length(chain_nums) > 1),
             output_folder_path = NULL,
             verbose = FALSE) {

        ## --------------------------------------------------------------------
        ## Parallelization mechanism

        if (run_in_parallel) {
            if (requireNamespace("doMC", quietly = TRUE)) {
                doMC::registerDoMC(cores = min(parallel::detectCores(), length(chain_nums)))
                message("Running with ", foreach::getDoParWorkers(), " core(s)")
            } else {
                if (requireNamespace("doParallel", quietly = TRUE)) {
                    doParallel::registerDoParallel(cores = min(parallel::detectCores(), length(chain_nums)))
                    message("Running with ", foreach::getDoParWorkers(), " core(s)")
                } else {
                    warning("Package 'doMC' not installed; chains will be run in serial.")
                }
            }
        }

        ##----------------------------------------------------------------------------
        ## Meta information

        ## Get information about the run being validated (should
        ## probably make it so that these are all in 'mcmc.meta').
        global_mcmc_args <-
            get(load(file.path(run_name_to_validate_output_folder_path,
                               "global_mcmc_args.RData"), verbose = verbose))
        global_mcmc_meta <-
            get(load(file.path(run_name_to_validate_output_folder_path, "mcmc.meta.rda"), verbose = verbose))

        ## --------------------------------------------------------------------
        ## Run name and output paths

        ## Type of validation being done
        validation_names <-
            c("at_random", "at_end", "exclude_unmet_only", "at_random_no_data", "leave_iso_out")
        validation_indicator <- unlist(mget(validation_names))
        if(sum(validation_indicator) > 1) {
            warning("More than one validation exercise requested but can only do one. Priority order is:\n",
                    validation_names, ".")
        }
        validation_doing <- validation_names[validation_indicator]

        ## Run name of this (validation) run (used for output folder)
        if(!is.null(run_name_override)) {
            run_name <- run_name_override
        } else {
            if(!is.null(run_desc) && isTRUE(nchar(run_desc) > 0)){
                run_note <- paste(validation_doing, run_desc, sep = "_")
            } else {
                run_note <- validation_doing
            }
            run_name <- paste(run_name_to_validate, "valid", run_note, sep = "_")
        }

        if(is.null(output_folder_path)) {
            output_folder_path <- file.path("output", run_name)
        }

        ## Check if output already exists. If not, create output
        ## dir. If yes, stop with an error.
        if (!dir.exists(output_folder_path)) {
            dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)
        } else {
            stop("A validation of this type has already been attempted. To re-run, delete or rename '", output_folder_path, "' and try again.")
        }

        ## Make sure the global run saved its input data These will be looked for
        ## in the 'data' subdirectory of the global run output
        ## file. Do it this way so that the validation run depends
        ## only on the saved outputs from the global run, rather than,
        ## for example, a reference to an input file that is part of
        ## the package, or something else that might be unavailable or
        ## different if the validation run is done on a different
        ## machine from the run being validated.
        if(is.null(global_mcmc_args$data_csv_filename)) {
            stop("'global_mcmc_args$data_csv_filename' is 'NULL'; don't know what input data was used for the global run.")
        } else if(is.null(global_mcmc_args$region_information_csv_filename)) {
            stop("'global_mcmc_args$region_information_csv_filename' is 'NULL'; don't know what classifications file was used for the global run.")
        } else {
            global_run_input_data_csv <- file.path(run_name_to_validate_output_folder_path, "data",
                                                   basename(global_mcmc_args$data_csv_filename))
            global_run_region_info_csv <- file.path(run_name_to_validate_output_folder_path, "data",
                                                    basename(global_mcmc_args$region_information_csv_filename))
            if(!file.exists(global_run_input_data_csv) && !file.exists(global_run_region_info_csv)) {
                stop("The run to be validated does not have a subdirectory called 'data' with the input data and country classifications files.")
            }
        }

        ##-----------------------------------------------------------------------------
        ## Copy to Output

        output_data_folder_path <- file.path(output_folder_path, "data")
        copy_data_files(run_name = run_name,
                        data_dir = file.path(run_name_to_validate_output_folder_path, "data"),
                        data_local = output_data_folder_path)

        ## --------------------------------------------------------------------
        ## Make MCMC chains

        message("This run has 'run_name': ", run_name, ". It validates ", run_name_to_validate, "'.")
        cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": This run validates '", run_name_to_validate, "'.",
            file = file.path(output_folder_path, "log.txt"), append = TRUE)

        RunMCMC(
            N.ITER = estimation_iterations,
            N.BURNIN = burn_in_iterations,
            N.STEPS = steps_before_progress_report,
            N.THIN = thinning,
            run.on.server = run_in_parallel,
            run.name =  run_name,
            data.csv = global_run_input_data_csv,
            regioninfo.csv = global_run_region_info_csv,
            output.dir = output_folder_path,
            ChainNums = chain_nums,
            disagg.RN.PMA = TRUE,
            write.model.fun = global_mcmc_meta$general$write.model.fun,
            include.AR = global_mcmc_meta$include.AR,
            marital.group = global_mcmc_meta$general$marital.group,
            age.group = global_mcmc_meta$general$age.group,
            uwra.z.priors = global_mcmc_args$uwra_z_priors,
            uwra.Omega.priors = global_mcmc_args$uwra_Omega_priors,
            uwra.kappa.c.priors = global_mcmc_args$uwra_kappa_c_priors,
            include.c.no.data = FALSE,   #Never for validation
            timing.world.priors = global_mcmc_args$timing_world_priors,
            EA.bias.negative = global_mcmc_meta$general$EA.bias.negative,
            HW.bias.negative = global_mcmc_meta$general$HW.bias.negative,
            ## Validation Parameters
            exclude.unmet.only = exclude_unmet_only,
            exclude.unmet.only.test.prop = exclude_unmet_only_test_prop,
            at.random = at_random,
            at.random.min.c = at_random_min_c,
            at.random.test.prop = at_random_test_prop,
            at.end = at_end,
            at.end.not.1.obs.c = at_end_not_1_obs_c,
            at.random.no.data = at_random_no_data,
            at.random.no.data.strata = at_random_no_data_strata,
            at.random.no.data.test.prop = at_random_no_data_test_prop,
            leave.iso.out = leave_iso_out,
            leave.iso.out.iso.test = leave_iso_out_iso_test,
            year.cutoff = year_cutoff,
            seed.validation = seed_validation,
            generate.new.set = generate_new_set,
            sink.seed.logfile = FALSE
        )

        ##-----------------------------------------------------------------------------
        ## Return

        return(invisible(run_name))

    }


##' Perform out-of-sample validation for a completed run of the FPEM global model
##'
##' This generates the MCMC chains using \code{\link{do_global_validation_mcmc}},
##' post-processes them using \code{\link{post_process_mcmc}} and
##' produces plots and tables using \code{\link{make_results}}.
##'
##' Exactly one of \code{exclude_unmet_only}, \code{at_random}, \code{at_end},
##' \code{at_random_no_data}, \code{leave_iso_out} should be \code{TRUE}. The
##' possible validation exercises are
##' \tabular{lll}{
##' \code{at_random = TRUE} \tab Leave out some obs at random, chosen at random \cr
##' \code{at_end = TRUE} \tab Leave out all obs after a certain year \cr
##' \code{at_random_no_data = TRUE} \tab Leave out all obs for some countries, chosen at random \cr
##' \code{leave_iso_out = TRUE} \tab Leave out all obs for a certain country
##' }
##'
##' @section Note:
##' There is no \dQuote{\code{do_global_validation_all_women_run}}
##'     function. You must run validation on married and unmarried women runs
##'     separately. Validation of an all women run is undefined.
##' @inheritParams do_global_validation_mcmc
##' @inheritParams do_global_mcmc
##' @inheritParams post_process_mcmc
##' @inheritParams make_results
##' @return A name for the run returned invisibly as a character string. Results
##'     are saved to \file{\code{output_folder_path}}.
##' @author Mark Wheldon
##' @examples vignette("FPEMglobal_Intro")
##' @export
do_global_validation_run <- function(run_desc = "",
                                     run_name_override = NULL,
                                     run_name_to_validate = NULL,
                                     run_name_to_validate_output_folder_path = file.path("output", run_name_to_validate),
                                     input_data_folder_path = NULL,
                                     exclude_unmet_only = FALSE,
                                     exclude_unmet_only_test_prop = 0.2,
                                     at_random = FALSE,
                                     at_random_min_c = 1,
                                     at_random_test_prop = 0.2,
                                     at_end = FALSE,
                                     at_end_not_1_obs_c = FALSE,
                                     at_random_no_data = FALSE,
                                     at_random_no_data_strata = NULL,
                                     at_random_no_data_test_prop = 0.2,
                                     leave_iso_out = FALSE,
                                     leave_iso_out_iso_test = NULL,
                                     year_cutoff = 2005,
                                     seed_validation = 12345,
                                     generate_new_set = TRUE,
                                     estimation_iterations = 3,
                                     burn_in_iterations = 1,
                                     steps_before_progress_report = 4,
                                     thinning = 2,
                                     chain_nums = 1:3,
                                     run_in_parallel = isTRUE(length(chain_nums) > 1),
                                     output_folder_path = NULL,
                                     verbose = FALSE) {

    ##----------------------------------------------------------------------------
    ## Must choose a validation exercise

    if(!identical(as.double(sum(at_random, at_end, exclude_unmet_only, at_random_no_data, leave_iso_out)), 1)) {
        stop("You must choose exactly one validation exercise. Exactly one of 'at_random', 'at_end', 'exclude_unmet_only', 'at_random_no_data', 'leave_iso_out' must be TRUE.")
    }

    ##----------------------------------------------------------------------------
    ## Meta information

    ## Get information about the run being validated (should
    ## probably make it so that these are all in 'mcmc.meta').

    global_mcmc_args_filepath <-
        file.path(run_name_to_validate_output_folder_path, "global_mcmc_args.RData")
    if(file.exists(global_mcmc_args_filepath))
        global_mcmc_args <- get(load(global_mcmc_args_filepath, verbose = verbose))

    if(is.null(input_data_folder_path))
        input_data_folder_path <- file.path(run_name_to_validate_output_folder_path, "data")
    if(!dir.exists(input_data_folder_path))
        stop("'", input_data_folder_path, "' does not exist; cannot find data used in run to validate.")

    global_mcmc_meta_filepath <-
        file.path(run_name_to_validate_output_folder_path, "mcmc.meta.rda")
    if(file.exists(global_mcmc_meta_filepath)) {
        global_mcmc_meta <- get(load(global_mcmc_meta_filepath, verbose = verbose))
    } else {
        stop("'", global_mcmc_meta_filepath, "' does not exist. Cannot do validation without this information.")
    }

    ## Load post-process args
    post_process_mcmc_args_filepath <-
        file.path(run_name_to_validate_output_folder_path, "post_process_args.RData")
    if(file.exists(post_process_mcmc_args_filepath)) {
        post_process_mcmc_args <- get(load(post_process_mcmc_args_filepath, verbose = verbose))
        if(!file.exists(file.path(input_data_folder_path,
                                  basename(post_process_mcmc_args$denominator_counts_csv_filename))))
            stop("Denominator counts file in 'post_process_args.Rdata' not found.")
        if(!file.exists(file.path(input_data_folder_path,
                                  basename(post_process_mcmc_args$countries_for_aggregates_csv_filename))))
            stop("Country aggregates file in 'post_process_args.Rdata' not found.")
    }

    ## --------------------------------------------------------------------
    ## Run name and output paths

    ## Type of validation being done
    validation_names <-
        c("at_random", "at_end", "exclude_unmet_only", "at_random_no_data", "leave_iso_out")
    validation_indicator <- unlist(mget(validation_names))
    validation_doing <- validation_names[validation_indicator]

    ## Leave ISO out validation
    if(validation_doing == "leave_iso_out") {
        validation_doing <- paste0(validation_doing, "_", as.character(leave_iso_out_iso_test))
    }

    ## Run name of this (validation) run (used for output folder)
    if(!is.null(run_name_override)) {
        run_name <- run_name_override
    } else {
        if(!is.null(run_desc) && isTRUE(nchar(run_desc) > 0)){
            run_note <- paste(validation_doing, run_desc, sep = "_")
        } else {
            run_note <- validation_doing
        }
        run_name <- paste(run_name_to_validate, "valid", run_note, sep = "_")
    }

    if(is.null(output_folder_path)) {
        output_folder_path <- file.path("output", run_name)
    }

    ##---------------------------------------------------------------------
    ## MCMC

    message("\nDoing validation run.")

    run_name_valid <-
        do_global_validation_mcmc(run_desc = run_desc,
                                  run_name_override = run_name_override,
                                  run_name_to_validate = run_name_to_validate,
                                  run_name_to_validate_output_folder_path = run_name_to_validate_output_folder_path,
                                  exclude_unmet_only = exclude_unmet_only,
                                  exclude_unmet_only_test_prop = exclude_unmet_only_test_prop,
                                  at_random = at_random,
                                  at_random_min_c = at_random_min_c,
                                  at_random_test_prop = at_random_test_prop,
                                  at_end = at_end,
                                  at_end_not_1_obs_c = at_end_not_1_obs_c,
                                  at_random_no_data = at_random_no_data,
                                  at_random_no_data_strata = at_random_no_data_strata,
                                  at_random_no_data_test_prop = at_random_no_data_test_prop,
                                  leave_iso_out = leave_iso_out,
                                  leave_iso_out_iso_test = leave_iso_out_iso_test,
                                  year_cutoff = year_cutoff,
                                  seed_validation = seed_validation,
                                  generate_new_set = generate_new_set,
                                  estimation_iterations = estimation_iterations,
                                  burn_in_iterations = burn_in_iterations,
                                  steps_before_progress_report = steps_before_progress_report,
                                  thinning = thinning,
                                  chain_nums = chain_nums,
                                  run_in_parallel = run_in_parallel,
                                  output_folder_path = output_folder_path,
                                  verbose = verbose)

    ##---------------------------------------------------------------------------
    ##  Post-Process

    post_process_mcmc(run_name = run_name_valid,
                      output_folder_path = output_folder_path,
                      input_data_folder_path = input_data_folder_path,
                      denominator_counts_csv_filename = basename(post_process_mcmc_args$denominator_counts_csv_filename),
                      countries_for_aggregates_csv_filename = basename(post_process_mcmc_args$countries_for_aggregates_csv_filename),
                      start_year = post_process_mcmc_args$start_year,
                      end_year = post_process_mcmc_args$end_year,
                      years_change = post_process_mcmc_args$years_change,
                      years_change2 = post_process_mcmc_args$years_change2,
                      model_diagnostics = TRUE,
                      special_aggregates_name = NULL,
                      age_ratios_age_total_run_name = NULL,
                      age_ratios_age_total_output_folder_path = NULL,
                      age_ratios_age_total_denominator_counts_csv_filename = NULL,
                      age_ratios_age_total_denominator_counts_folder_path = NULL,
                      verbose = verbose)

    ##-----------------------------------------------------------------------------
    ##  Plots, Tables

    make_results(run_name = run_name_valid,
                 input_data_folder_path = input_data_folder_path,
                 output_folder_path = output_folder_path,
                 verbose = verbose)

    ##-----------------------------------------------------------------------------
    ## Finish

    ## if(!interactive()) copy_Rout_files(run_name = run_name_valid)

    return(invisible(run_name_valid))

}



################################################################################
###
### MISC
###
################################################################################

##' Rename global run
##'
##' Renames the files and directories , and meta data, of a global run of FPEM for a single
##' marital group.
##'
##' @section Note: If you modify \code{ignore} make sure
##'     \file{\code{output_folder_path}/data} still matches; it is not
##'     a good idea to rename files in that directory.
##'
##' @inheritParams do_global_mcmc
##' @param run_name Character. Name of run to be renamed.
##' @param new_run_name Character. New run name.
##' @param output_folder_path Filepath to directory where outputs are saved. If \code{NULL}, defaults to
##'     \code{file.path("output", run_name)}.
##' @param ignore Regular expression. Files and directories with names that match will not be renamed.
##' @return Called for side effect only.
##' @author Mark Wheldon
##' @export
rename_global_run <- function(run_name,
                              new_run_name,
                              output_folder_path = NULL,
                              ignore = "^data$", verbose = FALSE) {

    ##-----------------------------------------------------------------------------
    ## Set-up

    if(is.null(output_folder_path)) output_folder_path <- file.path("output", run_name)

    ##-----------------------------------------------------------------------------
    ## Functions

    crawl_and_rename <- function(dir_path, run_name = run_name,
                                 new_run_name = new_run_name, ignore = ignore) {
        info <- file.info(dir(dir_path, full.names = TRUE))
        if (nrow(info)) {
            for(i in 1:nrow(info)) {
                fn <- basename(rownames(info)[i])
                if(info[i, "isdir"]) {
                    crawl_and_rename(dir_path = file.path(dir_path, fn),
                                     run_name = run_name,
                                     new_run_name = new_run_name,
                                     ignore = ignore)
                }
                ## should still rename subdirectories (except 'ignore') so no 'else'.
                if(grepl(run_name, fn, fixed = TRUE) && !grepl(pattern = ignore, x = fn)) {
                    new_fn <- gsub(run_name, new_run_name, fn, fixed = TRUE)
                    check <-
                        try(file.rename(from = file.path(dir_path, fn),
                                        to = file.path(dir_path, new_fn)))
                    if(inherits(check, "try-error") || !isTRUE(check))
                        warning("'", x,
                                "' was unable to be renamed. 'file.rename' returned: ",
                                paste(check))
                }
            }
        }
    }

    ##-----------------------------------------------------------------------------
    ## Rename

    ## 'global_mcmc_args' object
    global_args_fn <- file.path(output_folder_path, "global_mcmc_args.RData")
    if(file.exists(global_args_fn)) {
        load(global_args_fn, verbose = verbose)
        global_mcmc_args$renamed <- TRUE
        global_mcmc_args$rename_list <- c(new_run_name, global_mcmc_args$rename_list)
        if (!"rename_list" %in% names(comment(global_mcmc_args))) {
            comment(global_mcmc_args) <-
                c(comment(global_mcmc_args),
                  rename_list = c("'rename_list' is in reverse chronologial order; most recent run name is first."))
        }
        save(global_mcmc_args, file = global_args_fn)
    }
    post_process_args_fn <- file.path(output_folder_path, "post_process_args.RData")
    if(file.exists(post_process_args_fn)) {
        load(post_process_args_fn, verbose = verbose)
        post_process_args$renamed <- TRUE
        post_process_args$rename_list <- c(new_run_name, post_process_args$rename_list)
        if (!"rename_list" %in% names(comment(post_process_args))) {
            comment(post_process_args) <- c(comment(post_process_args),
                                            rename_list = c("'rename_list' is in reverse chronologial order; most recent run name is first."))
        }
        save(post_process_args, file = post_process_args_fn)
    }
    combine_args_fn <- file.path(output_folder_path, "combine_runs_args.RData")
    if(file.exists(combine_args_fn)) {
        load(combine_args_fn, verbose = verbose)
        combine_runs_args$renamed <- TRUE
        combine_runs_args$rename_list <- c(new_run_name, combine_runs_args$rename_list)
        if (!"rename_list" %in% names(comment(combine_runs_args))) {
            comment(combine_runs_args) <- c(comment(combine_runs_args),
                                            rename_list = c("'rename_list' is in reverse chronologial order; most recent run name is first."))
        }
        save(combine_runs_args, file = combine_args_fn)
    }

    crawl_and_rename(output_folder_path, run_name, new_run_name, ignore = ignore)

    ##----------------------------------------------------------------------------
    ## LOG

    msg <- paste0("Renamed run. Was called ", run_name, " now called ", new_run_name)
    message(msg)
    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg,
        file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##----------------------------------------------------------------------------
    ## Return

    return(new_run_name)
}


##' Compare two global runs by plotting
##'
##' Generates \dQuote{CI} (credible interval) line plots that compare
##' the results of two global model runs. Country- and aggregate-level
##' (if \code{compare_aggregates} is \code{TRUE}) plots are available.
##'
##' @param run_1_name The name of the first run to compare.
##' @param run_2_name The name of the second run to compare.
##' @param run_1_output_folder_path Filepath to directory where
##'     outputs for \code{run_1_name} are saved.
##' @param run_1_plot_label The label to use for \dQuote{run_1} in the
##'     plots. Defaults to \code{run_1_name}.
##' @param run_2_output_folder_path See
##'     \code{run_1_output_folder_path}, but for \dQuote{run_2}.
##' @param run_2_plot_label See \code{run_2_plot_label}, but for
##'     \dQuote{run_2}.
##' @param output_folder_path Filepath to the directory in which the
##'     comparison plots will be saved.
##' @param plot_data Logical; should available input data be plotted?
##'     Defaults to \code{TRUE} unless either \code{all_women} is
##'     \code{TRUE} or \dQuote{run_1} and \dQuote{run_2} are from
##'     different marital groups.
##' @param all_women Logical; is the run an all women run such as the
##'     kind produced by \code{\link{combine_runs}} or
##'     \code{\link{do_global_all_women_run}}? If \code{NULL} an
##'     attempt is made to determine this automatically from
##'     \file{mcmc.meta.rda} in the output folder of
##'     \dQuote{run_1}. Note that either both or neither must be
##'     either all women runs.
##' @param compare_aggregates Logical; should comparison plots be made
##'     for the aggregates as well as the countries? If \code{FALSE}
##'     only plots for countries are produced.
##' @return Called for the side effect of producing plots.
##' @author Mark Wheldon (wrapper) based on underlying function by Jin
##'     Rou New and Leontine Alkema.
##' @seealso \code{\link{compare_runs_fishbone_plots}}.
##' @author Jin Rou New, Leontine Alkema, Mark Wheldon
##' @export
compare_runs_CI_plots <- function(run_name_1, run_name_2,
                                  run_1_output_folder_path = file.path("output", run_name_1),
                                  run_1_plot_label = run_name_1,
                                  run_2_output_folder_path = file.path("output", run_name_2),
                                  run_2_plot_label = run_name_2,
                                  output_folder_path = file.path(run_1_output_folder_path, "fig", "compare_runs_plots"),
                                  plot_data = NULL,
                                  all_women = NULL,
                                  compare_aggregates = TRUE,
                                  verbose = FALSE) {

    message("Plotting comparison of ", run_name_1, " and ", run_name_2)

    ##----------------------------------------------------------------------------
    ## Meta Info

    load(file.path(run_1_output_folder_path, "mcmc.meta.rda"), verbose = verbose)
    run_1_mcmc_meta <- mcmc.meta

    load(file.path(run_2_output_folder_path, "mcmc.meta.rda"), verbose = verbose)
    run_2_mcmc_meta <- mcmc.meta

    ## All women run?
    if (!identical(run_1_mcmc_meta$general$all.women.run.copy,
                   run_2_mcmc_meta$general$all.women.run.copy))
        stop("Both runs must be either 'all women' runs or not.")
    if (!is.logical(all_women)) all_women <- isTRUE(run_1_mcmc_meta$general$all.women.run.copy)

    ##----------------------------------------------------------------------------
    ## Set values

    ## Directories
    if (!dir.exists(output_folder_path)) stopifnot(dir.create(output_folder_path))

    ## Marital group
    if (!all_women && identical(run_1_mcmc_meta$general$marital.group,
                                run_2_mcmc_meta$general$marital.group))
        UWRA <- identical(run_1_mcmc_meta$general$marital.group, "UWRA")
    else UWRA <- FALSE


    ## Plot data?
    if (!is.logical(plot_data)) {
        if (all_women || !identical(run_1_mcmc_meta$general$marital.group,
                                    run_2_mcmc_meta$general$marital.group)) plot_data <- FALSE
        else plot_data <- TRUE
    } else if (plot_data && !identical(run_1_mcmc_meta$general$marital.group,
                                       run_2_mcmc_meta$general$marital.group)) {
        message("Data from 'run_1' ('", run_name_1, "') will be plotted.")
        warning("Plots will be labelled as 'married' women results.")
    }

    ##----------------------------------------------------------------------------
    ## Plots

    PlotComparison(run.name2 = run_2_plot_label,
                   output.dir2 = paste0(run_2_output_folder_path, "/"),
                   run.name = run_1_plot_label,
                   output.dir = paste0(run_1_output_folder_path, "/"),
                   fig.dir = output_folder_path,
                   plot.for.aggregates = compare_aggregates,
                   all.women = all_women,
                   UWRA = UWRA,
                   plot_data = plot_data)
}



## Make sure output directory is valid.
##
## 'post_processed' is 'TRUE' by default because an un-processed
## directory doesn't even have 'mcmc.array.rda', which means it's
## unlikely to be used.




##' Check that a directory is a valid FPEMglobal output directory
##'
##' Checks the content of \code{output_dir} to make sure certain
##' directories and files are present. If some are missing, an error
##' is returned, otherwise \code{output_folder_path} is returned
##' invisibly.
##'
##' The default for 'post_processed' is 'TRUE' by because an
##' un-processed directory doesn't even have 'mcmc.array.rda', which
##' means it's unlikely to be used.
##'
##' @param output_folder_path Path (or paths, if a vector) to directory to validate.
##' @param post_processed Logical; has \code{\link{post_process_mcmc}}
##'     been run on the directory?
##' @param countrytrajectories Logical; check for
##'     \file{countrytrajectories} or \file{aggregatetrajectories}
##'     directories?
##' @param made_results Logical; has \code{\link{make_results}} been
##'     run on the directory?
##' @param adjusted_medians Logical; check for adjusted median
##'     results?
##' @param age_ratios Logical; check for age ratio results? Defaults
##'     to \code{FALSE} if \code{output_folder_path} points to a
##'     \dQuote{15-49} run, otherwise the value of
##'     \code{post_processed}.
##' @return If all checks pass, \code{output_folder_path} is returned
##'     invisibly, otherwise an error is thrown.
##' @author Mark Wheldon
##' @export
assert_valid_output_dir <- function(output_folder_path,
                                    post_processed = TRUE,
                                    countrytrajectories = post_processed,
                                    made_results = post_processed,
                                    adjusted_medians = post_processed,
                                    age_ratios = NULL,
                                    verbose = FALSE) {

    ## --------------------
    ## RECURSE
    if (length(output_folder_path) > 1) {
        assert_valid_output_dir_v <- Vectorize(FPEMglobal::assert_valid_output_dir, vectorize.args = "output_folder_path")
        call_args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
        return(do.call("assert_valid_output_dir_v", args = call_args))
    }
    ## --------------------

    ## -------* argument check

    checkmate::qassert(output_folder_path, "S1")

    ## -------* Existence

    checkmate::assert_directory_exists(output_folder_path)

    ## Emptiness
    empty_dirs <- isTRUE(identical(length(dir(output_folder_path)), 0L))
    if (any(empty_dirs)) {
        stop("Directory \n\t", output_folder_path, "\nis empty.")
    }

    ## -------* Content

    ## -------** All Marital Groups

    ## Meta Info
    checkmate::assert_file_exists(file.path(output_folder_path, c("mcmc.meta.rda")))
    mcmc.meta <- get(load(file.path(output_folder_path, "mcmc.meta.rda"), verbose = verbose))

    if (identical(mcmc.meta$general$age.group, "15-49")) age_ratios <- FALSE
    else if (is.null(age_ratios)) age_ratios <- post_processed

    checkmate::assert_directory_exists(file.path(output_folder_path, "data"))
    if (post_processed) {
        checkmate::assert_file_exists(file.path(output_folder_path, c("par.ciq.rda")))
    }
    if (made_results) {
        checkmate::assert_directory_exists(file.path(output_folder_path, c("fig", "table")))
    }
    if (countrytrajectories) {
        checkmate::assert_directory_exists(file.path(output_folder_path, "countrytrajectories"))
    }

    ## -------** Married / Unmarried

    if (!isTRUE(mcmc.meta$general$all.women.run.copy)) {

        checkmate::assert_file_exists(file.path(output_folder_path,
                                                c("global_mcmc_args.RData", "model.txt", "iso.Ptp3s.key.csv")))
        if (post_processed) {
            checkmate::assert_file_exists(file.path(output_folder_path, c("data.global.rda",
                                                                          "post_process_args.RData",
                                                                          "res.country.rda", "res.aggregate.rda")))
            if (!isTRUE(mcmc.meta$general$is.age.calibrated))
                checkmate::assert_file_exists(file.path(output_folder_path, c("mcmc.array.rda")))
        }

        if (adjusted_medians)
            checkmate::assert_file_exists(file.path(output_folder_path, c("res.country.adj-mod_tot_unmet.rda")))

        if (age_ratios)
            checkmate::assert_file_exists(file.path(output_folder_path, c("res.country.age.ratio.rda")))

    } else {

        ## -------** All Women

        if (post_processed)
            checkmate::assert_file_exists(file.path(output_folder_path, c("combine_runs_args.RData",
                                                                          "res.aggregate.all.women.rda",
                                                                          "res.country.all.women.rda")))
        if (countrytrajectories)
            checkmate::assert_directory_exists(file.path(output_folder_path, c("aggregatetrajectories")))

        if (adjusted_medians)
            checkmate::assert_file_exists(file.path(output_folder_path, c("res.country.all.women.adj-mod_tot_unmet.rda")))

        if (age_ratios)
            checkmate::assert_file_exists(file.path(output_folder_path, c("res.country.all.women.age.ratio.rda")))
    }

    ## RETURN
    if (verbose) message("'", output_folder_path, "' is a valid output directory.")
    return(invisible(output_folder_path))
}
