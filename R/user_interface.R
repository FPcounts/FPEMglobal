################################################################################
###
### Separate mcmc and summary functions.
###
################################################################################


##' Generate MCMC chains for global run of FPEM
##'
##' This function generates an MCMC sample for a global run of FPEM for a single
##' marital group. No post-processing or results generation is done; only the
##' chains are produced. The recommended way to use this function is via a call
##' to \code{\link{do_global_run}} or \code{\link{do_global_all_women_run}}.
##'
##' See \dQuote{Details} in the help file for \code{\link{do_global_all_women_run}}.
##'
##' @param run_desc Character. Brief note to be appended to the auto-generated
##'     \code{run_name}. Ignored if \code{run_name_override} is non-\code{NULL}.
##' @param run_name_override Character. User defined run name to override
##'     default generation. \code{run_desc} is ignored if this is
##'     non-\code{NULL}.
##' @param marital_group Character. The marital group for which a run of the
##'     model is desired.
##' @param age_group Character. The age group for which a run of the model is
##'     desired, specified in the format \dQuote{xx-yy}, where \dQuote{xx} is
##'     the start age, \dQuote{yy} is the end age, e.g., \code{"15-49"} or
##'     \code{"15-19"}. This is used to form the run name if
##'     \code{run_name_override} is \code{NULL}, to name of the file containing
##'     prevalence data if \code{data_csv_filename} is \code{NULL}, and to
##'     select rows from the denominator counts file if post-processing is done
##'     (see \code{\link{post_process_mcmc}}). Only "15-49" is currently supported.
##' @param estimation_iterations Numeric. Number of MCMC iterations that should
##'     be \emph{saved}. This is \emph{before} \code{thinning}.
##' @param burn_in_iterations Numeric. Number of MCMC iterations that should be
##'     run as burn-in before starting to save them.
##' @param steps_before_progress_report Numeric. The number of times progress
##'     should reported during MCMC sampling.
##' @param thinning Numeric. The actual number of iterations saved is
##'     \eqn{\frac{\code{estimation_iterations}}{\code{thinning}}}{\code{estimation_iterations}/\code{thinning}}.
##' @param chain_nums Numeric. The number of MCMC chains to run, \emph{as a
##'     sequence}. E.g., for three chains use \code{1:3}. You need to run at
##'     least two chains for post-processing to be successful.
##' @param run_in_parallel Logical. Determines if MCMC chains are run in
##'     parallel. Parallel running requires package
##' #ifdef windows
##' \pkg{doParallel}.
##' #endif
##' #ifndef windows
##'
##' \pkg{doMC}.
##' #endif
##' Defaults to serial running if \code{run_in_parallel = TRUE} but the package
##' is not available.
##' @param input_data_folder_path File path to folder containing
##'     \emph{all} input data. If
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
##' @examples vignette("FPEMglobal")
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
                           run_in_parallel = TRUE,
                           input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
                           data_csv_filename = paste0("data_cp_model_all_women_", age_group, ".csv"),
                           region_information_csv_filename = "country_and_area_classification.csv",
                           output_folder_path = NULL,
                           verbose = FALSE) {

    ##---------------------------------------------------------------------
    ## Make paths to input data

    if(!is.null(input_data_folder_path)) {
        data_csv_filename <- file.path(input_data_folder_path, data_csv_filename)
        region_information_csv_filename <-
            file.path(input_data_folder_path, region_information_csv_filename)
    }
    if(!file.exists(data_csv_filename)) stop("'data_csv_filename' does not exist.")
    if(!file.exists(region_information_csv_filename)) stop("'region_information_csv_filename' does not exist.")

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
    ## Run name and output paths

    marital_group <- match.arg(marital_group)

    if(!is.null(run_name_override)) run_name <- run_name_override
    else run_name <- make_run_name(marital_group, age_group, run_desc)

    message("This run has 'run_name': ", run_name)

    if(is.null(output_folder_path)) output_folder_path <- file.path("output", run_name)

    if (!dir.exists(output_folder_path)) {
        dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)
    }

    ##---------------------------------------------------------------------
    ## Marital group specific arguments

    marital_group_param_set <-
        marital_age_group_param_defaults(marital_group, age_group, model_family = "rate",
                                         model_name = NULL)

    ##---------------------------------------------------------------------
    ## Save the values of function arguments so same arguments can be used for validations

    global_mcmc_args <- c(mget(names(formals(do_global_mcmc))), marital_group_param_set)
    save(global_mcmc_args, file = file.path(output_folder_path, "global_mcmc_args.RData"))

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
        include.AR = TRUE,
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

    ##-----------------------------------------------------------------------------
    ## Copy to Output

    output_data_folder_path <- file.path(output_folder_path, "data")
    copy_data_files(run_name = run_name, data_dir = input_data_folder_path,
                    data_local = output_data_folder_path)

    return(invisible(run_name))

}


##' Add an MCMC chain to a global run of FPEM
##'
##' At least two chains are required to post-process the results of a run of the
##' FPEM model. Multiple chains can be run by \code{\link{do_global_mcmc}} but if additional chains are desired
##' this function can be used to add them. There must be at least one chain
##' already started (identified via \code{run_name}).
##'
##' @param run_name The name of the run to add a chain to.
##' @param chain_nums number identifying chains to add. Unlike
##'     \code{\link{do_global_mcmc}}, this can be a scalar. It must \emph{not}
##'     be the number identifying a chain already created. See \dQuote{Details}.
##' @inheritParams do_global_mcmc
##' @return A name for the run is returned invisibly as a character string. MCMC chains are
##'     saved to \file{output_folder_path/temp.JAGSobjects}. They need to
##'     be post-processed with \code{\link{post_process_mcmc}}. The run name
##'     must be passed to \code{\link{post_process_mcmc}} to locate find the
##'     saved chains for processing. Run names for married and unmarried runs
##'     must also be passed to \code{\link{combine_runs}} to generate all
##'     women MCMC results.
##' @author Mark Wheldon, Andrew Tait
##' @examples vignette("FPEMglobal")
##' @export
add_global_mcmc <- function(run_name,
                            chain_nums = 2,
                            output_folder_path = file.path("output", run_name),
                            verbose = FALSE) {
    ## Meta Info
    load(file.path(output_folder_path, "mcmc.meta.rda"))

    if (sum(is.element(chain_nums, mcmc.meta$general$ChainNums))>0){
        chain_nums <- setdiff(chain_nums, mcmc.meta$general$ChainNums)
        if (sum(chain_nums)==0){
            stop("MCMC run(s) for 'chain_nums' = ", chain_nums, " and 'run_name' = ", run_name
                ," already exist(s)!", "\n")
        }
    }
    AddMCMCChain(run.name = run_name
                ,ChainNums = chain_nums
                ,write.model.fun = mcmc.meta$general$write.model.fun
                 )

    return(invisible(run_name))
}

##' Post process MCMC chains from a global run of FPEM
##'
##' MCMC chains from a global or one country run of FPEM (via
##' \code{\link{do_global_mcmc}}) must be
##' post-processed before summary results (tables, plots) can be produced. This
##' function does the post-processing and saves the results in
##' \code{output_folder_path}. You need at least two chains for post-processing
##' to work.
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
##' @section References:
##' UN DESA Statistics Division,
##'     (2017) \emph{Standard Country or Area Codes for Statistical
##'     Use (M49)}. United Nations, Department of Economic and Social
##'     Affairs.  \url{https://unstats.un.org/unsd/methodology/m49/}
##' @param run_name The name of the run to post-process.
##' @param output_folder_path
##' @param input_data_folder_path File path to folder containing
##'     \emph{all} input data. If
##'     \code{NULL} the value is taken from
##'     \code{file.path(output_folder_path, "global_mcmc_args.RData")}
##'     if that file exists, otherwise
##'     \code{file.path(output_folder_path, "data")}.
##' @param denominator_counts_csv_filename Name of the \file{.csv}
##'     file containing estimates and projections of the number of
##'     women by marital status, age, and year. See \dQuote{Details}.
##' @param countries_for_aggregates_csv_filename Name of the
##'     \file{.csv} file listing countries that will be used in
##'     constructing country aggregates.
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
##'     and WAIC be computed?
##' @param special_aggregates_name Character vector of names
##'     (\emph{not} filenames) of any speical aggregates
##'     desired. There must be a corresponding file with name
##'     \file{\code{special_aggregates_name}.csv} in
##'     \code{input_data_folder_path} that defines the special
##'     aggregates. See \dQuote{Details}.
##' @param all_women Logical; is the run an all women run such as the
##'     kind produced by \code{\link{combine_runs}} or
##'     \code{\link{do_global_all_women_run}}? If \code{NULL} an
##'     attempt is made to determine this automatically from
##'     \file{mcmc.meta.rda} in the output folder of the run.
##' @inheritParams do_global_mcmc
##' @return The run name (invisibly). The function is mainly called
##'     for its side effects.
##' @author Mark Wheldon, Andrew Tait
##' @seealso \code{\link{do_global_run}} (which calls this function)
##'     to generate MCMC results for married or unmarried women,
##'     post-process, and produce results all in one call;
##'     \code{\link{combine_runs}} to create all women results from
##'     married and unmarried women runs;
##'     \code{\link{do_global_all_women_run}} to do married,
##'     unmarried, \emph{and all women runs}, and produce results, all
##'     in one call.
##' @examples vignette("FPEMglobal")
##' @export
post_process_mcmc <- function(run_name,
                              output_folder_path = file.path("output", run_name),
                              input_data_folder_path = NULL,
                              denominator_counts_csv_filename = NULL,
                              countries_for_aggregates_csv_filename = "countries_mwra_195.csv",
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
                              plot_years = c(2000, 2010, 2019),
                              model_diagnostics = !dir.exists(file.path(output_folder_path, "convergence")),
                              special_aggregates_name = NULL,
                              all_women = NULL,
                              verbose = FALSE) {

    message("Post-processing run: ", run_name)

    ##----------------------------------------------------------------------------
    ## Meta Info

    load(file.path(output_folder_path, "mcmc.meta.rda"))
    write_model_function <- mcmc.meta$general$write.model.fun

    global_mcmc_args_filepath <- file.path(output_folder_path, "global_mcmc_args.RData")
    if(file.exists(global_mcmc_args_filepath)) load(global_mcmc_args_filepath)

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
    if(is.null(all_women)) all_women <- isTRUE(mcmc.meta$general$all.women.run.copy)

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
    }

    ## Make country trajectories

    if (!file.exists(file.path(output_folder_path, "res.country.rda"))
        || !file.exists(file.path(output_folder_path, "res.aggregate.rda"))
        || !file.exists(file.path(output_folder_path, "par.ciq.rda"))) {

        message("\nConstructing output objects, including standard aggregates.")

        ConstructOutput(
            run.name = run_name,
            WRA.csv = denominator_counts_csv_filename,
            countries.to.include.in.aggregates.csv = countries_for_aggregates_csv_filename,
            output.dir = output_folder_path,
            start.year = start_year,
            end.year = end_year,
            years.change = years_change,
            verbose = verbose
        )
    }

    ## Special aggregates

    if (!is.null(special_aggregates_name)) {
        for (name.agg in special_aggregates_name) {
            if(!is.null(input_data_folder_path)) {
                file.agg <- file.path(input_data_folder_path, paste0(name.agg, ".csv"))
            } else {
                file.agg <- paste0(name.agg, ".csv")
            }

            message("\nMaking aggregates for ", name.agg, " from ", file.agg, ".")

            if(!all_women) {
                res.new <- GetAggregates(run.name = run_name,
                                         output.dir = output_folder_path,
                                         file.aggregates = file.agg,
                                         years.change = years_change,
                                         years.change2 = years_change2,
                                         countries.to.include.in.aggregates.csv = countries_for_aggregates_csv_filename,
                                         verbose = verbose
                                         )

                res.fname <- file.path(output_folder_path, paste0(name.agg, ".rda"))
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
    }

    ## ##----------------------------------------------------------------------------
    ## ## Summarize global run

    ##     message("\nSummarizing global run.")

    ##     SummariseGlobalRun(         #This creates (sufficient?) input for one-country run.
    ##         run.name = run_name,
    ##         output.dir = output_folder_path,
    ##         write.model.fun = write_model_function
    ##     )

    ##----------------------------------------------------------------------------
    ## Check convergence

    if(model_diagnostics) {
        if(dir.exists(file.path(output_folder_path, "convergence"))) {
            warning("Model diagnostics already exist; not re-done. Delete '",
                    file.path(output_folder_path, "convergence"),
                    "' and re-run if you want new ones.")
        } else {

            message("\nChecking convergence.")

            CheckConvergence(
                run.name = run_name,
                output.dir = output_folder_path,
                plot.trace = FALSE,
                use.all.parameters = FALSE,
                check.convergence = TRUE,
                png.traceplots = TRUE,
                fig.dir =file.path(output_folder_path, "convergence"),
                sink.convergence.log = FALSE
            )
        }
    }

    return(invisible(run_name))

}


##' Summarize a post-processed global or one-country run of FPEM
##'
##' Once post-processed, a global or one-country model can be used to generate
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
##' @param plot_CI_changes_years Vector of length two (if longer, only
##'     the first and last elements are used). Declares the years to
##'     be used to make the \dQuote{fish bone} plots, i.e., the plots
##'     appearing in files
##'     \file{\code{output_folder_path}/fig/\var{[run
##'     name]}_CIspropsubregional_modern_UNPD.pdf}. These must be in
##'     the range of \code{start_year} and \code{end_year} passed to
##'     \code{\link{post_process_mcmc}}.
##' @param countries_in_CI_plots_csv_filename Name of \file{.csv} file
##'     that lists the countries to be included in the main
##'     country-level indicator plots. These are the plots saved in
##'     \file{\code{output_folder_path}/fig/\var{[run
##'     name]}CIs.pdf}. The format is the same as
##'     \code{countries_for_aggregates_csv_filename}. The file is
##'     looked for in \code{input_data_folder_path}. Countries appear
##'     in the \file{.pdf} in the same order as they are listed in
##'     \code{countries_in_CI_plots_csv_filename}.
##' @param adjust_medians Logical. Should adjusted medians outputs be
##'     produced in addition to unadjusted outputs?
##' @param special_aggregates_name \emph{name} for special aggregates,
##'     if any. The default, \code{NULL}, means no special aggregates
##'     are produced. Note: this is \emph{not} a filename, although a
##'     corresponding file named
##'     \file{\code{special_aggregates_name}.csv} must be present in
##'     \code{input_data_folder_path}.
##' @param all_women
##' @param married_women_run_name Run name of a married women
##'     run.
##' @param unmarried_women_run_name Run name of a unmarried women
##'     run. See \code{married_women_run_name}.
##' @param married_women_output_folder_path Path to directory
##'     containing outputs for a married women run. See \code{married_women_run_name}.
##' @param unmarried_women_output_folder_path Path to directory
##'     containing outputs for a unmarried women run. See \code{married_women_output_folder_path}.
##' @inheritParams do_global_mcmc
##' @inheritParams post_process_mcmc
##' @return \code{run_name} invisibly as a character string.
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
##' @examples vignette("FPEMglobal")
##'
##' @export
make_results <- function(run_name,
                         output_folder_path = file.path("output", run_name),
                         input_data_folder_path = NULL,
                         denominator_counts_csv_filename = NULL,
                         countries_for_aggregates_csv_filename = "countries_mwra_195.csv",
                         region_information_csv_filename = "country_and_area_classification.csv",
                         data_info_plot_years = c(1990, 2000, 2010),
                         plot_CI_changes_years = NULL,
                         countries_in_CI_plots_csv_filename = NULL,
                         adjust_medians = FALSE,
                         special_aggregates_name = NULL,
                         all_women = NULL) {

    ##----------------------------------------------------------------------------
    ## Meta information
    ##----------------------------------------------------------------------------

    ## MCMC meta
    load(file.path(output_folder_path, "mcmc.meta.rda"))
    data_raw <- mcmc.meta$data.raw
    unmarried <- identical(mcmc.meta$general$marital.group, "UWRA")

    ## Extra meta information
    global_mcmc_args_filepath <- file.path(output_folder_path, "global_mcmc_args.RData")
    if(file.exists(global_mcmc_args_filepath)) load(global_mcmc_args_filepath)

    if(is.null(input_data_folder_path)) input_data_folder_path <- file.path(output_folder_path, "data")

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

    ## Years to plot
    if(is.null(plot_CI_changes_years)) {
        post_process_args_filepath <- file.path(output_folder_path, "post_process_args.RData")
        if(file.exists(post_process_args_filepath)) {
            load(post_process_args_filepath)
            if(is.null(plot_CI_changes_years)) {
                plot_CI_changes_years <-
                    c(floor(post_process_args$start_year), floor(post_process_args$end_year))
            }
        } else warning("'", post_process_args_filepath, "' does not exist: cannot determine 'plot_CI_changes_years'. Specify them as arguments.")
    }

    ## All women run?
    if(is.null(all_women)) all_women <- isTRUE(mcmc.meta$general$all.women.run.copy)

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

        if(!is.null(special_aggregates_name)) {
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

    ##----------------------------------------------------------------------------
    ## Special aggregates
    ##----------------------------------------------------------------------------

    if (!is.null(special_aggregates_name)) {
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

    ##----------------------------------------------------------------------------
    ## Non-Validation
    ##----------------------------------------------------------------------------

    ##............................................................................
    ## Create plots

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

    PlotResults(all.women = all_women,
                UWRA = unmarried,
                run.name = run_name,
                output.dir = output_folder_path,
                fig.dir = ci_fig_folder_path,
                plot.ind.country.results = FALSE,
                layout.style = "UNPD",
                cex.symbols = list(SS = 1.5, add.info = 4, no.info = 2),
                non.std.symbol = 24,
                select.c.csv = if(!is.null(countries_in_CI_plots_csv_filename)) file.path(input_data_folder_path, countries_in_CI_plots_csv_filename) else NULL,
                all.womenize.fig.name = FALSE,
                hide.CP.tot.lt.1pc = TRUE,
                plot.prior.post = FALSE,
                plot.parameters = FALSE
                )

    if(!is.null(plot_CI_changes_years)) {

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
        message("'plot_CI_changes_years' is 'NULL': No CI changes plots produced.")
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

        GetTablesRes(
            run.name = run_name,
            output.dir = output_folder_path,
            name.res = "UNPDaggregate",
            table.dir = unadjusted_table_folder_path
        )

        GetTablesChange(
            run.name = run_name,
            output.dir = output_folder_path,
            name.res = "UNPDaggregate",
            table.dir = unadjusted_table_folder_path,
            change.in.changes = TRUE
        )

        GetTablesChange(
            run.name = run_name,
            output.dir = output_folder_path,
            name.res = "Country",
            table.dir = unadjusted_table_folder_path,
            change.in.changes = TRUE
        )

    } else {

        GetTablesResAllWomen(
            run.name = run_name,
            output.dir = output_folder_path,
            name.res = "Country",
            table.dir = unadjusted_table_folder_path,
            all.womenize.table.name = FALSE
        )

        GetTablesResAllWomen(
            run.name = run_name,
            output.dir = output_folder_path,
            name.res = "UNPDaggregate",
            table.dir = unadjusted_table_folder_path,
            all.womenize.table.name = FALSE
        )

        GetTablesChangeAllWomen(
            run.name = run_name,
            output.dir = output_folder_path,
            name.res = "UNPDaggregate",
            table.dir = unadjusted_table_folder_path,
            all.womenize.table.name = FALSE
        )

        GetTablesChangeAllWomen(
            run.name = run_name,
            output.dir = output_folder_path,
            name.res = "Country",
            table.dir = unadjusted_table_folder_path,
            all.womenize.table.name = FALSE
        )

    }

    ##............................................................................
    ## Adjust medians

    if(adjust_medians) {

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
                                  plot = TRUE,
                                  plot.dir = compare_adjusted_fig_folder_path)

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
                                  plot = TRUE,
                                  plot.dir = compare_adjusted_fig_folder_path)

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
                                  plot = TRUE,
                                  all.women = all_women,
                                  all.womenize.table.name = FALSE)

                ## Aggregates

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
                                  plot = TRUE,
                                  all.women = all_women,
                                  all.womenize.table.name = FALSE)
            }
        }
    }

    ##............................................................................
    ## Special aggregates

    ## Extra Aggregates

    if (!is.null(special_aggregates_name)) {

        for (name.agg in special_aggregates_name) {

            if(!all_women) {
                file.agg.rda <- file.path(output_folder_path, paste0(name.agg, ".rda"))
                                #^ Produced by 'post_process_mcmc()'
            } else {
                file.agg.rda <- file.path(output_folder_path, paste0(name.agg, ".all.women.rda"))
                                #^ Produced by 'combine_runs()'
            }

            new_agg <- load(file.agg.rda)
            res_new <- get(new_agg)

            message("\nMaking aggregates for ", name.agg, " from ", file.agg.rda, ".")

            if(!all_women) {

                ## Tables
                GetTablesRes(run.name = run_name, res = res_new, name.res = name.agg
                            ,table.dir = unadjusted_table_folder_path)
                GetTablesChange(run.name = run_name, res = res_new, name.res = name.agg
                               ,table.dir = unadjusted_table_folder_path
                               ,change.in.changes = TRUE)

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
                                    ,all.womenize.table.name = FALSE
                                     )
                GetTablesChangeAllWomen(run.name = run_name
                                       ,name.res = name.agg
                                       ,output.dir = output_folder_path
                                       ,table.dir = unadjusted_table_folder_path
                                       ,res = res_new
                                       ,all.womenize.table.name = FALSE
                                        )

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

    return(invisible(run_name))
}


################################################################################
###
### Wrap up do_mcmc, post_process, and make_results
###
################################################################################


##' Run the global FPEM model for married or unmarried women
##'
##' This function generates the MCMC chains using \code{\link{do_global_mcmc}},
##' post-processes them using \code{\link{post_process_mcmc}} and
##' produces plots and tables using \code{\link{make_results}}.
##'
##' See \dQuote{Details} in the help file for \code{\link{do_global_all_women_run}}.
##'
##' @param run_desc
##' @param marital_group
##' @param age_group
##' @param estimation_iterations
##' @param burn_in_iterations
##' @param steps_before_progress_report
##' @param thinning
##' @param chain_nums
##' @param run_in_parallel
##' @param input_data_folder_path
##' @param data_csv_filename
##' @param region_information_csv_filename
##' @param denominator_counts_csv_filename
##' @param countries_for_aggregates_csv_filename
##' @param countries_in_CI_plots_csv_filename
##' @param special_aggregates_name
##' @param output_folder_path
##' @param start_year
##' @param end_year
##' @param years_change
##' @param ncol
##' @param byrow
##' @param years_change2
##' @param ncol
##' @param byrow
##' @param make_any_results Logical. Should tables and plots be produced? If
##'     \code{FALSE}, the arguments that pertain to specific plots or tables are
##'     ineffective.
##' @param plot_CI_changes_years
##' @param adjust_medians
##' @param run_name_override
##' @param model_diagnostics
##' @inheritParams do_global_mcmc
##' @inheritParams post_process_mcmc
##' @inheritParams make_results
##' @return A name for the run, \code{run_name}, returned invisibly as a
##'     character string.
##' @author Mark Wheldon, Andrew Tait
##'
##' @section References:
##' Kettunen, J. et al. (2012) Genome-wide association
##' study identifies multiple loci influencing human serum metabolite
##' levels. Nat Genet advance online publication.
##' \url{http://dx.doi.org/10.1038/ng.1073.}
##'
##' @seealso \code{\link{combine_runs}} to create all women
##'     results from married and unmarried women runs;
##'     \code{\link{do_global_all_women_run}} to do married, unmarried,
##'     \emph{and all women runs}, and produce results, all in one call.
##' @examples vignette("FPEMglobal")
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
                          run_in_parallel = TRUE,
                          input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
                          data_csv_filename = paste0("data_cp_model_all_women_", age_group, ".csv"),
                          region_information_csv_filename = "country_and_area_classification.csv",
                          denominator_counts_csv_filename = paste0("number_of_women_", age_group, ".csv"),
                          countries_for_aggregates_csv_filename = "countries_mwra_195.csv",
                          countries_in_CI_plots_csv_filename = NULL,
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
                          plot_CI_changes_years = c(floor(start_year), floor(end_year)),
                          adjust_medians = FALSE,
                          run_name_override = NULL,
                          model_diagnostics = TRUE,
                          verbose = FALSE) {

    ##-----------------------------------------------------------------------------
    ## Set-up

    marital_group <- match.arg(marital_group)

    if(!is.null(run_name_override)) run_name <- run_name_override
    else run_name <- make_run_name(marital_group, age_group, run_desc)

    if(is.null(output_folder_path)) output_folder_path <- file.path("output", run_name)

    ## Default for 'output_data_folder_path' needs to be defined here because
    ## 'post_process_mcmc' needs it.
    output_data_folder_path <- file.path(output_folder_path, "data")

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
                       run_in_parallel = run_in_parallel,
                       input_data_folder_path = input_data_folder_path,
                       data_csv_filename = data_csv_filename,
                       region_information_csv_filename = region_information_csv_filename,
                       output_folder_path = output_folder_path,
                       verbose = verbose)

    ##-----------------------------------------------------------------------------
    ## Post-Process

    ## Meta Info
    load(file.path(output_folder_path, "mcmc.meta.rda"))

    post_process_mcmc(run_name = run_name,
                      input_data_folder_path = output_data_folder_path,
                      denominator_counts_csv_filename = denominator_counts_csv_filename,
                      countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
                      start_year = start_year,
                      end_year = end_year,
                      years_change = years_change,
                      model_diagnostics = model_diagnostics,
                      special_aggregates_name = special_aggregates_name,
                      verbose = verbose)

    ##-----------------------------------------------------------------------------
    ## Plots, Tables

    if(make_any_results) {

        make_results(run_name = run_name,
                     input_data_folder_path = output_data_folder_path,
                     denominator_counts_csv_filename = denominator_counts_csv_filename,
                     countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
                     region_information_csv_filename = region_information_csv_filename,
                     plot_CI_changes_years = plot_CI_changes_years,
                     countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
                     adjust_medians = adjust_medians,
                     special_aggregates_name = special_aggregates_name)

    }

    ##-----------------------------------------------------------------------------
    ## Finish

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
##' @param run_desc
##' @param married_women_run_name Run name of the married women run to
##'     be combined. Ignored if
##'     \code{married_women_run_output_folder_path} is
##'     \code{NULL}. Otherwise, the output folder will be assumed to
##'     be \code{file.path("output", married_women_run_name)}.
##' @param married_women_run_output_folder_path Path to the folder
##'     containing results for the married women run to be combined.
##' @param unmarried_women_run_output_folder_path Path to the folder
##'     containing results for the unmarried women run to be combined.
##' @param unmarried_women_run_name Same as \code{married_women_run_name} but for the unmarried women run.
##' @param unmarried_women_run_data_folder_path Path to the folder
##'     containing results for the unmarried women run to be
##'     combined. (Only used if \code{special_aggregates_name} is
##'     non-\code{NULL}.
##' @param make_any_aggregates Logical. Should country aggregates of
##'     any kind (including default aggregates) be produced?
##' @param adjust_medians
##' @param run_name_override
##' @param verbose
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
##' vignette("FPEMglobal").
##'
##' @export
combine_runs <- function(## Describe the run
                         run_desc = "",
                         married_women_run_name,
                         married_women_run_output_folder_path = NULL,
                         unmarried_women_run_output_folder_path = NULL,
                         unmarried_women_run_name,
                         unmarried_women_run_data_folder_path = file.path(unmarried_women_run_output_folder_path, "data"),
                         region_information_csv_filename = "country_and_area_classification.csv",
                         special_aggregates_name = NULL,
                         denominator_counts_csv_filename = NULL,
                         countries_for_aggregates_csv_filename = "countries_mwra_195.csv",
                         output_folder_path = NULL,
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
                         plot_years = c(2000, 2010, 2019),
                         make_any_aggregates = TRUE,
                         adjust_medians = FALSE,
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
    }

    if(is.null(unmarried_women_run_output_folder_path)) {
        if(!is.null(unmarried_women_run_name)) {
            unmarried_women_run_output_folder_path <- file.path("output", unmarried_women_run_name)
        } else {
            stop("'unmarried_women_run_output_folder_path' or 'unmarried_women_run_name' must be specified.")
        }
    }

    load(file.path(unmarried_women_run_output_folder_path, "mcmc.meta.rda"))

    ## Age group
    age_group <- mcmc.meta$general$age.group

    ##--------------------------------------------------------------------------
    ## run_name and output folder
    ##--------------------------------------------------------------------------

    if(!is.null(run_name_override)) run_name <- run_name_override
    else run_name <- make_run_name("all_women", age_group, run_desc)

    message("This run has 'run_name': ", run_name, ".")

    ## Make filepaths that need 'age_group'
    if(is.null(denominator_counts_csv_filename)) {
        denominator_counts_csv_filename <-
            paste0("number_of_women_", mcmc.meta$general$age.group, ".csv")
    }

    ## Output Folder
    if(is.null(output_folder_path)) output_folder_path <- file.path("output", run_name)
    dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)

    ## Output Data Folder
    data_folder_path <- file.path(output_folder_path, "data")
    dir.create(data_folder_path, showWarnings = FALSE)

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

    ## Add a label in 'mcmc.meta.rda' to mark this as an all women copy
    load(file.path(output_folder_path, "mcmc.meta.rda"))
                                # The copy of 'mcmc.meta.rda', not the
                                # original from the unmarried women
                                # run.
    mcmc.meta$general$all.women.run.copy <- TRUE
    save(mcmc.meta, file = file.path(output_folder_path, "mcmc.meta.rda"))

    ##--------------------------------------------------------------------------
    ## Construct output for all women
    ##--------------------------------------------------------------------------

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
                            verbose = verbose
                            )

    msg <- paste0("Combined married run from '", married_women_run_output_folder_path,
                  "' with unmarried run from '", unmarried_women_run_output_folder_path, "'.")
    message(msg)
    cat("\n", format(Sys.time(), "%y%m%d_%H%M%S"), ": ",
        msg, file = file.path(output_folder_path, "log.txt"), sep = "", append = TRUE)

    ##--------------------------------------------------------------------------
    ## Adjust medians
    ##--------------------------------------------------------------------------

    if(adjust_medians) {

        adjust_medians_method <- "mod_tot_unmet"

        ## Married
        copy_uwra_mwra_files(makeFileName(paste0("res.country.adj-", adjust_medians_method, ".rda")),
                             output_folder_path,
                             married_women_run_output_folder_path,
                             new_filename = makeFileName(paste0("mwra_", "res.country.adj-",
                                                                adjust_medians_method, ".rda"))
                             )
        copy_uwra_mwra_files(makeFileName(paste0("res.UNPDaggregate.adj-", adjust_medians_method, ".rda")),
                             output_folder_path,
                             married_women_run_output_folder_path,
                             new_filename = makeFileName(paste0("mwra_", "res.UNPDaggregate.adj-",
                                                                adjust_medians_method, ".rda"))
                             )

        ## Unmarried
        copy_uwra_mwra_files(makeFileName(paste0("res.country.adj-", adjust_medians_method, ".rda")),
                             output_folder_path,
                             unmarried_women_run_output_folder_path,
                             new_filename = makeFileName(paste0("uwra_", "res.country.adj-",
                                                                adjust_medians_method, ".rda"))
                             )
        copy_uwra_mwra_files(makeFileName(paste0("res.UNPDaggregate.adj-", adjust_medians_method, ".rda")),
                             output_folder_path,
                             unmarried_women_run_output_folder_path,
                             new_filename = makeFileName(paste0("uwra_", "res.UNPDaggregate.adj-",
                                                                adjust_medians_method, ".rda"))
                             )

        if(!is.null(special_aggregates_name)) {
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

    ##--------------------------------------------------------------------------
    ## Special aggregates
    ##--------------------------------------------------------------------------

    if (!is.null(special_aggregates_name)) {

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
##' See \code{system.file("extdata", "country_and_area_classification.csv")} for
##' how the country classification file should be formatted. Assume all columns
##' are required.
##' @param run_desc
##' @param age_group
##' @param estimation_iterations
##' @param burn_in_iterations
##' @param steps_before_progress_report
##' @param thinning
##' @param chain_nums
##' @param run_in_parallel
##' @param input_data_folder_path
##' @param data_csv_filename
##' @param region_information_csv_filename
##' @param denominator_counts_csv_filename File path. Filepath to \file{.csv}
##'     file with denominator counts (married and unmarried) for this
##'     \code{age_group}. If \code{NULL}, defaults to \code{paste0("women_",
##'     \code{age_group}, ".csv")}.
##' @param countries_for_aggregates_csv_filename
##' @param countries_in_CI_plots_csv_filename
##' @param special_aggregates_name
##' @param start_year
##' @param end_year
##' @param years_change
##' @param ncol
##' @param byrow
##' @param years_change2
##' @param ncol
##' @param byrow
##' @param plot_CI_changes_years
##' @param make_any_aggregates
##' @param adjust_medians
##' @param run_name_override_married
##' @param run_name_override_unmarried
##' @param run_name_override_all_women
##' @param model_diagnostics
##' @inheritParams do_global_mcmc
##' @inheritParams post_process_mcmc
##' @inheritParams make_results
##' @inheritParams combine_global_run
##' @return A \emph{list} of run names for married, unmarried, and all women
##'     runs, returned invisibly. Results are saved to
##'     \file{\code{output_folder_path}}.
##' @author Mark Wheldon, Andrew Tait
##'
##' @seealso \code{\link{do_global_run}} for just married or unmarried women runs.
##'
##' @examples vignette("FPEMglobal")
##' @export
do_global_all_women_run <- function(## Describe the run
                                    run_desc = "",
                                    age_group = "15-49",
                                    estimation_iterations = 3,
                                    burn_in_iterations = 1,
                                    steps_before_progress_report = 4,
                                    thinning = 2,
                                    chain_nums = 1:3,
                                    run_in_parallel = TRUE,
                                    input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
                                    data_csv_filename = paste0("data_cp_model_all_women_",
                                                               age_group, ".csv"),
                                    region_information_csv_filename = "country_and_area_classification.csv",
                                    denominator_counts_csv_filename = paste0("number_of_women_", age_group, ".csv"),
                                    countries_for_aggregates_csv_filename = "countries_mwra_195.csv",
                                    countries_in_CI_plots_csv_filename = NULL,
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
                                    plot_CI_changes_years = c(floor(start_year), floor(end_year)),
                                    make_any_aggregates = TRUE,
                                    adjust_medians = FALSE,
                                    run_name_override_married = NULL,
                                    run_name_override_unmarried = NULL,
                                    run_name_override_all_women = NULL,
                                    model_diagnostics = TRUE,
                                    verbose = FALSE) {

    ##---------------------------------------------------------------------
    ## Run Names with Common Time Stamp

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
            run_in_parallel = run_in_parallel,
            ## Inputs
            input_data_folder_path = input_data_folder_path,
            data_csv_filename = data_csv_filename,
            region_information_csv_filename = region_information_csv_filename,
            denominator_counts_csv_filename = denominator_counts_csv_filename,
            countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
            countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
            special_aggregates_name = special_aggregates_name,
            ## Outputs
            start_year = start_year,
            end_year = end_year,
            years_change = years_change,
            years_change2 = years_change2,
            plot_CI_changes_years = plot_CI_changes_years,
            adjust_medians = adjust_medians,
            ## Advanced
            run_name_override = run_name_override_married,
            model_diagnostics = model_diagnostics,
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
            run_in_parallel = run_in_parallel,
            ## Inputs
            input_data_folder_path = input_data_folder_path,
            data_csv_filename = data_csv_filename,
            region_information_csv_filename = region_information_csv_filename,
            denominator_counts_csv_filename = denominator_counts_csv_filename,
            countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
            countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
            special_aggregates_name = special_aggregates_name,
            ## Outputs
            start_year = start_year,
            end_year = end_year,
            years_change = years_change,
            years_change2 = years_change2,
            plot_CI_changes_years = plot_CI_changes_years,
            adjust_medians = adjust_medians,
            ## Advanced
            run_name_override = run_name_override_unmarried,
            model_diagnostics = model_diagnostics,
            verbose = verbose)

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
            special_aggregates_name = special_aggregates_name,
            ## Outputs
            start_year = start_year,
            end_year = end_year,
            years_change = years_change,
            years_change2 = years_change2,
            make_any_aggregates = make_any_aggregates,
            adjust_medians = adjust_medians,
            ## Advanced
            run_name_override = run_name_override_all_women,
            verbose = verbose)



    make_results(run_name = all_women_run_name,
                 input_data_folder_path = file.path("output", unmarried_run_name, "data"),
                 denominator_counts_csv_filename = denominator_counts_csv_filename,
                 countries_for_aggregates_csv_filename = countries_for_aggregates_csv_filename,
                 region_information_csv_filename = region_information_csv_filename,
                 plot_CI_changes_years = plot_CI_changes_years,
                 countries_in_CI_plots_csv_filename = countries_in_CI_plots_csv_filename,
                 adjust_medians = adjust_medians,
                 special_aggregates_name = special_aggregates_name
                 )

    return(invisible(list(married_run_name = married_run_name,
                          unmarried_run_name = unmarried_run_name,
                          all_women_run_name = all_women_run_name)))
}
