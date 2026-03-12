################################################################################
###
### PACKAGE 'FPEMglobal'
###
### TEST 15-49 and 15-19
###
################################################################################

###-----------------------------------------------------------------------------
### * Package

library(FPEMglobal)
options(FPEMglobal.verbose = TRUE)

### TEMP DIRECTORY
(tmpdir <- tempdir())
(owd <- setwd(tmpdir)) # 'owd' is the *o*ld *w*orking *d*irectory

###-----------------------------------------------------------------------------
### * Common Arguments

###-----------------------------------------------------------------------------
### ** Run names

run_name_married_1549 <- "test_1549_married"
run_name_unmarried_1549 <- "test_1549_unmarried"
run_name_all_women_1549 <- "test_1549_all_women"

run_name_1519 <- "test_1519"
run_name_married_1519 <- "test_1519_married"
run_name_unmarried_1519 <- "test_1519_unmarried"
run_name_all_women_1519 <- "test_1519_all_women"

output_dir_path <- file.path(getwd(), "test_run_output")
output_dir_path_omnibus <- file.path(getwd(), "test_run_output_omnibus")

age_ratios_age_total_run_name_prefix <- "test"
age_ratios_age_total_married_run_dir_path <-
    file.path(output_dir_path, paste0(run_name_married_1549))
age_ratios_age_total_unmarried_run_dir_path <-
    file.path(output_dir_path, paste0(run_name_unmarried_1549))
age_ratios_age_total_all_women_run_dir_path <-
    file.path(output_dir_path, paste0(run_name_all_women_1549))

###-----------------------------------------------------------------------------
### ** MCMC parameters

set.seed(1)

chain_nums <- 1:2 #1:6

burn_in_iterations <- 1 #2e4
estimation_iterations <- 2 #ceiling(5e5 / nchains)
number_incremental_backups <- 1 #4
thinning <- 1 #30
run_in_parallel <- TRUE

include_AR <- FALSE

###-----------------------------------------------------------------------------
### ** Inputs

data_csv_filename_1549 <- "data_cp_model_all_women_SHORTFORTESTING_15-49.csv"
data_csv_filename_1519 <- "OLD_data_cp_model_all_women_SHORTFORTESTING_15-19.csv"

denominator_counts_csv_filename_1549 <- "number_of_women_15-49.csv"
denominator_counts_csv_filename_1519 <- "OLD_number_of_women_15-19--testing-only.csv"

###-----------------------------------------------------------------------------
### ** Outputs

years_change <- matrix(c(1990.5, 2000.5,
                        2000.5, 2019.5,
                        2019.5, 2030.5,
                        2012.5, 2019.5,
                        2012.5, 2020.5), ncol = 2,
                       byrow = TRUE)
years_change2 <- matrix(c(2005.5, 2010.5, 2015.5,
                         2000.5, 2005.5, 2010.5,
                         1995.5, 2000.5, 2005.5,
                         1990.5, 1995.5, 2000.5,
                         1990.5, 2000.5, 2010.5,
                         2000.5, 2010.5, 2020.5),
                        ncol = 3, byrow = TRUE)

adjust_medians <- TRUE
plot_maps_shapefile_folder <- NULL
plot_maps_years <- NULL
special_aggregates_name <- "OLD_WHO_regions"
model_diagnostics <- TRUE
make_age_ratios <- TRUE

###-----------------------------------------------------------------------------
### * TEST RUNS

###-----------------------------------------------------------------------------
### ** 15-49 (ALL WRA)

###-----------------------------------------------------------------------------
### *** Base Functions

###-----------------------------------------------------------------------------
### **** Married Women

###
### MCMC

(mcmc_1549_married <-
     do_global_mcmc(marital_group = "married",
                    age_group = "15-49",
                    ## MCMC Params
                    estimation_iterations = estimation_iterations,
                    burn_in_iterations = burn_in_iterations,
                    number_incremental_backups = number_incremental_backups,
                    thinning = thinning,
                    chain_nums = chain_nums,
                    run_in_parallel = run_in_parallel,
                    set_seed_chains = 1,
                    include_AR = include_AR,
                    ## Files and Directories
                    run_name = NULL,
                    output_dir_path = output_dir_path,
                    data_csv_filename = data_csv_filename_1549))

dir(output_dir_path)

###
### Add a Chain

add_global_mcmc(run_name = mcmc_1549_married[["run_name"]],
                output_dir_path = mcmc_1549_married[["output_dir_path"]],
                chain_nums = max(chain_nums) + 1)

###
### Post process

(pp_mcmc_1549_married <-
     post_process_mcmc(run_name = mcmc_1549_married[["run_name"]],
                       output_dir_path = mcmc_1549_married[["output_dir_path"]],
                       ## Inputs
                       denominator_counts_csv_filename = denominator_counts_csv_filename_1549,
                       ## Results
                       years_change = years_change,
                       years_change2 = years_change2,
                       model_diagnostics = model_diagnostics,
                       special_aggregates_name = special_aggregates_name))

dir(output_dir_path)

###
### Make results

(res_1549_married <-
    make_results(run_name = pp_mcmc_1549_married[["run_name"]],
             output_dir_path = pp_mcmc_1549_married[["output_dir_path"]],
             plot_maps_shapefile_folder = plot_maps_shapefile_folder,
             plot_maps_years = plot_maps_years,
             adjust_medians = adjust_medians,
             special_aggregates_name = special_aggregates_name))

dir(output_dir_path)

###-----------------------------------------------------------------------------
### **** Unmarried Women

###
### MCMC

(res_1549_unmarried <-
    do_global_run(run_name = NULL,
               marital_group = "unmarried",
               age_group = "15-49",
               output_dir_path = output_dir_path,
               ## MCMC parameters
               estimation_iterations = estimation_iterations,
               burn_in_iterations = burn_in_iterations,
               number_incremental_backups = number_incremental_backups,
               thinning = thinning,
               chain_nums = chain_nums + 1,
               run_in_parallel = run_in_parallel,
               set_seed_chains = 1,
               include_AR = include_AR,
               ## Inputs
               data_csv_filename = data_csv_filename_1549,
               special_aggregates_name = special_aggregates_name))

dir(output_dir_path)

###-----------------------------------------------------------------------------
### **** All Women

###
### Combine Runs

(combine_1549 <-
    combine_runs(married_women_run_name = res_1549_married[["run_name"]],
             unmarried_women_run_name = res_1549_unmarried[["run_name"]],
             run_name = NULL,
             output_dir_path = output_dir_path,
             ## Inputs
             denominator_counts_csv_filename = denominator_counts_csv_filename_1549,
             ## Results
             years_change = years_change,
             years_change2 = years_change2,
             special_aggregates_name = special_aggregates_name,
             adjust_medians = adjust_medians))

dir(output_dir_path)

###
### Make Results

(res_all_women_1549 <-
     make_results(run_name = combine_1549[["run_name"]],
                  output_dir_path = combine_1549[["output_dir_path"]],
             plot_maps_shapefile_folder = plot_maps_shapefile_folder,
             plot_maps_years = plot_maps_years,
             adjust_medians = adjust_medians,
             special_aggregates_name = special_aggregates_name))

dir(output_dir_path)

###-----------------------------------------------------------------------------
### *** Omnibus Run

(all_women_1549_runs <-
    do_global_all_women_run(
        run_name = list(married = run_name_married_1549,
                        unmarried = run_name_unmarried_1549,
                        all_women = run_name_all_women_1549),
        output_dir_path = output_dir_path_omnibus,
        age_group = "15-49",
        ## MCMC parameters
        estimation_iterations = estimation_iterations,
        burn_in_iterations = burn_in_iterations,
        number_incremental_backups = number_incremental_backups,
        thinning = thinning,
        chain_nums = chain_nums,
        run_in_parallel = run_in_parallel,
        set_seed_chains = 1,
        include_AR = include_AR,
        ## Inputs
        data_csv_filename = data_csv_filename_1549,
        denominator_counts_csv_filename = denominator_counts_csv_filename_1549,
        special_aggregates_name = special_aggregates_name,
        ## Outputs
        years_change = years_change,
        years_change2 = years_change2,
        plot_maps_shapefile_folder = plot_maps_shapefile_folder,
        plot_maps_years = plot_maps_years,
        adjust_medians = adjust_medians,
        age_ratios_age_total_run_name_prefix = age_ratios_age_total_run_name_prefix
    ))

dir(output_dir_path_omnibus)

###-----------------------------------------------------------------------------
### ** 15-19 (Adolescent Women)

###-----------------------------------------------------------------------------
### *** Omnibus Run

(all_women_1519_runs <-
    do_global_all_women_run(
        run_name = list(married = run_name_married_1519,
                        unmarried = run_name_unmarried_1519,
                        all_women = run_name_all_women_1519),
        output_dir_path = output_dir_path_omnibus,
        age_group = "15-19",
        ## MCMC parameters
        estimation_iterations = estimation_iterations,
        burn_in_iterations = burn_in_iterations,
        number_incremental_backups = number_incremental_backups,
        thinning = thinning,
        chain_nums = chain_nums,
        run_in_parallel = run_in_parallel,
        set_seed_chains = 1,
        include_AR = include_AR,
        ## Inputs
        data_csv_filename = data_csv_filename_1519,
        denominator_counts_csv_filename = denominator_counts_csv_filename_1519,
        special_aggregates_name = special_aggregates_name,
        ## Outputs
        years_change = years_change,
        years_change2 = years_change2,
        plot_maps_shapefile_folder = plot_maps_shapefile_folder,
        plot_maps_years = plot_maps_years,
        adjust_medians = adjust_medians,
        age_ratios_age_total_run_name_prefix = age_ratios_age_total_run_name_prefix,
        age_ratios_age_total_married_run_name = run_name_married_1549,
        age_ratios_age_total_unmarried_run_name = run_name_unmarried_1549,
        age_ratios_age_total_all_women_run_name = run_name_all_women_1549
    ))

dir(output_dir_path_omnibus)

###-----------------------------------------------------------------------------
### * Validation

###-----------------------------------------------------------------------------
### ** Base Function

###
### MCMC

(valid_1549_mcmc <-
    do_global_validation_mcmc(run_name = NULL,
                             output_dir_path = mcmc_1549_married[["output_dir_path"]],
                             run_name_to_validate = mcmc_1549_married[["run_name"]],
                             run_name_to_validate_output_dir_path = mcmc_1549_married[["output_dir_path"]],
             exclude_unmet_only = FALSE,
             exclude_unmet_only_test_prop = 0.2,
             at_random = TRUE, #<<
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
             estimation_iterations = estimation_iterations,
             burn_in_iterations = burn_in_iterations,
             thinning = thinning,
             chain_nums = chain_nums,
             run_in_parallel = run_in_parallel))

dir(output_dir_path)


###
### Post process

(pp_valid_1549_mcmc <-
     post_process_mcmc(run_name = valid_1549_mcmc[["run_name"]],
                       output_dir_path = valid_1549_mcmc[["output_dir_path"]],
                       ## Inputs
                       denominator_counts_csv_filename = denominator_counts_csv_filename_1549,
                       ## Results
                       years_change = years_change,
                       years_change2 = years_change2,
                       model_diagnostics = model_diagnostics,
                       special_aggregates_name = special_aggregates_name))

dir(output_dir_path)


###
### Make results

(res_valid_1549_mcmc <-
     make_results(run_name = pp_valid_1549_mcmc[["run_name"]],
                  output_dir_path = pp_valid_1549_mcmc[["output_dir_path"]],
             plot_maps_shapefile_folder = plot_maps_shapefile_folder,
             plot_maps_years = plot_maps_years,
             adjust_medians = adjust_medians,
             special_aggregates_name = special_aggregates_name))

dir(output_dir_path)

###-----------------------------------------------------------------------------
### ** Full validation

(valid_1519_mcmc <-
     do_global_validation_run(run_name = NULL,
                              output_dir_path = output_dir_path_omnibus,
                              run_name_to_validate = all_women_1519_runs[["married"]][["run_name"]],
                              run_name_to_validate_output_dir_path = all_women_1519_runs[["married"]][["output_dir_path"]],
             exclude_unmet_only = FALSE,
             exclude_unmet_only_test_prop = 0.2,
             at_random = FALSE,
             at_random_min_c = 1,
             at_random_test_prop = 0.2,
             at_end = TRUE, #<<
             at_end_not_1_obs_c = FALSE,
             at_random_no_data = FALSE,
             at_random_no_data_strata = NULL,
             at_random_no_data_test_prop = 0.2,
             leave_iso_out = FALSE,
             leave_iso_out_iso_test = NULL,
             year_cutoff = 2005,
             seed_validation = 12345,
             generate_new_set = TRUE,
             estimation_iterations = estimation_iterations,
             burn_in_iterations = burn_in_iterations,
             thinning = thinning,
             chain_nums = chain_nums,
             run_in_parallel = run_in_parallel))

dir(output_dir_path_omnibus)

###-----------------------------------------------------------------------------
### * TEST RENAME

new_run_names <-
    mapply(function(y, z) rename_global_run(run_name = y[["run_name"]],
                                            new_run_name = z[["run_name"]],
                                            output_dir_path = y[["output_dir_path"]]),
           all_women_1549_runs,
           rapply(all_women_1549_runs, function(z) paste0(z, "_RENAMED"),
                  how = "replace"))

###-----------------------------------------------------------------------------
### * END

### RESET DIRECTORY
setwd(owd)
