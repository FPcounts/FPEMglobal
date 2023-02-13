################################################################################
###
### PACKAGE 'cuaw'
###
### TEST 15-49 and 15-19
###
################################################################################

###-----------------------------------------------------------------------------
### * Package

library(FPEMglobal)

###-----------------------------------------------------------------------------
### * Common Arguments

###-----------------------------------------------------------------------------
### ** Run names

run_name_override_married_1549 <- "test_1549_married"
run_name_override_unmarried_1549 <- "test_1549_unmarried"
run_name_override_all_women_1549 <- "test_1549_all_women"

run_name_override_1519 <- "test2"
run_name_override_married_1519 <- "test_1519_married"
run_name_override_unmarried_1519 <- "test_1519_unmarried"
run_name_override_all_women_1519 <- "test_1519_all_women"

age_ratios_age_total_run_name_prefix <- "test"
age_ratios_age_total_married_output_folder_path <-
    file.path("output", paste0(run_name_override_married_1549))
age_ratios_age_total_unmarried_output_folder_path <-
    file.path("output", paste0(run_name_override_unmarried_1549))
age_ratios_age_total_all_women_output_folder_path <-
    file.path("output", paste0(run_name_override_all_women_1549))

###-----------------------------------------------------------------------------
### ** MCMC parameters

set.seed(1)

chain_nums <- 1:2#1:6

burn_in_iterations <- 1#2e4
estimation_iterations <- 2#ceiling(5e5 / nchains)
steps_before_progress_report <- 1#4
thinning <- 1#30
run_in_parallel <- TRUE

include_AR <- FALSE

###-----------------------------------------------------------------------------
### ** Inputs

data_csv_filename_1549 <- "data_cp_model_all_women_SHORTFORTESTING_15-49.csv"
data_csv_filename_1519 <- "data_cp_model_all_women_SHORTFORTESTING_15-19.csv"

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
special_aggregates_name <- "WHO_regions"
model_diagnostics <- TRUE
make_age_ratios <- TRUE
verbose <- FALSE

###-----------------------------------------------------------------------------
### * TEST RUNS

###-----------------------------------------------------------------------------
### ** 15-49 (ALL WRA)

###-----------------------------------------------------------------------------
### *** Initial Run

all_women_1549_runs <-
    do_global_all_women_run(
        run_name_override_married = run_name_override_married_1549,
        run_name_override_unmarried = run_name_override_unmarried_1549,
        run_name_override_all_women = run_name_override_all_women_1549,
        age_group = "15-49",
        ## MCMC parameters
        estimation_iterations = estimation_iterations,
        burn_in_iterations = burn_in_iterations,
        steps_before_progress_report = steps_before_progress_report,
        thinning = thinning,
        chain_nums = chain_nums,
        run_in_parallel = run_in_parallel,
        set_seed_chains = 1,
        ## Inputs
        data_csv_filename = data_csv_filename_1549,
        special_aggregates_name = special_aggregates_name,
        ## Outputs
        years_change = years_change,
        years_change2 = years_change2,
        plot_maps_shapefile_folder = plot_maps_shapefile_folder,
        plot_maps_years = plot_maps_years,
        adjust_medians = adjust_medians,
        age_ratios_age_total_run_name_prefix = age_ratios_age_total_run_name_prefix,
        ## Advanced
        verbose = verbose,
        include_AR = include_AR
    )

all_women_1549_runs

###-----------------------------------------------------------------------------
### *** Add a Chain

add_global_mcmc(all_women_1549_runs$married_run_name,
                chain_nums = max(chain_nums) + 1,
                verbose = verbose)

###-----------------------------------------------------------------------------
### ** 15-19 (Adolescent Women)

###-----------------------------------------------------------------------------
### *** Base Functions

###-----------------------------------------------------------------------------
### **** Married Women

###
### MCMC

do_global_mcmc(run_name_override = run_name_override_married_1519,
               marital_group = "married",
               age_group = "15-19",
               ## MCMC parameters
               estimation_iterations = estimation_iterations,
               burn_in_iterations = burn_in_iterations,
               steps_before_progress_report = steps_before_progress_report,
               thinning = thinning,
               chain_nums = chain_nums,
               run_in_parallel = run_in_parallel,
               set_seed_chains = 1,
               ## Inputs
               data_csv_filename = data_csv_filename_1519,
               include_AR = include_AR)

###
### Post process

post_process_mcmc(run_name = run_name_override_married_1519,
    ## Results
    years_change = years_change,
    years_change2 = years_change2,
    model_diagnostics = model_diagnostics,
    special_aggregates_name = special_aggregates_name,
    age_ratios_age_total_output_folder_path = age_ratios_age_total_married_output_folder_path,
    verbose = verbose)

###
### Make results

make_results(run_name = run_name_override_married_1519,
             plot_maps_shapefile_folder = plot_maps_shapefile_folder,
             plot_maps_years = plot_maps_years,
             adjust_medians = adjust_medians,
             special_aggregates_name = special_aggregates_name,
             make_age_ratios = make_age_ratios,
             verbose = verbose)

###-----------------------------------------------------------------------------
### **** Unmarried Women

###
### MCMC

do_global_mcmc(run_name_override = run_name_override_unmarried_1519,
               marital_group = "unmarried",
               age_group = "15-19",
               ## MCMC parameters
               estimation_iterations = estimation_iterations,
               burn_in_iterations = burn_in_iterations,
               steps_before_progress_report = steps_before_progress_report,
               thinning = thinning,
               chain_nums = chain_nums,
               run_in_parallel = run_in_parallel,
               set_seed_chains = 1,
               ## Inputs
               data_csv_filename = data_csv_filename_1519,
               include_AR = include_AR)

###
### Post process

post_process_mcmc(run_name = run_name_override_unmarried_1519,
    ## Results
    years_change = years_change,
    years_change2 = years_change2,
    model_diagnostics = model_diagnostics,
    special_aggregates_name = special_aggregates_name,
    age_ratios_age_total_output_folder_path = age_ratios_age_total_unmarried_output_folder_path,
    verbose = verbose)

###
### Make results

make_results(run_name = run_name_override_unmarried_1519,
             plot_maps_shapefile_folder = plot_maps_shapefile_folder,
             plot_maps_years = plot_maps_years,
             adjust_medians = adjust_medians,
             special_aggregates_name = special_aggregates_name,
             make_age_ratios = make_age_ratios,
             verbose = verbose)

###-----------------------------------------------------------------------------
### **** Combine

combine_runs(married_women_run_name = run_name_override_married_1519,
             unmarried_women_run_name = run_name_override_unmarried_1519,
             run_name_override = run_name_override_all_women_1519,
             ## Results
             years_change = years_change,
             years_change2 = years_change2,
             special_aggregates_name = special_aggregates_name,
             adjust_medians = adjust_medians,
             age_ratios_age_total_unmarried_output_folder_path = age_ratios_age_total_unmarried_output_folder_path,
             age_ratios_age_total_married_output_folder_path = age_ratios_age_total_married_output_folder_path,
             age_ratios_age_total_all_women_output_folder_path = age_ratios_age_total_all_women_output_folder_path,
             verbose = FALSE)

###-----------------------------------------------------------------------------
### **** Make Results

make_results(run_name = run_name_override_all_women_1519,
             plot_maps_shapefile_folder = plot_maps_shapefile_folder,
             plot_maps_years = plot_maps_years,
             adjust_medians = adjust_medians,
             special_aggregates_name = special_aggregates_name,
             make_age_ratios = make_age_ratios,
             verbose = verbose)

###-----------------------------------------------------------------------------
### **** Make Results again for MW and UW

make_results(run_name = run_name_override_married_1519,
             adjust_medians = adjust_medians,
             special_aggregates_name = special_aggregates_name,
             make_age_ratios = make_age_ratios,
             verbose = verbose)

make_results(run_name = run_name_override_unmarried_1519,
             adjust_medians = adjust_medians,
             special_aggregates_name = special_aggregates_name,
             make_age_ratios = make_age_ratios,
             verbose = verbose)
