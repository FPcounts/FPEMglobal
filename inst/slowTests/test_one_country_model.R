################################################################################
###
### PACKAGE 'FPEMglobal'
###
### TEST One-country model
###
################################################################################

###-----------------------------------------------------------------------------
### * Package

library(FPEMglobal)

### TEMP DIRECTORY
owd <- getwd()
(tmpdir <- tempdir())
setwd(tmpdir)

##### TEMPORARILY USE LOCAL DIR
setwd(owd)

###-----------------------------------------------------------------------------
### * Common Arguments

###-----------------------------------------------------------------------------
### ** Run names

run_name_override_married_1549 <- "test_1c_1549_married"
run_name_override_unmarried_1549 <- "test_1c_1549_unmarried"
run_name_override_all_women_1549 <- "test_1c_1549_all_women"

run_name_override_1519 <- "test_1c_2"
run_name_override_married_1519 <- "test_1c_1519_married"
run_name_override_unmarried_1519 <- "test_1c_1519_unmarried"
run_name_override_all_women_1519 <- "test_1c_1519_all_women"

age_ratios_age_total_run_name_prefix <- "test_1c"
age_ratios_age_total_married_output_folder_path <-
    file.path("output", paste0(run_name_override_married_1549))
age_ratios_age_total_unmarried_output_folder_path <-
    file.path("output", paste0(run_name_override_unmarried_1549))
age_ratios_age_total_all_women_output_folder_path <-
    file.path("output", paste0(run_name_override_all_women_1549))

###-----------------------------------------------------------------------------
### ** MCMC parameters

set.seed(1)

chain_nums <- 1:2 #1:6

burn_in_iterations <- 1#2e4
estimation_iterations <- 2#ceiling(5e5 / nchains)
steps_before_progress_report <- 1 #4
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
verbose <- FALSE

###-----------------------------------------------------------------------------
### * TEST RUNS

###-----------------------------------------------------------------------------
### ** 15-49 (ALL WRA)

###-----------------------------------------------------------------------------
### *** Full Run

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
        denominator_counts_csv_filename = denominator_counts_csv_filename_1549,
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
