################################################################################
###
### DATE CREATED: 2025-02-20
###
### AUTHOR: Mark Wheldon
###
### PROJECT: FPEMglobal
###
### DESCRIPTION: Test functions in 'user_interface_utils.R'
###
###-----------------------------------------------------------------------------
###
################################################################################

###-----------------------------------------------------------------------------
### * Validation

test_that("`validate_extra_config()` works", {

    ## Null inputs
    expect_no_error(validate_extra_config(list()))

    ## Basic error: list missing some elements
    expect_error(validate_extra_config(list(one_country_run = TRUE)),
                 "Assertion on '\\.extra_config")
})


###-----------------------------------------------------------------------------
### * Copying Files

test_that("`copy_csv_data_files()` works (without '...')", {

    ## Set up
    test_run_name <- "run_test"
    temp_dir <- withr::local_tempdir()
    paths_list <-
        test_help_copy_csv_data_files_temp(temp_dir = temp_dir,
                                           test_run_name = test_run_name)

    ## Test
    expect_no_error(copy_csv_data_files(run_name = test_run_name,
                                        from_dir = paths_list$input_path,
                                        to_dir = paths_list$output_path))
    expect_true(file.exists(file.path(paths_list$output_path, "input.csv")))
})


test_that("`copy_csv_data_files()` works with '...')", {

    ## -------* Set Up

    ## -------** Set up basics

    test_run_name <- "run_test"
    temp_dir <- withr::local_tempdir()
    paths_list <-
        test_help_copy_csv_data_files_temp(temp_dir = temp_dir,
                                           test_run_name = test_run_name)

    ## -------** Global Run

    ## Set up the global run paths
    global_run_output_folder_path <- file.path(temp_dir, "global_run")
    global_run_output_data_folder_path <- file.path(global_run_output_folder_path, "data")
    stopifnot(dir.create(global_run_output_data_folder_path, recursive = TRUE))

    ## Must have the global run summary file
    global_run_summary_file_path <-
        make_global_summary_file_external_path(global_run_output_folder_path, check = FALSE)
    nothing <- "0"
    expect_no_error(save(list = "nothing", file = global_run_summary_file_path))

    ## Create the global aux data .csv file
    global_run_csv_file_name <- "global_input.csv"
    global_run_csv_file_path <- file.path(global_run_output_data_folder_path, global_run_csv_file_name)
    stopifnot(file.create(global_run_csv_file_path))

    expect_no_error(writeLines("COL1,COL2\nx,y", global_run_csv_file_path))

    ## -------* Do the Tests

    ## -------** Simple Test

    expect_no_error(copy_csv_data_files(run_name = test_run_name,
                                        from_dir = paths_list$input_path,
                                        to_dir = paths_list$output_path,
                                        one_country_run = TRUE,
                                        use_global_run_data_files = TRUE,
                                        global_run_output_folder_path = global_run_output_folder_path
                                        ))
    expect_true(file.exists(file.path(paths_list$output_path, global_run_csv_file_name)))

    ## -------** Test with Duplicate Files

    ## Make duplicate file
    duplicate_csv_file_path <- file.path(paths_list$input_path, "global_input.csv")
    stopifnot(file.create(duplicate_csv_file_path))
    expect_no_error(writeLines("COL1,COL2\ni,j", duplicate_csv_file_path))

    ## Test
    expect_error(copy_csv_data_files(run_name = test_run_name,
                        from_dir = paths_list$input_path,
                        to_dir = paths_list$output_path,
                        one_country_run = TRUE,
                        use_global_run_data_files = TRUE,
                        global_run_output_folder_path = global_run_output_folder_path
                        ),
                 "'use_global_run_data_files' is 'TRUE' but the local input directory")
})
