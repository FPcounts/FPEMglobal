################################################################################
###
### DATE CREATED: 2025-02-20
###
### AUTHOR: Mark Wheldon
###
### PROJECT: FPEMglobal
###
### DESCRIPTION: General helpers for testthat tests.
###
###-----------------------------------------------------------------------------
###
### IMPORTANT: All functions should begin with `test_help_`.
###
################################################################################

###-----------------------------------------------------------------------------
### * Temp Directories


##' Set up temp directories for testing 'copy_csv_data_files()'
##'
##' @param temp_dir Session temp directory. You must create this using
##'     \code{withr::local_tempdir()} in the body of the function calling this
##'     function.
##' @param test_run_path String.
##' @param input_subdir_path Path relative to `tempdir`.
##' @param output_subdir_path Path relative to `tempdir`
##' @param input_csv_file_name String.
##' @return A list with paths and file names for use in the test.
##' @author Mark C Wheldon
##' @noRd
test_help_copy_csv_data_files_temp <-
    function(temp_dir, test_run_name,
             input_subdir_path = "input",
             output_subdir_path = file.path("output", test_run_name, "data"),
             input_csv_file_name = "input.csv") {

        input_path <- file.path(temp_dir, "input")
        output_path <- file.path(temp_dir, "output", test_run_name, "data")

        stopifnot(dir.create(input_path))
        stopifnot(dir.create(output_path, recursive = TRUE))

        input_csv_file_path <- file.path(input_path, "input.csv")
        stopifnot(file.create(input_csv_file_path))
        expect_no_error(writeLines("COL1,COL2\na,b", input_csv_file_path))

        return(list(input_path = input_path,
                    output_path = output_path,
                    input_csv_file_path = input_csv_file_path))
    }
