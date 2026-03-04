# FPEMglobal 1.6.0 [Upcoming]

* The arguments specifying run names and output directories of the main user interface functions have been simplified. All now take only `run_name` and `output_dir_path` to indicate, respectively, a unique name for a given model run and the top-level directory in which outputs are saved. Once a run is complete, the directory `output_dir_path` will contain a subdirectory named `run_name`, where `run_name` uniquely identifies  marital-age-group specific run.
* Auto-generated run names are based only on the date, not the date-time.  
* Argument `steps_before_progress_report` in the `do_...` functions renamed to `number_incremental_backups`, which better describes what it actually does. 


# FPEMglobal 1.5.4

* Patch to correct erroneous file paths for results tables when `adjust_medians = TRUE`.


# FPEMglobal 1.2.0

* New function `compare_runs_CI_plots()`, a wrapper for `PlotComparison()`.
* New function `validate_input_file()`. Can be run on candidate input .csv files to check they are valid.
* Argument `countries_in_CI_plots_csv_filename` added to `combine_runs()`.
* Package "doMC" removed from the "Suggests" field of DESCRIPTION.
* Obs with non-missing 'Unmet' no longer excluded if CP values are missing. This was either and obsolete omission rule or a bug.
* Buf fixes and improvements to messages and input file processing.


# FPEMglobal 1.1.0

* Estimates and projections can now be produced for adolescent women aged 15--19. See the vignette "FPEMglobal_Age_1519".
* Auto-generated run-names are slighlty different; age group is before marital group.
* User functions produce some additional plots and diagnostic outputs.
* Stability improvements.
