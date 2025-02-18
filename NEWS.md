# FPEMglobal (development version)

# FPEMglobal 1.5.3

* Updated Regional Commission aggregates with versions released 2024-12-19.
  These have a small number of new groups and are used in SDG reporting in 2025.

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
