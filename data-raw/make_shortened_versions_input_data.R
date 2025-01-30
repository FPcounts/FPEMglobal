## code to prepare `DATASET` dataset goes here

###-----------------------------------------------------------------------------
### Short Versions of Input Data

## NOTE!! Updating the input data sets? You'll need to re-install the package
## unless you supply the full path to the main function.

## Create shortened versions of input data sets for testing.
shorten_for_testing_df <- function(x) {

  ## Keep all rows with non-missing 'Unmet', maximum three
  x_unmet <- base::by(data = x, INDICES = x$ISO.code,
                      FUN = function(z) {
                        z_unmet <- which(!is.na(z$Unmet))
                        z_unm_ind <- seq_along(z_unmet)
                        if (length(z_unmet)) {
                          z_unm_ind <- unique(floor(seq(from = 1,
                                                        to = length(z_unmet),
                                                        length.out = 3)))
                        }
                        return(z[z_unmet[z_unm_ind], , drop = FALSE])
                      }) |> (function(z) do.call("rbind", z))()

  ## Keep latest obs. and oldest obs.
  latest_obs <- stats::aggregate(Start.year ~ ISO.code + In.union,
                                 data = x,
                                 FUN = function(z) max(z, na.rm = TRUE))
  x_latest <- base::merge(x, latest_obs, all = FALSE)

  oldest_obs <- stats::aggregate(Start.year ~ ISO.code + In.union,
                                 data = x,
                                 FUN = function(z) min(z, na.rm = TRUE))
  x_oldest <- base::merge(x, oldest_obs, all = FALSE)

  ## Combine
  x <- rbind(x_unmet, x_latest, x_oldest)
  x <- x[!duplicated(x), ]

  ## Done
  return(x)
}



## Standard age groups
get_all_age_groups <- function() {
  c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "15-49")
}


## Define file names. Requires package 'here' to be installed.
input_data_file_name <- function(age_group) {
  stopifnot(requireNamespace("here"))
  if (identical(age_group, "15-49"))
    fnames <- paste0("data_cp_model_all_women_", age_group, ".csv")
  else
    fnames <- paste0("OLD_data_cp_model_all_women_", age_group, ".csv")
  here::here("inst", "extdata", fnames)
}


## Create shortened input files
make_shortened_input_file <- function(age_group = get_all_age_groups()) {
  stopifnot(requireNamespace("here"))
  for (ag in age_group) {
    fn <- input_data_file_name(ag)

    message("Reading file: '", fn, "'.")

    FPEMglobal::validate_input_file(age_group = ag,
                                    input_data_folder_path = here::here("inst", "extdata"),
                                    data_csv_filename = basename(fn),
                                    marital_group = c("married", "unmarried"))

    csv_df <- read.csv(fn, header = TRUE, as.is = TRUE, check.names = FALSE,
                       stringsAsFactors = FALSE, strip.white = TRUE)

    csv_df <- shorten_for_testing_df(csv_df)

    new_fname <- gsub(paste0(ag, "\\.csv$"), paste0("SHORTFORTESTING_", ag, ".csv"),
                      x = fn)
    message("Writing file: '", new_fname, "'.")

    write.csv(csv_df, file = new_fname, row.names = FALSE, quote = TRUE)

    FPEMglobal::validate_input_file(age_group = ag,
                                    input_data_folder_path = here::here("inst", "extdata"),
                                    data_csv_filename = basename(new_fname),
                                    marital_group = c("married", "unmarried"))

  }
}


###-----------------------------------------------------------------------------
### OUTPUT all

## usethis::use_data(DATASET, overwrite = TRUE)

make_shortened_input_file()
