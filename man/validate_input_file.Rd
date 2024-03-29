% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_interface.R
\name{validate_input_file}
\alias{validate_input_file}
\title{Validate input file for a global run of FPEM}
\usage{
validate_input_file(
  age_group = "15-49",
  input_data_folder_path = system.file("extdata", package = "FPEMglobal"),
  data_csv_filename = paste0("data_cp_model_all_women_", age_group, ".csv"),
  marital_group = c("married", "unmarried"),
  verbose = FALSE
)
}
\arguments{
\item{age_group}{Character. The age group for which a run of the
model is desired, specified in the format \dQuote{xx-yy},
where \dQuote{xx} is the start age, \dQuote{yy} is the end
age, e.g., \code{"15-49"} or \code{"15-19"}. This is used to
form the run name if \code{run_name_override} is \code{NULL},
to name of the file containing prevalence data if
\code{data_csv_filename} is \code{NULL}, and to select rows
from the denominator counts file if post-processing is done
(see \code{\link{post_process_mcmc}}).}

\item{input_data_folder_path}{File path to folder containing
\emph{all} input data (except any map shapefiles). If
\code{NULL} the value of \code{data_csv_filename}, etc., will
be passed to \code{\link{file.path}} as-is. Otherwise,
\code{file.path(input_data_folder_path, data_csv_filename)}
will be passed. The default value points to the data directory
supplied with the package.}

\item{data_csv_filename}{Filename of the \file{.csv} file
containing country-level prevalence data. See
\dQuote{Details}.}

\item{marital_group}{Character. The marital group for which a run
of the model is desired.}

\item{verbose}{Logical; print lots and lots of messages about
progress?}
}
\value{
If all checks pass, the processed input file is returned \emph{invisibly} as a data frame.
}
\description{
The main input file for \code{\link{do_global_run}} and friends
must meet certain requirements to be valid. These include correct
column names and valid cell values. This function can be used to
check that a candidate \file{.csv} files satisfies these
requirements.
}
\details{
See \dQuote{Details} in the help file for \code{\link{do_global_all_women_run}}.
}
\seealso{
\code{link{validate_denominator_counts_file}}, \code{\link{do_global_all_women_run}}
}
\author{
Mark Wheldon
}
