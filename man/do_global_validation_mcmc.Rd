% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_interface.R
\name{do_global_validation_mcmc}
\alias{do_global_validation_mcmc}
\title{Generate MCMC sample for an out-of-sample validation of an FPEM global run}
\usage{
do_global_validation_mcmc(
  run_desc = "",
  run_name_override = NULL,
  run_name_to_validate = NULL,
  run_name_to_validate_output_folder_path = file.path("output", run_name_to_validate),
  exclude_unmet_only = FALSE,
  exclude_unmet_only_test_prop = 0.2,
  at_random = FALSE,
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
  estimation_iterations = 3,
  burn_in_iterations = 1,
  steps_before_progress_report = 4,
  thinning = 2,
  chain_nums = 1:3,
  run_in_parallel = isTRUE(length(chain_nums) > 1),
  output_folder_path = NULL,
  verbose = FALSE
)
}
\arguments{
\item{run_desc}{Character. Brief note to be appended to the
auto-generated \code{run_name}. Ignored if
\code{run_name_override} is non-\code{NULL}.}

\item{run_name_override}{Character. User defined run name to
override default generation. \code{run_desc} is ignored if
this is non-\code{NULL}.}

\item{run_name_to_validate}{Name of completed global run to
validate.}

\item{exclude_unmet_only}{Logical; do validtion exercise leaving
out unmet need observations? See details.}

\item{exclude_unmet_only_test_prop}{Numeric; the proportion of
observations to be left out for an unmet need validation run.}

\item{at_random}{Logical; do validtion exercise leaving out
observations at random? See details.}

\item{at_random_min_c}{Minimum number of data points per country
to ensure are left in training set.}

\item{at_random_test_prop}{Proportion of obs used for test set
when \code{at_random} is \code{TRUE}.}

\item{at_end}{Logical; do validtion exercise leaving out all
observations after \code{year_cutoff}. See details.}

\item{at_end_not_1_obs_c}{Logical; should obs that are the only
one for their particular country be retained in the training
set?}

\item{at_random_no_data}{Logical; do validation exercise where all
observations for a randomly selected set of countries are left
out at random?}

\item{at_random_no_data_strata}{Column in the country
classifications file (used in the run being validated) to
stratify on if \code{at_random_no_data = TRUE}. If \code{NULL}
(default) do not stratify.}

\item{at_random_no_data_test_prop}{Numeric; the proportion of
observations to be left out when \code{at_random_no_data =
    TRUE}.}

\item{leave_iso_out}{Logical; do validation exercise where all
data for only one country is left out?}

\item{leave_iso_out_iso_test}{Three-digit country ISO code,
numeric or character, designating the country to leave out if
\code{leave_iso_out = TRUE}.}

\item{year_cutoff}{The cut-off year to use to separate test from
training set if \code{at_end = TRUE}. All observations with
observation year equal to or greater than \code{year_cutoff}
are put in the test set.}

\item{seed_validation}{Random seed used, among other things, in
selection of countries to leave out.}

\item{generate_new_set}{Logical; generate a new training set in
validation exercise?}

\item{estimation_iterations}{Numeric. Number of MCMC iterations
that should be \emph{saved}. This is \emph{before}
\code{thinning}.}

\item{burn_in_iterations}{Numeric. Number of MCMC iterations that
should be run as burn-in before starting to save them.}

\item{steps_before_progress_report}{Numeric. The number of times
progress should reported during MCMC sampling.}

\item{thinning}{Numeric. The actual number of iterations saved is
\eqn{\frac{\code{estimation_iterations}}{\code{thinning}}}{\code{estimation_iterations}/\code{thinning}}.}

\item{chain_nums}{Numeric. The number of MCMC chains to run,
\emph{as a sequence}. E.g., for three chains use
\code{1:3}. You need to run at least two chains for
post-processing to be successful.}

\item{run_in_parallel}{Logical. Determines if MCMC chains are run
in parallel. Parallel running requires package
#ifdef windows
\pkg{doParallel}.
#endif
#ifndef windows

\pkg{doParallel} or \pkg{doMC}.
#endif
Defaults to serial running if \code{run_in_parallel = TRUE} but the package
is not available.}

\item{output_folder_path}{Filepath to directory where outputs
should be saved. If \code{NULL}, defaults to
\code{file.path("output", run_name)}.}

\item{verbose}{Logical; print lots and lots of messages about
progress?}

\item{run_name_to_validate_output_folder}{File path to results of
run \code{run_name_to_validate}.}
}
\value{
A name for the run returned invisibly as a character
string. MCMC chains are saved to
\file{\code{output_folder_path}/temp.JAGSobjects}. They need
to be post-processed with \code{\link{post_process_mcmc}}. The
run name must be passed to \code{\link{post_process_mcmc}} to
locate find the saved chains for processing. Run names for
married and unmarried runs must also be passed to
\code{\link{combine_runs}} to generate all women MCMC results.
}
\description{
Generates the MCMC output of an out-of-sample validation of a completed
global run of an FPEM model. No post-processing or results generation is done; only the
chains are produced. The recommended way to use this function is via a call
to \code{\link{do_global_validation_run}}. See the section \dQuote{See Also} below.
}
\details{
See \dQuote{Details} in the help file for
\code{\link{do_global_validation_run}}.
}
\examples{
vignette("FPEMglobal_Intro")
}
\seealso{
\code{\link{do_global_validation_run}} which calls this
function to generate MCMC results for a validation exercise on
a married or unmarried women run, post-processes it, and
produces results all in one call.
}
\author{
Mark Wheldon
}
