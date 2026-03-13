################################################################################
###
### Code related to deprecation messages.
###
### Not intended to be exported.
###
################################################################################

###-----------------------------------------------------------------------------
### * Arguments

###-----------------------------------------------------------------------------
### ** 'verbose'

## Copied this from 'R Packages (2e)' book, Ch. 21.7.4.
warn_for_verbose <- function(verbose = FALSE,
                             env = rlang::caller_env(),
                             user_env = rlang::caller_env(2)) {
  ## This function is not meant to be called directly, so don't worry about its
  ## default of `verbose = TRUE`.
  ## In authentic, indirect usage of this helper, this picks up on whether
  ## `verbose` was present in the **user's** call to the calling function.
  if (!lifecycle::is_present(verbose) || isFALSE(verbose)) {
    return(invisible())
  }

  lifecycle::deprecate_warn(
    when = "1.6.0",
    what = I("The `verbose` argument"),
    details = c(
        "Set `options(FPEMglobal.verbose = TRUE)` to turn on verbose progress messages globally."
    ),
    user_env = user_env
    )
    ## only set the option during authentic, indirect usage
    if (!identical(env, rlang::global_env())) {
        FPEMglobal_verbose(env = env)
    }
  invisible()
}

## Copied this from 'googledrive::local_drive_quiet()'
FPEMglobal_verbose <- function(env = parent.frame())
{
    withr::local_options(list(FPEMglobal.verbose = TRUE), .local_envir = env)
}

###-----------------------------------------------------------------------------
### ** 'run_desc', etc.

run_desc_etc_info_string <- function(fun_name) {
    paste0("The 'run_desc', 'run_name_override' (incl. 'run_name_override_married', etc.), and 'run_dir_path' arguments have all been *removed* and the 'output_dir_path' argument has been modified. Use 'run_name' to specify the run name and 'output_dir_path' to specify the top-level directory for outputs; a directory with name 'run_name' will be created, or searched for, inside 'output_dir_path'.")
}

warn_for_run_desc <- function(run_desc = TRUE,
                                  env = rlang::caller_env(),
                                  user_env = rlang::caller_env(2)) {

    if (!lifecycle::is_present(run_desc)) {
        return(invisible())
    }

    lifecycle::deprecate_stop(
                   when = "1.6.0",
                   what = I("The `run_desc` argument"),
                   details = c(run_desc_etc_info_string("run_desc")
                               ),
                   env = env
               )
    invisible()
}

warn_for_run_name_override <- function(run_name_override = TRUE,
                              env = rlang::caller_env(),
                              user_env = rlang::caller_env(2)) {

    if (!lifecycle::is_present(run_name_override)) {
        return(invisible())
    }

    lifecycle::deprecate_stop(
                   when = "1.6.0",
                   what = I("The `run_name_override` argument"),
                   details = c(run_desc_etc_info_string("run_name_override")
                               ),
                   env = env
               )
    invisible()
}
warn_for_run_name_override_married <- function(run_name_override_married = TRUE,
                                       env = rlang::caller_env(),
                                       user_env = rlang::caller_env(2)) {

    if (!lifecycle::is_present(run_name_override_married)) {
        return(invisible())
    }

    lifecycle::deprecate_stop(
                   when = "1.6.0",
                   what = I("The `run_name_override_married` argument"),
                   details = c(run_desc_etc_info_string("run_name_override_married")
                               ),
                   env = env
               )
    invisible()
}
warn_for_run_name_override_unmarried <- function(run_name_override_unmarried = TRUE,
                                               env = rlang::caller_env(),
                                               user_env = rlang::caller_env(2)) {

    if (!lifecycle::is_present(run_name_override_unmarried)) {
        return(invisible())
    }

    lifecycle::deprecate_stop(
                   when = "1.6.0",
                   what = I("The `run_name_override_unmarried` argument"),
                   details = c(run_desc_etc_info_string("run_name_override_unmarried")
                               ),
                   env = env
               )
    invisible()
}
warn_for_run_name_override_all_women <- function(run_name_override_all_women = TRUE,
                                                 env = rlang::caller_env(),
                                                 user_env = rlang::caller_env(2)) {

    if (!lifecycle::is_present(run_name_override_all_women)) {
        return(invisible())
    }

    lifecycle::deprecate_stop(
                   when = "1.6.0",
                   what = I("The `run_name_override_all_women` argument"),
                   details = c(run_desc_etc_info_string("run_name_override_all_women")
                               ),
                   env = env
               )
    invisible()
}

warn_for_run_dir_path <- function(run_dir_path = TRUE,
                                       env = rlang::caller_env(),
                                       user_env = rlang::caller_env(2)) {

    if (!lifecycle::is_present(run_dir_path)) {
        return(invisible())
    }

    lifecycle::deprecate_stop(
                   when = "1.6.0",
                   what = I("The `run_dir_path` argument"),
                   details = c(run_desc_etc_info_string("run_dir_path")
                               ),
                   env = env
               )
    invisible()
}

warn_for_output_folder_path <- function(output_folder_path = TRUE,
                                  env = rlang::caller_env(),
                                  user_env = rlang::caller_env(2)) {

    if (!lifecycle::is_present(output_folder_path)) {
        return(invisible())
    }

    lifecycle::deprecate_stop(
                   when = "1.6.0",
                   what = I("The `output_folder_path` argument"),
                   details = c(run_desc_etc_info_string("output_folder_path")
                               ),
                   env = env
               )
    invisible()
}
