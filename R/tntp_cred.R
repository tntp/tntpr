#' TNTP Credential Get/Set Command
#'
#' @description
#' A wrapper around the `keyring` package for secure credential management.
#'
#' `tntp_cred()` will attempt to get a credential, and if no credential is found
#' it will prompt you to add it (and then return it).
#'
#' `tntp_cred_set()` will set a credential. By default it will prompt before
#' overwriting any current credentials.
#'
#' `tntp_cred_list()` will list all current credentials by sorted by service
#' and username.
#'
#' @md
#'
#' @param service The identifier for the credential you are pulling or setting
#' @param username OPTIONAL. Can be used to specify different usernames for the same service
#' @param keyring OPTIONAL. Can be used to specify a specific keyring
#' @param prompt OPTIONAL. What text should be displayed above the input box for the key while setting?
#' @param overwrite OPTIONAL. By default, `tntp_cred_set()` will prompt if it finds a credential already saved. Set this to `TRUE` to overwrite without prompting or `FALSE` to throw an error if a current credential is found.
#'
#' @returns
#' *  `tntp_cred()` returns a stored (or newly created) credential
#' *  `tntp_cred_set()` returns nothing
#' *  `tntp_cred_list()` returns a 2-column data frame of services and usernames
#' @export
#'
#' @examplesIf interactive() && rlang::is_installed("qualtRics")
#' # Using tntp_cred() with qualtRics
#' library(qualtRics)
#'
#' # If no credential is set, this command will prompt for it first
#' qualtrics_token <- tntp_cred("QUALTRICS_TOKEN")
#' qualtrics_api_credentials(api_key = qualtrics_token,
#'                           base_url = 'tntp.co1.qualtrics.com')
#'
#' # To overwrite your Qualtrics credential
#' tntp_cred("QUALTRICS_TOKEN", .set = TRUE)
#'
tntp_cred <- function(service, username = NULL, keyring = NULL, prompt = NULL) {

  # Pull with key_get, or return an error
  cred <- tryCatch(keyring::key_get(service, username, keyring),
                   error = function(e) e)

  # If a credential isn't found, prompt user to create it
  if(("error" %in% attr(cred, "class"))) {

    cli::cli_inform(c("i" = "No credentials found for {.val {service}}",
                      "i" = "To list all current credentials stop the script and run {.run tntpr::tntp_cred_list()}",
                      "","Would you like to set new credentials for {.val {service}} now?"))

    input <- utils::select.list(c("Yes","No"))

    # If not, end with an error
    if(input != "Yes") {
      cli::cli_abort("No credentials found for {.val {service}}.")
    }

    # If yes, set and then re-pull the credential
    tntp_cred_set(service, username, keyring, prompt, overwrite = TRUE)
    cred <- keyring::key_get(service, username, keyring)

  }

  # Return the credential
  cred

}

#' @export
#' @rdname tntp_cred
tntp_cred_set <- function(service = NULL, username = NULL, keyring = NULL, prompt = NULL, overwrite = NULL) {

  if(is.null(prompt)) prompt <- paste0("Enter credential for '", service, "': ")

  # If overwrite is not TRUE, check for existing credential first
  if(!isTRUE(overwrite)) {

    # Check for existence of key by looking for an error in key_get()
    cred <- tryCatch(keyring::key_get(service, username, keyring),
                     error = function(e) e)

    # If a credential is found
    if(!("error" %in% attr(cred, "class"))) {

      # If overwrite == FALSE, end with error
      if(isFALSE(overwrite)) cli::cli_abort(c("x" = "Credential already found for this service and username.",
                                             "i" = "To overwrite, run with parameter {.code overwrite = TRUE}"))

      # If overwrite is anything else (NULL), warn about duplicate and prompt to overwrite
      cli::cli_inform(c("i" = "Credential already found for this service and username. Overwrite?"))
      if(utils::select.list(c('Overwrite with new credential','Cancel')) == 'Cancel') cli::cli_abort("Credentials not updated")
    }
  }

  # Write credential
  keyring::key_set(service, username, keyring, prompt)

}

#' @export
#' @rdname tntp_cred
tntp_cred_list <- function(service = NULL, keyring = NULL) {
  # Pull with key_list
  list <- keyring::key_list(service, keyring)

  # Sort by service, then username
  sorted <- list[order(list$service, list$username),]

  # Fix row names
  row.names(sorted) <- 1:nrow(sorted)

  # return sorted list
  sorted
}
