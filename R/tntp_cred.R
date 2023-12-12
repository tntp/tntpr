#' TNTP Credential Get/Set Command
#'
#' A wrapper around the `keyring` package for secure credential management.
#' This command will attempt to get a key, and if no credential is found it
#' will prompt you to add it (and then return it). You can also use this to
#' just set (or overwrite) values by using the `.set` parameter.
#'
#' @md
#'
#' @param service The identifier for the credential you are pulling or setting
#' @param username OPTIONAL. Can be used to specify different usernames for the same service
#' @param keyring OPTIONAL. Can be used to specify a specific keyring
#' @param .set Logical. If `TRUE`, will prompt the user to set a credential and will not return a value. Essentially the same as `keyring::key_set()`.
#'
#' @return A stored (or newly created) credential
#' @export
#'
#' @examples
#' \dontrun{
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
#' }
#'
tntp_cred <- function(service, username = NULL, keyring = NULL, .set = FALSE) {

  # Check for and return a value for that key
  if(!.set) {
    # Pull with key_get, or return an error
    cred <- tryCatch(keyring::key_get(service, username, keyring),
                     error = function(e) e)

    # If a credential is found, return it!
    if(!("error" %in% attr(cred, "class"))) {
      return(cred)

      # If no credential is found, ask if a user wants to create it
    } else if(!isTRUE(utils::askYesNo(paste0("No credentials found for '", service,
                                "'.\n\nDo you want to set credentials now?")))) {

      # If not, end with an error
      cli::cli_abort("No credentials found for {.val {service}}.")
    }
  }

  # If no value exists, ask if they would like to set a value, then set a value
  keyring::key_set(service, username, keyring,
                   prompt = paste0("Enter credential for '", service, "': "))

  # Return the newly set value
  if(!.set) keyring::key_get(service, username, keyring)

}
