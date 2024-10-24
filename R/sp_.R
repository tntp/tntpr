
# Package-specific environment for storing site and drive
.sp_env <- new.env()

#' Set default Sharepoint settings for a session
#'
#' Sets default site and drive for using the `sp_list()`, `sp_read()`,
#' and `sp_write()` functions.
#'
#' @param site Site identifier. Can be the site_name, site_url or site_id
#' @param drive Optional. Name of the drive. If no site is specified, will use the currently stored site (if it exists).
#'
#' @return
#' @export
#'
#' @examples
sp_defaults <- function(site = NULL, drive = NULL) {
  .sp_env$site <- sp_site(site)
  .sp_env$drive <- sp_drive(NULL, drive) # NULL pulls from stored site
}

#' Return Microsoft365R site object
#'
#' Pulls the site (if given) or returns the stored default site
#'
#' @param site Site identifier. Can be the site_name, site_url or site_id. If not provided, returns the stored default site if it exists.
#'
#' @return
#' @export
#'
#' @examples
sp_site <- function(site) {
  if (!is.null(site)) {
    # Add in support for url or id?
    Microsoft365R::get_sharepoint_site(site)
  } else if (!is.null(.sp_env$site)) {
    .sp_env$site
  } else {
    cli::cli_abort("No site provided and no default site")
  }
}

#' Return Microsoft365R drive object
#'
#' Pulls the site and drive (if given) or returns the stored default site/drive
#'
#' @param site
#' @param drive
#'
#' @return
#' @export
#'
#' @examples
sp_drive <- function(site, drive) {
  site <- sp_site(site)
  if (is.null(drive) && !is.null(.sp_env$drive)) {
    .sp_env$drive
  } else {
    site$sp_drive(drive)
  }
}

#' Create cli-friendly site/drive/path string
sp_string <- function(site = NULL, site_name = NULL,
                      drive = NULL, drive_name = NULL,
                      folder = NULL) {
  site <- site_name %||% site$properties$displayName %||% .sp_env$site$properties$displayName
  drive <- drive_name %||% drive$properties$name %||% .sp_env$drive$properties$name

  paste0(cli::col_blue(site), ":",
         cli::col_green(drive), ":",
         cli::col_yellow(folder))
}

# Lists drives for a given site
sp_list_drives <- function(site = NULL) {
  site <- sp_site(site)

  path_string <- sp_string(site = site, drive_name = "")
  cli::cli_inform("Listing drives in {path_string}")
  drives <- site$list_drives()
  drives |>
    purrr::map("properties") |>
    purrr::map(\(p) tibble::tibble(name = p$name, url = p$webUrl)) |>
    purrr::list_rbind()
}

# Lists files/folders
sp_list <- function(folder = NULL, site = NULL, drive = NULL) {
  path <- folder %||% .sp_env$folder %||% ""
  drive <- sp_drive(site, drive)
  path_string <- sp_string(site_name = site, drive = drive, folder = path)
  cli::cli_inform("Listing from {path_string}")
  drive$list_files(path) |>
    tibble::as_tibble()
}

#' Read/Write from Sharepoint
#'
#' Read or write data to/from a Sharepoint drive. If site and/or drive aren't
#' specified, uses the currently saved default from `sp_defaults()` if it exists.
#'
#' These functions will attempt to use the appropriate read/write function based
#' on the file extension, however this can be overridden by specifying type
#'
#' @param x The object to be written
#' @param path The location in the Sharepoint drive
#' @param site Optional
#' @param drive
#' @param type Optional. One of "rdata", "rds", or "dataframe". Uses the file extension to determine type if not provided.
#'
#' @return
#' @export
#'
#' @examples
sp_read <- function(path, site = NULL, drive = NULL, type = NULL) {

}

#' @export
#' @rdname read_sp
sp_write <- function(x, path, site = NULL, drive = NULL, type = NULL) {

}

# sp_upload
# sp_download
