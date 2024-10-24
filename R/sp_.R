
# Package-specific environment for storing site and drive
.sp_env <- new.env()

#' Set default Sharepoint settings for a session
#'
#' Sets default site and drive for using the other `sp_*()` functions.
#'
#' @param site Site identifier. Can be the site_name, site_url or site_id
#' @param drive Optional. Name of the drive. If no site is specified, will use the currently stored site (if it exists).
#'
#' @return
#' No return value
#'
#' @export
#'
#' @examplesIf interactive()
#' # Set default site
#' sp_defaults(site = "Data Analytics")
#'
#' # List drives from the default site
#' sp_list_drives()
#'
#' # List files/folders in the default site and drive.
#' # Since no default drive was added, uses the first listed drive for the site.
#' sp_list()
#'
sp_defaults <- function(site = NULL, drive = NULL) {
  .sp_env$site <- sp_site(site)
  .sp_env$drive <- sp_drive(NULL, drive) # NULL pulls from stored site
}

#' Return Microsoft365R site or drive object
#'
#' Pulls the site or drive (if given) or returns the stored default
#'
#' @param site Site identifier. Can be the site_name, site_url or site_id. If not provided, returns the stored default site if it exists.
#' @param drive Name of the drive within the site. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#'
#' @return
#' A Microsoft365R site object or drive object
#' @export
#'
#' @examplesIf interactive()
#'
#' # Get current default site
#' x <- sp_site()
#'
#' # Get specified site
#' x <- sp_site("Data Analytics")
sp_site <- function(site) {
  if (!is.null(site)) {
    if (grepl("^https://", site)) { # Site URL
      tryCatch(
        Microsoft365R::get_sharepoint_site(site_url = site),
        error = \(cnd) {
          cli::cli_abort(c(
            "x" = "Could not find site with url {.url {site}}",
            "i" = "Check that the site URL is correct."
            ), parent = NA)
        }
      )
    } else if (grepl("^tntp.sharepoint.com,", site)) { # Site ID
      tryCatch(
        Microsoft365R::get_sharepoint_site(site_id = site),
        error = \(cnd) {
          cli::cli_abort(c(
            "x" = "Could not find site with id {.val {site}}",
            "i" = "Try using the site URL or name instead."
          ), parent = NA)
        }
      )
    } else { # Site Name
      tryCatch(
        Microsoft365R::get_sharepoint_site(site_name = site),
        error = \(cnd) {
          cli::cli_abort(c(
            "x" = "Could not find site with name {.val {site}}",
            "i" = "To find a site by name, you must be following the site.",
            "i" = "Go to {.url https://tntp.sharepoint.com/_layouts/15/sharepoint.aspx} to see a list of sites you are following."
          ), parent = NA)
        }
      )
    }
  } else if (!is.null(.sp_env$site)) {
    .sp_env$site
  } else {
    cli::cli_abort(c("x" = "No site provided and no default site exists."))
  }
}

#' Return Microsoft365R drive object
#' @export
#' @rdname sp_site
sp_drive <- function(site, drive) {
  site <- sp_site(site)
  if (is.null(drive) && !is.null(.sp_env$drive)) {
    .sp_env$drive
  } else {
    site$get_drive(drive)
  }
}

#' Internal function to create cli-friendly site/drive/path string
#'
#' Can use either a site_name or site object, a drive_name or drive_object
#'
#' @param site site object (from sp_site())
#' @param site_name site name
#' @param drive drive object (from sp_drive())
#' @param drive_name drive name
#' @param path path
#'
#' @returns a cli-formatted string
sp_string <- function(site = NULL, site_name = NULL,
                      drive = NULL, drive_name = NULL,
                      path = NULL) {
  site <- site_name %||% site$properties$displayName %||% .sp_env$site$properties$displayName
  drive <- drive_name %||% drive$properties$name %||% .sp_env$drive$properties$name

  paste0(cli::col_blue(site), ":",
         cli::col_green(drive), ":",
         cli::col_yellow(path))
}

#' List Sharepoint Contents
#'
#' `sp_list()` lists the contents of a Sharepoint Drive or a folder.
#'
#' `sp_list_drives()` lists the drives contained in a Sharepoint site.
#'
#' @param folder Path to the folder. By default, lists the top-level contents of the drive.
#' @param site Site identifier. Can be the site_name, site_url or site_id. If not provided, uses the stored default site if it exists.
#' @param drive Name of the drive within the site. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#' @param pattern Optional regular expression. Only names which match the regular expression will be returned.
#' @param full_names logical. If TRUE, the directory path is prepended to the file names to give a relative file path. If FALSE, the file names (rather than paths) are returned.
#' @param recursive logical. Should the listing recurse into directories? If TRUE, full_names is also set to TRUE.
#' @param include_dirs logical. Should subdirectory names be included in recursive listings? (They always are in non-recursive ones)
#'
#' @return
#' A tibble with name and additional information on the relevant drives/files
#'
#' @export
#'
#' @examplesIf interactive()
#'
#' # List drives from the default site
#' sp_list_drives()
#'
#' # List drives from a specific site
#' sp_list_drives("Data Analytics")
#'
sp_list <- function(folder = "", site = NULL, drive = NULL, pattern = NULL,
                    full_names = FALSE, recursive = FALSE, include_dirs = FALSE) {
  dr <- sp_drive(site, drive)

  if (recursive) full_names <- TRUE

  path_string <- sp_string(site_name = site, drive = dr, path = folder)
  cli::cli_inform("Fetching from {path_string}")

  tbl <- dr$list_files(folder, full_names = full_names) |>
    tibble::as_tibble()

  # Remove leading "/" for drive-level names
  tbl$name <- gsub("^/", "", tbl$name)

  if (recursive) {
    folders <- tbl[tbl$isdir, ]
    if (nrow(folders) > 0) {
      tbl <- purrr::map(folders$name,
                        \(f) sp_list(f, site, drive, recursive = TRUE)) |>
        dplyr::bind_rows(tbl)

      tbl <- tbl[order(tbl$name), ]
    }
  }

  if (!is.null(pattern)) tbl <- tbl[grep(pattern, tbl$name), ]
  if (recursive && !include_dirs) tbl <- tbl[!tbl$isdir, ]
  tbl
}

#' List Drives in a Sharepoint Site
#' @rdname sp_list
#' @export
sp_list_drives <- function(site = NULL, pattern = NULL) {
  site <- sp_site(site)

  path_string <- sp_string(site = site, drive_name = "")
  cli::cli_inform("Listing drives in {path_string}")

  tbl <- site$list_drives() |>
    purrr::map("properties") |>
    purrr::map(\(p) tibble::tibble(name = p$name, url = p$webUrl)) |>
    purrr::list_rbind()

  if (is.null(pattern)) tbl else tbl[grep(pattern, tbl$name), ]
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
#' @param site Site identifier. Can be the site_name, site_url or site_id. If not provided, uses the stored default site if it exists.
#' @param drive Name of the drive within the site. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#' @param type Optional. One of "rdata", "rds", or "dataframe". Uses the file extension to determine type if not provided.
#'
#' @return
#' `sp_read()` returns an R object as specified by type
#' `sp_write()` returns x, invisibly
#'
#' @export
#'
#' @examplesIf interactive()
#'
#' # Set site defaults
#' sp_defaults(site = "Data Analytics")
#'
#'
sp_read <- function(path, site = NULL, drive = NULL, type = NULL) {

}

#' @export
#' @rdname sp_read
sp_write <- function(x, path, site = NULL, drive = NULL, type = NULL) {

  invisible(x)
}

# sp_upload
# sp_download
