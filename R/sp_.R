
# Package-specific environment for storing site and drive
.sp_env <- new.env(parent = emptyenv())

#' Set default Sharepoint settings for a session
#'
#' Sets default site and drive for using the other `sp_*()` functions.
#'
#' @param site Site identifier. Can be the site_name, site_url, site_id, or an ms_site object. If not provided, uses the current stored default site if it exists.
#' @param drive Name of the drive within the site. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#'
#' @return
#' No return value
#'
#' @export
#' @md
#'
#' @seealso [sp_list()], [sp_read()], [sp_write()], [sp_site()], [sp_drive()]
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

  # sp_drive() will accept a drive object, however this could lead to a
  # situation where the stored drive is NOT part of the stored site.
  if ("ms_drive" %in% class(drive)) {
    cli::cli_abort(c(
      "x" = "Cannot provide drive object to {.code sp_defaults()}",
      "i" = "Provide a drive name or id instead"
    ))
  }
  .sp_env$site <- sp_site(site)

  # If a site is provided, clear the current drive (prevents the situation where
  # the stored drive is NOT part of the stored site)
  if (!is.null(site)) .sp_env$drive <- NULL

  .sp_env$drive <- sp_drive(drive) # Uses stored site from previous line

  path <- sp_string(site = .sp_env$site, drive = .sp_env$drive)
  cli::cli_inform("Set default Sharepoint Site to {path}")
}

#' Test if a string is formatted like a Sharepoint site id
#'
#' @param x string to test
#' @return TRUE or FALSE
is_site_id <- function(x) {
  isTRUE(grepl("^.*\\.sharepoint\\.com,[a-z0-9-]*,[a-z0-9-]*$", x))
}

#' Test if a string is formatted like a Sharepoint drive id
#'
#' @param x string to test
#' @return TRUE or FALSE
is_drive_id <- function(x) {
  isTRUE(grepl("^b![a-zA-Z0-9_-]*$", x))
}

#' Test if a string is formatted like a Sharepoint site url
#'
#' @param x string to test
#' @return TRUE or FALSE
is_site_url <- function(x) {
  isTRUE(grepl("^https://", x))
}

#' Return Microsoft365R site or drive object
#'
#' Pulls the site or drive (if given) or returns the stored default. Useful if
#' you need to use methods that aren't currently wrapped by tntpr
#'
#' @param site Site identifier. Can be the site name, id, URL, or an ms_site object. If no site identifier is provided, uses the stored default site if it exists.
#' @param drive Drive identifier. Can be the drive name, id, or an ms_drive object. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#'
#' @return
#' A Microsoft365R site object or drive object
#' @export
#' @md
#'
#' @seealso [ms_site], [ms_drive]
#'
#' @examplesIf interactive()
#'
#' # Get current default site or drive
#' x <- sp_site()
#' y <- sp_drive()
#'
#' # Get specified site or drive
#' x <- sp_site("Data Analytics")
#' y <- sp_drive("Documents") # Uses stored default site
#' y <- sp_drive("Documents", site = "Data Analytics") # Use provided site
#'
#' # Use additional methods in the site/drive objects
#' x$get_lists()
#' y$get_item_properties("Analysis Tools.docx")
#'
sp_site <- function(site = NULL) {

  # No provided site
  if (is.null(site)) {
    if (!is.null(.sp_env$site)) {
      .sp_env$site
    } else {
      cli::cli_abort(c("x" = "No site provided and no default site exists."))
    }
  # Site object
  } else if ("ms_site" %in% class(site)) {
    site
  # Site URL
  } else if (is_site_url(site)) {
    tryCatch(
      Microsoft365R::get_sharepoint_site(site_url = site),
      error = \(cnd) {
        cli::cli_abort(c(
          "x" = "Could not find site with url {.url {site}}",
          "i" = "Check that the site URL is correct."
          ), parent = NA)
      }
    )
  # Site ID
  } else if (is_site_id(site)) {
    tryCatch(
      Microsoft365R::get_sharepoint_site(site_id = site),
      error = \(cnd) {
        cli::cli_abort(c(
          "x" = "Could not find site with id {.val {site}}",
          "i" = "Try using the site URL or name instead."
        ), parent = NA)
      }
    )
  # Site Name
  } else {
    tryCatch(
      Microsoft365R::get_sharepoint_site(site_name = site),
      error = \(cnd) {
        cli::cli_abort(c(
          "x" = "Could not find site with name {.val {site}}",
          "i" = "To find a site by name, you must be following the site.",
          "i" = "Go to {.url https://tntp.sharepoint.com/_layouts/15/sharepoint.aspx} or run {.run tntpr::sp_list_sites()} to see a list of sites you are following."
        ), parent = NA)
      }
    )
  }

}

#' Return Microsoft365R drive object
#' @export
#' @rdname sp_site
sp_drive <- function(drive = NULL, site = NULL) {

  # If a drive item is provided, use that
  if ("ms_drive" %in% class(drive)) return(drive)

  # Check provided drive is a character (if provided)
  if (!is.null(drive) && !"character" %in% class(drive)) {
    cli::cli_abort(c(
      "x" = "Unidentified drive value",
      "i" = "Provide a drive name or ms_drive object"
    ))
  }

  # Pull site
  site <- sp_site(site)

  # No drive provided: Use the default drive (if site matches and it exists), or use the first drive
  if (is.null(drive)) {
    if (all.equal(site, .sp_env$site) && !is.null(.sp_env$drive)) {
      .sp_env$drive
    } else {
      site$get_drive()
    }
  # Drive ID
  } else if (is_drive_id(drive)) {
    tryCatch(
      site$get_drive(drive_id = drive),
      error = \(cnd) {
        site_name <- site$properties$displayName
        cli::cli_abort(c(
          "x" = "Could not find a drive with id {.val {drive}} in site {.val {site_name}}",
          "i" = "Run {.run tntpr::sp_list_drives(site = '{site_name}')} to see available drives",
          "i" = "For sites with multiple drives with the same name, use drive ID"
        ), parent = NA)
      }
    )
  # Drive Name
  } else {
    tryCatch(
      site$get_drive(drive_name = drive),
      error = \(cnd) {
        site_name <- site$properties$displayName
        cli::cli_abort(c(
          "x" = "Could not find a drive with name {.val {drive}} in site {.val {site_name}}",
          "i" = "Run {.run tntpr::sp_list_drives(site = '{site_name}')} to see available drives"
        ), parent = NA)
      }
    )
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
  site <- site_name %||% site$properties$displayName
  drive <- drive_name %||% drive$properties$name

  paste(c(cli::col_blue(site),
          cli::col_green(drive),
          cli::col_yellow(path)),
        collapse = ":")
}

#' List Sharepoint Contents
#'
#' @description
#' Lists site/drive/folder contents. Can be used with default site/drive set by
#' [sp_defaults()] or with a specified site/drive.
#'
#' *  `sp_list()` lists the contents of a Sharepoint Drive or a folder.
#' *  `sp_list_drives()` lists the drives contained in a Sharepoint site.
#' *  `sp_list_sites()` lists the sites you have access to. These are the sites you are following in Sharepoint
#'
#' @param folder Path to the folder. By default, lists the top-level contents of the drive.
#' @param site Site identifier. Can be the site name, id, URL, or an ms_site object. If no site identifier is provided, uses the stored default site if it exists.
#' @param drive Drive identifier. Can be the drive name, id, or an ms_drive object. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#' @param pattern Optional regular expression. Only names which match the regular expression will be returned.
#' @param full_names logical. If TRUE, the directory path is prepended to the file names to give a relative file path. If FALSE, the file names (rather than paths) are returned.
#' @param recursive logical. Should the listing recurse into directories? If TRUE, full_names is also set to TRUE.
#' @param include_dirs logical. Should subdirectory names be included in recursive listings? (They always are in non-recursive ones)
#'
#' @return
#' A tibble with name and additional information on the relevant drives/files
#'
#' @export
#' @md
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

  site <- sp_site(site)
  drive <- sp_drive(drive = drive, site = site)

  if (recursive) full_names <- TRUE

  path_string <- sp_string(site = site, drive = drive, path = folder)
  cli::cli_inform("Fetching from {path_string}")

  tbl <- drive$list_files(folder, full_names = full_names) |>
    tibble::as_tibble()

  # Remove leading "/" for drive-level names
  tbl$name <- gsub("^/", "", tbl$name)

  # As far as I can tell there is no way to do a recursive search directly
  # through $list_files(), so this manually recurses.
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

  path_string <- sp_string(site = site)
  cli::cli_inform("Listing drives in {path_string}")

  tbl <- site$list_drives() |>
    purrr::map("properties") |>
    purrr::map(\(p) tibble::tibble(name = p$name, url = p$webUrl, id = p$id)) |>
    purrr::list_rbind()

  if (is.null(pattern)) tbl else tbl[grep(pattern, tbl$name), ]
}

#' List Drives in a Sharepoint Site
#' @rdname sp_list
#' @export
sp_list_sites <- function(pattern = NULL) {

  tbl <- Microsoft365R::list_sharepoint_sites() |>
    purrr::map("properties") |>
    purrr::map(\(p) tibble::tibble(name = p$displayName,
                                   url = p$webUrl,
                                   id = p$id)) |>
    purrr::list_rbind()

  cli::cli_inform(c(
    "i" = "Go to {.url https://tntp.sharepoint.com/_layouts/15/sharepoint.aspx} to follow additional sites"
  ))

  if (is.null(pattern)) tbl else tbl[grep(pattern, tbl$name), ]
}



#' Read/Write from Sharepoint
#'
#' @description
#' Read or write data to/from a Sharepoint drive. Can be used with default
#' site/drive set by [sp_defaults()] or with a specified site/drive.
#'
#' Currently supported file types include: `.csv`, `.csv2`, `.tsv`, `.xls`,
#' `.xlsx`, `.rds`
#'
#' These functions will attempt to use the appropriate read/write function based
#' on the file extension, however this can be overridden by specifying type.
#'
#' The `...` parameter is passed on to the appropriate reading or writing
#' function. See the details section for more information on these functions
#' by type.
#'
#' If the folder in `path` does not yet exist, the user will be prompted if they
#' would like to create it.
#'
#' @details
#' # Details
#' For more information on methods (shown as `$__()` below) see documentation
#' on [ms_drive].
#' ## Reading Functions
#' *  ".csv", ".csv2", ".tsv" are read using the `$load_dataframe()` method,
#' which uses [`readr::read_delim()`].
#' *  ".rds" is read using the `$load_rds()` method which accepts no additional
#' arguments.
#' *  ".xls" and ".xlsx" are read using [`readxl::read_excel()`] (if installed).
#' The function will download the excel file temporarily, then import it and
#' delete the temporary copy
#'
#' ## Writing Functions
#' *  ".csv", ".csv2", ".tsv" are written using the `$save_dataframe()` method
#' and uses [`readr::write_delim()`]. Delimiter will be assumed by the extension
#' unless provided in a `delim` argument
#' *  ".rds" is written using the `$save_rds()` method, which accepts no
#' additional arguments
#' *  ".xlsx" is written using [`writexl::write_xlsx()`] (if
#' installed) and then uploaded using the `$upload_file()` method.
#'
#' @param x The object to be written
#' @param path The location in the Sharepoint drive
#' @param site Site identifier. Can be the site name, id, URL, or an ms_site object. If no site identifier is provided, uses the stored default site if it exists.
#' @param drive Drive identifier. Can be the drive name, id, or an ms_drive object. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#' @param type Optional. One of "dataframe" (for delimited files), "xlsx", or "rds". Uses the file extension to determine type if not provided.
#' @param ... Additional arguments passed on to the reading/writing function.
#'
#' @seealso [sp_upload()], [sp_download()]; `$upload_file()`, `$download_file()`, `$save_rdata()`, `$load_rdata()` from [ms_drive]
#'
#' @return `sp_read()` returns an R object as specified by type. `sp_write()`
#' returns x, invisibly
#'
#' @export
#' @md
#'
#' @examplesIf interactive()
#'
#' # Set site defaults
#' sp_defaults(site = "Data Analytics")
#'
#' # Write a file
#' sp_write(mtcars, "mtcars.csv")
#'
#' # Write a file, specifying type and adding additional parameters
#' sp_write(mtcars, "mtcars.txt", type = "dataframe", delim = "|")
#'
#' # Read a file
#' x <- sp_read("mtcars.csv")
#' y <- sp_read("mtcars.txt", type = "dataframe", delim = "|")
#'
#' # Save / load an .rdata file using ms_drive methods
#' dr <- sp_drive() # Get stored default ms_drive object
#' dr$save_rdata(x, y, file = "data.rdata")
#' dr$load_rdata("data.rdata")
#'
#'
sp_read <- function(path, site = NULL, drive = NULL, type = NULL, ...) {

  ext <- get_ext(path)
  type <- process_type(ext, type)
  site <- sp_site(site)
  drive <- sp_drive(drive = drive, site = site)
  args <- rlang::list2(...)
  path_string <- sp_string(site = site, drive = drive, path = path)

  cli::cli_inform("Reading data from {path_string}")

  if (type == "rds") {
    tryCatch(
      drive$load_rds(path = path),
      error = \(cnd) sp_error(cnd, path_string)
    )
  } else if (type == "dataframe") {
    tryCatch(
      drive$load_dataframe(path = path, ...),
      error = \(cnd) sp_error(cnd, path_string)
    )
  } else if (type == "xlsx") {
    sp_read_xlsx(path, site, drive, ...) # Error catching in this function
  }

}

#' Internal function for reading excel files from Sharepoint
#'
#' @param path path
#' @param site ms_site object
#' @param drive ms_drive object
#' @param ... additional arguments from sp_read()
#'
#' @return data read by readxl::read_excel()
sp_read_xlsx <- function(path, site, drive, ...) {
  if (rlang::is_installed("readxl")) {
    ext <- get_ext(path)
    tf <- tempfile(fileext = ext)
    path_string <- sp_string(site = site, drive = drive, path = path)

    suppressMessages( # To block the cli_inform() with the tempfile
      sp_download(src = path, dest = tf, site = site, drive = drive)
    )
    on.exit(file.remove(tf)) # Error-safe cleanup

    readxl::read_excel(tf, ...)
  } else {
    cli::cli_abort(c(
      "x" = "Package `readxl` required to read .xls/.xlsx files",
      "i" = "Run {.code install.packages('readxl')} to install"
    ))
  }
}

#' @export
#' @rdname sp_read
sp_write <- function(x, path, site = NULL, drive = NULL, type = NULL, ...) {

  ext <- get_ext(path)
  type <- process_type(ext, type)
  site <- sp_site(site)
  drive <- sp_drive(drive = drive, site = site)
  args <- rlang::list2(...)
  path_string <- sp_string(site = site, drive = drive, path = path)

  # Check for existing folder in Sharepoint
  sp_check_folder(site, drive, folder_path = dirname(path))

  cli::cli_inform("Writing data to {path_string}")

  if (type == "rds") {
    tryCatch(
      drive$save_rds(object = x, file = path),
      error = \(cnd) sp_error(cnd, path_string)
    )
  } else if (type == "dataframe") {
    if (!"delim" %in% names(args)) {
      args$delim <- switch(ext, ".csv" = ",", .csv2 = ";", "\t")
    }
    args <- c(list(df = x, file = path), args)
    tryCatch(
      rlang::exec(drive$save_dataframe, !!!args),
      error = \(cnd) sp_error(cnd, path_string)
    )
  } else if (type == "xlsx") {
    if (ext != ".xlsx") cli::cli_abort(c(
      "{.code sp_write()} can only write Excel documents to {.val .xlsx} files"
    ))
    sp_write_xlsx(x, path, site, drive, ...) # Creates folders -- no error catching needed
  }

  invisible(x)
}

#' Function for uploading an xls / xlsx file
#'
#' @param x R object
#' @param path path on the Sharepoint drive
#' @param site ms_site object
#' @param drive ms_drive object
#' @param ... additional arguments passed on from sp_write()
#'
#' @return nothing
sp_write_xlsx <- function(x, path, site, drive, ...) {
  if (rlang::is_installed("writexl")) {
    tf <- tempfile(fileext = "xlsx")
    writexl::write_xlsx(x = x, path = tf, ...)
    on.exit(file.remove(tf)) # Error-safe cleanup
    suppressMessages( # To block the cli_inform() with the tempfile
      sp_upload(src = tf, dest = path, site = site, drive = drive)
    )
  } else {
    cli::cli_abort(c(
      "x" = "Package `writexl` required to write .xlsx files",
      "i" = "Run {.code install.packages('writexl')} to install"
    ))
  }
}

#' Sharepoint upload/download
#'
#' `sp_upload()` and `sp_download()` wrap the `$upload_file()` and `$download_file()`
#' methods from the [ms_drive] object. In addition, `sp_upload()` checks for the
#' existence of the destination folder and will prompt the user to create it if
#' it doesn't exist.
#'
#' @param src Location of source file. Either a local path (for `sp_upload`), or a Sharepoint path (for `sp_download`)
#' @param dest Location of destination file. If not provided, uses the same file name as `src`
#' @param site Site identifier. Can be the site name, id, URL, or an ms_site object. If no site identifier is provided, uses the stored default site if it exists.
#' @param drive Drive identifier. Can be the drive name, id, or an ms_drive object. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#' @param overwrite Should the destination file be overwritten if it exists?
#'
#' @md
#'
#' @seealso [sp_read()], [sp_write()]; `$upload_file()` and `$download_file()` from [ms_drive]
#'
#' @return Returns `dest` invisibly
#' @export
#'
#' @examplesIf interactive()
#'
#' # Set site defaults
#' sp_defaults("Data Analytics")
#'
#' # List files
#' sp_list()
#'
#' # Download a document locally
#' sp_download("Analysis Tools.docx", "Documents/AT.docx")
#'
#' # Upload a document
#' sp_upload("Documents/AT.docx", "Analysis Tools.docx")
#'
sp_upload <- function(src, dest, site = NULL, drive = NULL) {

  site <- sp_site(site)
  drive <- sp_drive(drive = drive, site = site)
  path_string <- sp_string(site = site, drive = drive, path = dest)

  # Check for existing file
  if (!file.exists(src)) {
    cli::cli_abort(c(
      "x" = "Could not find source file {.val {src}}"
    ))
  }

  # Check for existing folder in Sharepoint
  sp_check_folder(site, drive, folder_path = dirname(dest))

  cli::cli_inform("Uploading {.val {src}} to {path_string}")

  tryCatch(
    drive$upload_file(src = src, dest = dest),
    error = \(cnd) sp_error(cnd, path_string)
  )

  invisible(dest)
}

#' @export
#' @rdname sp_upload
sp_download <- function(src, dest, site = NULL, drive = NULL, overwrite = FALSE) {

  site <- sp_site(site)
  drive <- sp_drive(drive = drive, site = site)
  path_string <- sp_string(site = site, drive = drive, path = src)

  # Check for existing destination file
  if (file.exists(dest) && !overwrite) {
    cli::cli_abort(c(
      "x" = "Destination path {.val {dest}} exists, and {.var overwrite} is set to {.val FALSE}"
    ))
  }

  cli::cli_inform("Downloading {path_string} to {.val {dest}}")

  tryCatch(
    drive$download_file(src = src, dest = dest, overwrite = overwrite),
    error = \(cnd) sp_error(cnd, path_string)
  )

  invisible(dest)
}

#' Create Sharepoint Folder
#'
#' Wrapper around the `$create_folder()` method from [ms_drive]
#'
#' @param folder_path Path to the new folder
#' @param site Site identifier. Can be the site name, id, URL, or an ms_site object. If no site identifier is provided, uses the stored default site if it exists.
#' @param drive Drive identifier. Can be the drive name, id, or an ms_drive object. If site is provided but drive is not, uses the first drive of the provided site. If neither is provided, uses the stored default drive if it exists.
#'
#' @return returns `folder_path` invisibly
#' @export
#'
#' @md
#'
#' @examplesIf interactive()
#'
#' # Set site/drive defaults
#' sp_defaults("Data Analytics")
#'
#' # Create a folder
#' sp_create_folder("new/folder")
#'
sp_create_folder <- function(folder_path, site = NULL, drive = NULL) {

  site <- sp_site(site)
  drive <- sp_drive(drive = drive, site = site)
  path_string <- sp_string(site = site, drive = drive, path = folder_path)

  cli::cli_inform("Creating {path_string}")

  tryCatch(
    drive$create_folder(folder_path),
    error = \(cnd) sp_error(cnd, path_string)
  )

  invisible(folder_path)
}

#' Check for existence of a Sharepoint folder and offer to create it if it
#' doesn't exist
#'
#' @param site ms_site object
#' @param drive ms_drive object
#' @param folder_path path to an item
#'
#' @return nothing
sp_check_folder <- function(site, drive, folder_path) {

  # If no path is provided, return (dirname() returns "." for root)
  if (folder_path %in% c(".", NA)) return()

  tryCatch(
    drive$get_item(folder_path),

    # If it doesn't exist, ask if it should be created
    error = \(cnd) {
      path_string <- sp_string(site = site, drive = drive, path = folder_path)
      cli::cli_inform(c(
        "i" = "Destination folder {path_string} does not exist.",
        "Would you like to create this folder?"
      ))
      if (utils::select.list(c("No", "Yes")) == "Yes") {
        sp_create_folder(folder_path, site = site, drive = drive)
      } else {
        cli::cli_abort(c("x" = "Folder not created. Script Halted"),
                       parent = NA)
      }
    }
  )
}


#' For parsing sharepoint errors (right now just path errors)
#'
#' @param cnd condition
#' @param path_string path string (from sp_string())
#'
#' @return nothing
sp_error <- function(cnd, path_string) {
  if (grepl("HTTP 404", cnd$message)) {
    cli::cli_abort(c("X" = "File {path_string} not found"), parent = NA)
  } else {
    cli::cli_abort(c("X" = "Unexpected Error",
                     "i" = "{cnd$message}"), parent = NA)
  }
}

#' Pull extension from a path
#'
#' @param path file path
#' @return The extension as a character vector
get_ext <- function(path) {
  ext <- regmatches(path, regexpr("\\..{1,5}$", path))
  tolower(ext)
}

#' Determine function type by extension and provided type
#' Handles type validation
#'
#' @param ext File extension (from get_ext)
#' @param type User-specified type
#'
#' @return a type ("dataframe" "rds" or "rdata")
process_type <- function(ext, type) {

  # Extensions and associated types
  file_types <- list(
    ".csv" = "dataframe", ".csv2" = "dataframe", ".tsv" = "dataframe",
    ".xlsx" = "xlsx",".xls" = "xlsx", ".rds" = "rds"
  )

  # Determine function by file type
  if (is.null(type)) {

    type <- do.call(switch, c(ext, file_types, "error"))

    if (type == "error") {
      cli::cli_abort(c(
        "x" = "Cannot determine file type",
        "i" = "Extension {.val {ext}} is not associated with a known read/write function",
        "i" = "Fix the extension, or manually provide a {.var type} ({.val dataframe}, {.val rds}, or {.val xlsx})",
        "i" = "Known extensions are {.val {names(file_types)}}"
      ))
    }
  } else if (!type %in% file_types) {
    cli::cli_abort(c(
      "x" = "Invalid {.var type} {.val {type}}",
      "i" = "Valid read types are {.val {as.character(unique(file_types))}}"
    ))
  }
  type
}

# sp_upload
# sp_download
# sp_list_subsites
