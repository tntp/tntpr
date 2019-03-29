#' Initialize a new repository, and a single subfolder, TNTP style.
#'
#' @description Create a new repository on Bitbucket, then set your working
#' directory to that folder and run this function.  It will set up the main
#' repo folder as well as a single subfolder in which you can work on your
#' immediate project.
#'
#' You must specify the subfolder name as well as the long name associated with
#' that project and the analyst(s) working on it.  These latter two values are
#' used to create a README.Md file.
#'
#' @param subfolder a character vector containing the concise name of a project subfolder.
#' E.g., if the repository is the name of a city "Anywhere City", a project subfolder might be
#' "ela_access" or "aps_talent_landscape").
#' @param proj_name the longer, full name of the subfolder project.  This will
#' appear in the subfolder's README.md file.  E.g., "Access to Grade-Level ELA Content Pilot."
#' @param analyst_name the name(s) of the analysts currently working on the subfolder
#' project.  This will appear in the subfolder's README.md file.
#'
#' @export
#' @examples
#' # After the user has created a repository "Anywhere City" and set their working
#' # directory to that folder:
#' setup_repo("ela_access",
#' "Access to Grade-Level ELA Content",
#' "Dustin Pashouwer and Sam Firke")
setup_repo <- function(subfolder, proj_name, analyst_name){
  if(missing(subfolder)){
    stop("Specify the subfolder name of the project within this repo you'll be working on")
  }
  if(missing(proj_name)){
    stop("Specify the full name of the subfolder project as it should appear in the README")
  }
  if(missing(analyst_name)){
    stop("Specify the analyst(s) working on this subfolder project")
  }

  # Files for git to ignore
  to_ignore <- c(
    ".Rhistory", ".RData", ".Rproj.user" # set RStudio to not store .Rhistory
    # and .RData ... but just in case someone didn't
  )

  # Create project using the current working directory
  #  If no .Rproj is returned (may see a .here file) then ensure
  #  RStudio is available
  usethis::create_project(path = getwd())
  unlink("R", recursive = TRUE) # create_project created "R" folder, we don't want
  usethis::use_git_ignore(ignores = to_ignore)

  # Create subfolder
  invisible(lapply(subfolder, setup_subdirectory, proj_name, analyst_name))

  # Create main repo readme
  if(!any(grepl("README.Md", list.files(), ignore.case = TRUE))) {
    writeLines(create_repo_readme(proj_name, analyst_name), "README.Md")
  } else {
    overwrite_repo_readme <- utils::menu(c("Keep current README.Md", "Replace with template README.Md"),
                                         title = "This repository already contains a file README.Md.  Do you wish to replace with the tntpr template README file?")
    if(overwrite_repo_readme == 2){
      writeLines(create_repo_readme(proj_name, analyst_name), "README.Md")
    }
  }

}


#' Initialize a new subdirectory in an existing repository, TNTP style.
#'
#' @description A repository might represent a region, like "Anywhere City",
#' or a major client or contract, like "Midwestern Charter Network.
#' Within that repo you would have a subfolder for each analysis project.
#' This function creates such a subfolder and populates it with folders and a README.
#'
#' To use: within an existing repository on Bitbucket, set your your working
#' directory to that folder and run this function to create a sub-folder.
#'
#' Use \code{setup_repo()} in a blank new repository to add the first project subfolder
#' and create the RProject and .gitignore files.  Add subsequent analysis project folders
#' with this function.
#'
#' @param subfolder a character vector containing the concise name of a project subfolder.
#' E.g., if the repository is the name of a city "Anywhere City", a project subfolder might be
#' "ela_access" or "aps_talent_landscape").
#' @param proj_name the longer, full name of the subfolder project.  This will
#' appear in the subfolder's README.md file.
#' @param analyst_name the name(s) of the analysts currently working on the subfolder
#' project.  This will appear in the subfolder's README.md file.

#' @export
#' @examples
#' # When there's already a repository "Anywhere City" with an RProject and a .gitignore
#' # and a new project analysis subfolder is needed within that repo:
#' setup_subdirectory("ela_access",
#' "Equitable Access to Grade-Level ELA",
#' "Dustin Pashouwer and Sam Firke")

setup_subdirectory <- function(subfolder, proj_name, analyst_name){
  if(missing(proj_name)){
    stop("Specify the full name of the subfolder project as it should appear in the README")
  }
  if(missing(analyst_name)){
    stop("Specify the analyst(s) working on this subfolder project")
  }

  # Check if project exists in main directory, if not create it
  existing_files <- list.files(all.files = TRUE)
  if(!any(grepl(".Rproj$", existing_files))){
    usethis::create_project(path = getwd())
    unlink("R", recursive = TRUE) # create_project created "R" folder, we don't want
  }
  if(!any(grepl(".gitignore", existing_files))){
    to_ignore <- c(
      ".Rhistory", ".RData", ".Rproj.user" # set RStudio to not store .Rhistory
      # and .RData ... but just in case someone didn't
    )
    usethis::use_git_ignore(ignores = to_ignore)

  }

  # Create project directories
  dirs_to_make <- paste(
    subfolder,
    c("data",
      "data/raw",
      "data/clean",
      "code",
      "code/prep", # these scripts should read from the data/raw directory
      "code/analysis", # these scripts should read from the data/clean directory
      "output"), # Reports, with figures etc. in subdirectories as needed
    sep = "/"
  )
  invisible(lapply(dirs_to_make, usethis::use_directory))


  # Create subfolder readme
  if(!any(grepl("README.Md", list.files(subfolder), ignore.case = TRUE))) {
    writeLines(create_subfolder_readme(proj_name, analyst_name),
               paste0(subfolder, "/README.Md"))
  } else {
    warning(paste0("README.Md already exists in subdirectory ",
                   subfolder,
                   "; no new README.Md created"))
  }
}

# Create the vector of info to generate a standard README.Md main repo template
create_repo_readme <- function(proj_name, analyst){
  repo_dir <- getwd()
  repo_dir <- stringr::str_split(repo_dir, "/")
  repo_dir <- dplyr::last(repo_dir[[1]])

  c(repo_dir,
    paste0(rep("=", nchar(repo_dir)), collapse = ""),
    paste0("**Repository initialized:** ", Sys.Date(), "  "), # 2+ spaces at end create line break in Bitbucket
    "\n",
    "Projects in this repo",
    "==========================  ",
    "\n",
    paste0(proj_name, "  "),
    "----------------------------",
    paste0("**Analyst(s):** ", analyst, "  "),
    "**Other TNTP staff involved**:  ",
    "**Summary of the project:** ...  ",
    "\n", "\n"
  )
}

# Create the vector of info to generate a standard README.Md subfolder template
create_subfolder_readme <- function(proj_name, analyst){
  c(proj_name,
    paste0(rep("=", nchar(proj_name)), collapse = ""),
    paste0("**Date initialized:** ", Sys.Date(), "  "), # 2+ spaces at end create line break in Bitbucket
    paste0("**Analyst(s):** ", analyst, "  "),
    "**Other TNTP staff involved**:  ",
    "\n", "\n",
    "Purpose of the analysis: ",
    "----------------------------",
    "...  ",
    "Other good-to-know context/info:  ",
    "----------------------------",
    "...  ",
    "Resolution of the project:  ",
    "----------------------------",
    "...  "
  )
}
