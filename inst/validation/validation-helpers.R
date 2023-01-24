
#' Extract information relevant to package validation from an R package
#'
#' This function is designed to quickly pull relevant validation information
#' from an R package. Note this is information relevant to **`mrgvalidate 2.0.0`
#' and later.** The function makes a number of assumptions about the structure
#' and contents of the R package directory (e.g. that it is a git repo). See
#' "Details" section below.
#'
#' @details This section describes the assumptions made by this function about
#' your R package directory. Some of these are fairly standard, but others rely
#' heavily on convention established at Metrum Research Group. To be clear,
#' these conventions are _not_ necessary for validating your package with
#' `mrgvalidate`, but they are necessary for using this helper function.
#'
#' * There is a `DESCRIPTION` file with `Package:` and `Version:` fields at the top-level.
#'
#' * There is a `NEWS.md` file at the top-level, which contains release notes. Each release is contained
#'   in a block seperated by top-level headings that begin `# <package name>`.
#'
#' * The package directory is a local clone of a git repository.
#'
#' * There are stories and requirements defined in YAML files in the `inst/validation/` sub-directory.
#'   These files are expected to be named _ending in_ `stories.yaml` and `requirements.yaml`, respectively.
#'   For example, `<package name>-stories.yaml` is valid, but `stories-for-<package name>.yaml` is not.
#'
#' @param path Path to the root directory of the package repo. This directory
#'   will be expected to contain DESCRIPTION and NEWS.md, as well be the root
#'   directory of a git repository.
#' @export
extract_r_package_info <- function(path) {

  # check that package dir exists and build absolute path
  checkmate::assert_directory_exists(path)
  path <- fs::path_real(path)

  # load name and version from DESCRIPTION
  desc_file <- desc::description$new(file.path(path, "DESCRIPTION"))
  .n <- desc_file$get("Package")
  .v <- desc_file$get("Version")

  # get repo url from remote
  repo <- stringr::str_split(system(glue::glue("git -C {path} remote -v"), intern = TRUE), "\\t| ")[[1]][2]

  # Build specs
  val_dir <- file.path(path, "inst", "validation")
  checkmate::assert_directory_exists(val_dir)
  spec_files <- find_spec_files(val_dir)
  spec_df <- mrgvalprep::read_spec_yaml(
    spec_files$stories,
    spec_files$requirements
  )

  # Parse release notes from YAML
  release_file <- parse_latest_release(
    file.path(path, "NEWS.md"),
    .n,
    tempfile(pattern = glue::glue("{.n}-{.v}-release"), fileext = ".md")
  )

  return(list(
    product_name = .n,
    version = .v,
    language = "R",
    repo_url = repo,
    specs = spec_df,
    release_notes_file = release_file
  ))
}

#' Parse most recent release from NEWS.md file
#'
#' Reads a text file at `news_path` and splits into "release" sections, assuming
#' releases are in blocks separated by top-level headings that begin `# {package_name}`.
#' The top one of these blocks is written to a new file at `out_path`.
#'
#' @return Returns `out_path`, to facilitate passing to
#'   `mrgvalidate::create_package_docs`, or another reader function.
#'
#' @param news_path Path to news file to read in.
#' @param package_name Name of the relevant package. Assumes release blocks are
#'   separated by top-level headings that begin `# {package_name}`.
#' @param out_path Path to write release notes to.
#' @export
parse_latest_release <- function(news_path, package_name, out_path) {
  checkmate::assert_string(news_path)
  checkmate::assert_string(out_path)

  news_lines <- readr::read_lines(news_path)
  releases <- grep(glue::glue("^# {package_name}"), news_lines)
  if (length(releases) == 0) {
    stop(glue::glue("No top-level headings matching '# <package name>' found in {news_path}"))
  }

  end_line <- if (length(releases) == 1) {
    length(news_lines)
  } else {
    releases[2] - 1
  }

  release_lines <- news_lines[releases[1]:end_line]
  readr::write_lines(release_lines, out_path)
  return(out_path)
}

#' Find stories and requirements YAML files
#' @param val_dir the directory to look in
#' @keywords internal
find_spec_files <- function(val_dir) {

  st_files <- fs::dir_ls(val_dir, regexp = "stories\\.ya?ml")

  if (length(st_files) == 0) {
    stop(glue::glue("can't find any valid stories YAML files in {val_dir}"))
  }

  req_files <- fs::dir_ls(val_dir, regexp = "requirements\\.ya?ml")

  if (length(req_files) == 0) {
    stop(glue::glue("can't find any valid requirements YAML files in {val_dir}"))
  }

  return(list(
    stories = st_files,
    requirements = req_files
  ))
}
