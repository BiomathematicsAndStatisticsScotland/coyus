#These functions are used during setup of the COYU package and not otherwise

DEFAULT_CRAN_MIRROR = "http://star-www.st-andrews.ac.uk/cran/"
options("repos"=DEFAULT_CRAN_MIRROR)

#' name_bin_package
#'
#' Produces the native package file name for particular platforms
#' At the moment only windows and Redhat-derived linux known to be supported
#'
#' @param pkg_name Base name of package (e.g. "matrix") 
#' @param version_string The version string (e.g. 1.2-1) for the package version
#' @return the formatted package name
name_bin_package <- function(pkg_name,version_string) {
  r_info <- R.Version()

  if (grepl("linux",r_info$os) ) {
    return(sprintf("%s_%s.tar.gz",pkg_name,version_string))
    #N.B. Linux bin package name is 
    #return(sprintf("%s_%s_R_%s.tar.gz",pkg_name,version_string,r_info$platform))
  } else if (grepl("Windows",r_info$os) || grepl("mingw32",r_info$os)) {
    return(sprintf("%s_%s.zip",pkg_name,version_string))
  } else if (grepl("^darwin",r_info$os)) {
    return(sprintf("%s_%s.tgz",pkg_name,version_string))
  }

  stop(sprintf("Unsupported platform %s",format(r_info$platform)))
}

name_src_package <- function(pkg_name,version_string) {
  return(sprintf("%s_%s.tar.gz",pkg_name,version_string))
}

#' name_packages
#'
#' @return names for all package types that we can deal with (src, windows, macosx, linux (this platform only)
name_packages <- function(pkg_name,version_string) {
  r_info <- R.Version()
  
  pkg_names=list(
    src=sprintf("%s_%s.tar.gz",pkg_name,version_string),
    win=sprintf("%s_%s.zip",pkg_name,version_string),
    linux=sprintf("%s_%s.tar.gz",pkg_name,version_string),
    macosx=sprintf("%s_%s.tgz",pkg_name,version_string))

  return(pkg_names)
}


#Gets package and its non-Base dependencies. With thanks to
#http://stackoverflow.com/questions/21010705/update-a-specific-r-package-and-its-dependencies
pkg_and_deps <- function(pkg,
                         which = c("Depends", "Imports", "LinkingTo"),
                         mirror = getOption("repos"),
                         inc_pkg = TRUE) {
  stopifnot(require("tools"))
  message(sprintf("Obtaining available packages from %s - this can be slow - please wait", mirror))
  ap <- available.packages(repos=mirror) ## takes a minute on first use

  deps <- package_dependencies(pkg, db = ap, which = which, recursive = TRUE)
  
  ## the next line can generate warnings; I think these are harmless
  ## returns the Priority field. `NA` indicates not Base or Recommended
  pri <- sapply(deps[[1]], packageDescription, fields = "Priority")
  
  ## filter out Base & Recommended pkgs - we want the `NA` entries - i.e. addons to Base R
  deps <- deps[[1]][is.na(pri)]
  
  ## Add original package back to list
  if (inc_pkg) {
    deps = c(pkg, deps)
  }
  
  return(deps)
}

is_package_installed <- function(pkg_name,expected_version=NA) {

  is_latest_installed<-FALSE
   
  if (length(find.package(pkg_name,quiet=TRUE)) > 0) {
    details<-packageDescription(pkg_name)
    if (!is.na(expected_version)) {
      is_latest_installed <- compareVersion(details$Version, expected_version) >=0
    } else {
      is_latest_installed <- TRUE
    }
  }

  return(is_latest_installed)
}

is_directory <- function(path) {
  if (file.exists(path)) {
    file_info <- file.info(path)
    return(file_info$isdir)
  }
  return(FALSE)
}

get_repo_info <- function(repo_root) {

  repo=list(
    root=normalizePath(
      repo_root,
      winslash=.Platform$file.sep,
      mustWork=FALSE),
    win_root=normalizePath(
      paste(repo_root,"bin","windows","contrib",sep=.Platform$file.sep),
      winslash=.Platform$file.sep,
      mustWork=FALSE),
    src=normalizePath(
      paste(repo_root,"src","contrib",sep=.Platform$file.sep),
      winslash=.Platform$file.sep,
      mustWork=FALSE
      ),
    win_by_version=list())

  if (is_directory(repo$win_root)) {

    win_by_version=list()
    for (repo_file in dir(repo$win_root)) {
      win_repo_dir = paste(repo$win_root,repo_file,sep=.Platform$file.sep)
      if (is_directory(win_repo_dir) &&
          grepl('^[0-9]+.[0-9]+',repo_file)){
        win_by_version[[repo_file]]=normalizePath(
                        win_repo_dir,
                        winslash=.Platform$file.sep, 
                        mustWork=FALSE
                        )
      }
    }

    repo$win_by_version=win_by_version
  }    


  class(repo)<-"RepositoryInfo"
  
  return(repo)          
}

get_win_versions <- function(repo_obj) UseMethod("get_win_versions")

get_win_versions.RepositoryInfo <- function(repo_obj) {
  return(names(repo_obj$win_by_version))
}

is_valid_repo<- function(repo_obj) UseMethod("is_valid_repo")

is_valid_repo.RepositoryInfo<- function(repo_obj) {
  return( is_directory(repo_obj$src) &&
          is_directory(repo_obj$win_root) &&
          (length(repo_obj$win_by_version) > 0)
         )
}

#' get_win_repo_thisver
#'
#'
#' Given a repository object, return the directory within the
#' repository that should contain windows binary packages for this
#' version of R. For example, for R 3.0.2 and a repository root of
#' /tmp/reporoot, the function would return:
#'
#'   /tmp/reporoot/bin/windows/contrib/3.0
#'
#' This function does not guarantee that this directory exists, however calling create_repo on this
#' RepositoryInfo object will ensure that it does
#'
#' @param repo_obj Repository object
#' @return Full path to the windows binary package repository for this version of R
#' @seealso create_repo
get_win_repo_thisver <- function(repo_obj) UseMethod("get_win_repo_thisver")

get_win_repo_thisver.RepositoryInfo <- function(repo_obj) {
  r_version_string=paste(R.version$major,strsplit(R.version$minor,".",fixed=TRUE)[[1]][1],sep=".")

  return(paste(repo_obj$win_root,r_version_string,sep=.Platform$file.sep))
}

#' create_repo
#'
#' Given a RepositoryInfo object, create the repository it refers to, including a directory
#' for the windows binaries for this version of R (as returned by get_win_repo_thisver).
#' Function is idempotent
#' 
#' @param repo_obj Repository Object
#' @seealso get_win_repo_thisver
#' @return revised RepositoryInfo object containing details about the windows version supported by this repository
create_repo <- function(repo_obj) UseMethod("create_repo")

create_repo.RepositoryInfo <- function(repo_obj) {
  win_dir = get_win_repo_thisver(repo_obj)
  dir.create(win_dir, showWarnings = FALSE, recursive=TRUE)
  dir.create(repo_obj$src, showWarnings = FALSE, recursive=TRUE)

  return(get_repo_info(repo_obj$root))
}

#' update_repo
#'
#' Update the package repo's PACKAGES.gz file. 
#'
#' @param repo_obj RepositoryInfo object. A stop error will occur if is_valid_repo does not return true on this object
update_repo <- function(repo_obj) UseMethod("update_repo")


update_repo.RepositoryInfo <- function(repo_obj) {
  stopifnot(require("tools"))
  if (!is_valid_repo(repo_obj)) {
    stop(sprintf("Repository %s is not valid",repo_obj$root))
  }

  write_PACKAGES(repo_obj$src,type="source")

  for (win_repo in repo_obj$win_by_version) {
    write_PACKAGES(win_repo,type="win.binary")
  }  
}

#TODO: extend these to find packages without version numbers
find_package <- function(repo_obj,pkg_name,pkg_ver) UseMethod("find_package")

find_package.RepositoryInfo <- function(repo_obj,pkg_name,pkg_ver) {
  pkg_name <- name_bin_package(pkg_name,pkg_ver)
  return(list.files(repo_obj$root, recursive=TRUE, full.names=TRUE, pattern=pkg_name))
}

find_first_package <- function(repo_obj,pkg_name,pkg_ver) UseMethod("find_first_package")

find_first_package.RepositoryInfo <- function(repo_obj,pkg_name,pkg_ver) {
  pkg_name <- name_bin_package(pkg_name,pkg_ver)
  pkgs <- list.files(repo_obj$root, full.names=TRUE, recursive=TRUE, pattern=pkg_name)

  if (length(pkgs) > 0) {
    return(pkgs[1])
  }
  stop(sprintf("Unable to find package file %s in %s",pkg_name,repo_obj$root))
}


