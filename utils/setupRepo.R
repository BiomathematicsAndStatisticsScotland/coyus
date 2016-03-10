
library(tools)

#Find the path to this script. Inspired by: https://github.com/krlmlr/kimisc/blob/master/R/thisfile.R
cmd_args_all <- commandArgs(trailingOnly = FALSE)
cmd_args_trailing <- commandArgs(trailingOnly = TRUE)

leading_idx <-
  seq.int(from=1, length.out=length(cmd_args_all) - length(cmd_args_trailing))
cmd_args_all <- cmd_args_all[leading_idx]
this_file <- gsub("^(?:--file=(.*)|.*)$", "\\1", cmd_args_all)
                                        # If multiple --file arguments are given, R uses the last one
this_file <- tail(this_file[this_file!= ""], 1)

if (length(this_file) > 0) {
  script_dir<-normalizePath(dirname(this_file),mustWork=TRUE)
} else {
  stop("Unable to detemine path to currently executing script")
}

include_file<-file.path(script_dir,"..","dust_stub","setupFunctions.R")

source(include_file)

if (length(cmd_args_trailing) != 3) {
  stop('Usage: setupRepo.R "<download>" "<package version>" "<repository dir>"')
}

Sys.setenv(TAR="internal")

download = cmd_args_trailing[1]
pkg_ver = cmd_args_trailing[2]
repo_root = cmd_args_trailing[3]
  
pkg_list <- pkg_and_deps("lme4")

repo <- create_repo(get_repo_info(repo_root))

if (!is_valid_repo(repo)) {
  stop(sprintf("Repository %s is not valid",repo$root))
}

if (download=="yes") {
  download.packages(pkg_list,repo$src, type="source")
  download.packages(pkg_list, get_win_repo_thisver(repo), type="win.binary")
}

pkg_names <- name_packages("coyu",pkg_ver)

coyu_src_package <- paste("package",pkg_names$src, sep=.Platform$file.sep)
coyu_win_package <- paste("package",pkg_names$win, sep=.Platform$file.sep)

#coyu_macosx_package <- paste("package",pkg_names$macosx, sep=.Platform$file.sep)

#Copy in pre-built COYU source and binary packages
if (file.exists(coyu_src_package)) {
  file.copy(coyu_src_package, repo$src, overwrite=TRUE, copy.mode=TRUE)
}

if (file.exists(coyu_win_package)) {
  file.copy(coyu_win_package, get_win_repo_thisver(repo), overwrite=TRUE, copy.mode=TRUE)
}

# if (file.exists(coyu_macosx_package)) {
#   file.copy(coyu_win_package, get_macosx_repo_thisver(repo), overwrite=TRUE, copy.mode=TRUE)
# }

update_repo(repo)

#Now can do package install as follows and we have solved the distribution problem (I think).
#  install.packages("coyu",repos="file:///path/to/reposdir", dependencies=c("Depends", "Imports", "LinkingTo"))
#Update the DUST-related R stubs to do the install from a repo to support this.

#TODO: can we download packages from a different version to the one we're running? Handy for win.binary? Not a problem if not. 