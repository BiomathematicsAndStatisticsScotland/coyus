#Setup the package library we will use
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
  stop('Usage: setupLibrary.R "<repository dir" "<deps|coyu>" "<library dir>"')
}

repo_root = cmd_args_trailing[1]
pkg_install = cmd_args_trailing[2]
library_dir = cmd_args_trailing[3]

if (!file.exists(library_dir)) {
  dir.create(library_dir)
}

local_repo=get_repo_info(repo_root)

install.packages("coyu", repos=sprintf("file:%s",local_repo$root), lib=library_dir, dependencies=c("Depends", "Imports", "LinkingTo"))