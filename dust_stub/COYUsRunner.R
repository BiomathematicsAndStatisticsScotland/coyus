
REPOSITORY_DIRECTORY<-"repo"
EXPECTED_R_VERSION<-"3.0.0"
EXPECTED_PACKAGE_VERSION<-"1.4-1"
PACKAGE_NAME<-"coyu"

source("setupFunctions.R")

local_repo = get_repo_info(REPOSITORY_DIRECTORY)

if (!is_valid_repo(local_repo)) {
  stop(sprintf("Local package repostitory %s is not valid", local_repo_dir))
} 

if (compareVersion(as.character(getRversion()),EXPECTED_R_VERSION) == -1) {
  stop(sprintf("The version of R you have %s is older than the minimum supported version %s",
               getRversion(),
               EXPECTED_R_VERSION))
}

#Try local package repository first
if (!is_package_installed(PACKAGE_NAME,EXPECTED_PACKAGE_VERSION)) {
  print(sprintf("Installing %s from local repository %s", PACKAGE_NAME, local_repo$root))
  install.packages(PACKAGE_NAME,
                   repo=sprintf("file:%s",local_repo$root),
                   dependencies=c("Depends", "Imports", "LinkingTo"),
                   quiet=TRUE)
}

#Now try to get dependencies from CRAN if the package still isn't installed
if (!is_package_installed(PACKAGE_NAME,EXPECTED_PACKAGE_VERSION)) {
  print("Local install failed. Trying alternative method")
  pkg_file<-find_first_package(local_repo, PACKAGE_NAME, EXPECTED_PACKAGE_VERSION)

  if (!is_package_installed("lme4")) {
    print("Installing lme4 from CRAN")
    install.packages("lme4",dependencies=c("Depends", "Imports", "LinkingTo"))
  }

  print(sprintf("Installing package file %s", pkg_file))
  install.packages(pkg_file,repos=NULL,quiet=TRUE)
}

source("CoyuRunnerFunctions.R")

args <- commandArgs(trailingOnly = TRUE)
input_file<-args[1]
error_file<-args[2]

if (is.null(error_file)) {
  error_file="COYUsRunnerErrors.DAT"
}

if (!file.exists(input_file)) {
  stop(sprintf("COYUsRunner: File '%s' does not exist",input_file))
}

has_errors<-FALSE
current_dir<-getwd() 
status=0

#TODO: improve error handling in readCoyu9 and print informative messages to VBErrors.DAT
 withCallingHandlers(
          {
            
            setwd(dirname(input_file))
            data_input<-readCoyu9File(basename(input_file))            
            setwd(current_dir)
            
            results<-COYU_all_results(data_input$trial_data,
                                      data_input$coyu_parameters,
                                      data_input$probability_sets)
            
            discard<-COYU_plot_results(results[[1]],
                                       data_input$character_key,
                                       plot_file=data_input$plot_file)
            
            con<-file(data_input$output_file, open="w")
            ignore<-formatResults(data_input, results, con)
            close(con)

            results_df = COYU_results_as_dataframe(results[[1]])
            write.csv(results_df,file=data_input$csv_file)
          },
          error=function(e) {
            has_errors<<-TRUE
            raw_calls<-sys.calls()
            #Get the stack trace as something we can write(), losing
            #the last 2 elements of the stack as that's this error
            #handler           
            calls<-capture.output(raw_calls[1:(length(raw_calls)-2)])
           
            writeErrorFile(paste("R Error ocurred. Stack trace:\n\n",paste(calls,collapse="\n")),
                           errorFile=error_file)
            status<-1
          })

if (!has_errors) {
  writeErrorFile(" FORTRAN COMPLETED OK",errorFile=error_file)
}

quit(save="no",status=status)



