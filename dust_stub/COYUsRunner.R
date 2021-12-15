
REPOSITORY_DIRECTORY<-"repo"
EXPECTED_R_VERSION<-"3.0.0"
EXPECTED_PACKAGE_VERSION<-"1.6-1"
PACKAGE_NAME<-"coyu"

initial_args <- commandArgs(trailingOnly = FALSE)

file_arg_name = "--file="
script_dir = dirname(sub(file_arg_name, "", initial_args[grep(file_arg_name, initial_args)]))

args <- commandArgs(trailingOnly = TRUE)
input_file<-args[1]
error_file<-args[2]

if (is.null(error_file)) {
  error_file="COYUsRunnerErrors.DAT"
}

source(file.path(script_dir, "CoyuRunnerFunctions.R"))

if (!file.exists(winVirtStoreLocateFileToRead(input_file))) {
  stop(sprintf("COYUsRunner: File '%s' does not exist",input_file))
}

error_file=winVirtStoreLocateFileToWrite(error_file)

has_errors<-FALSE
current_dir<-getwd() 
status=0
warnings_list=list()

capture_warnings <- function() {
    if (length(warnings_list) > 1) {
        warnings_text = paste(sapply(warnings_list, function(x) { sprintf("Problem: %s", conditionMessage(x) )}), collapse="\n")
        #See documentation for "call" in R for details of how to format conditionCall. Example: capture.output(conditionCall(x))
    } else {
        warnings_text = ""
    }
    return(warnings_text)
}

 withCallingHandlers(
          {

            data_input<-readCoyu9File(input_file)
            
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
          warning=function(w) {
              new_w = warnings_list
              new_w[[length(new_w)+1]] =w
              warnings_list <<- new_w
              #invokeRestart("muffleWarning")
          },     
          error=function(e) {
            has_errors<<-TRUE
            raw_calls<-sys.calls()
            #Get the stack trace as something we can write(), losing
            #the last 2 elements of the stack as that's this error
            #handler.
            #
            #Also, if stack is "long", lose the first frame to save space in DUST's crappy error dialog...
            if (length(raw_calls) > 4) {
                calls<-capture.output(raw_calls[2:(length(raw_calls)-2)])
            } else {
                calls<-capture.output(raw_calls[1:(length(raw_calls)-2)])
            }

            warnings_text = capture_warnings()
            
            stack_text = paste(warnings_text, "\n", "R Error ocurred. ",e,"\nStack trace:\n\n",paste(calls,collapse="\n"))

            writeErrorFile(stack_text,
                           errorFile=error_file)
            status<-1
          })

if (!has_errors) {
  warnings_text=capture_warnings()
  writeErrorFile(paste(" FORTRAN COMPLETED OK","\n",warnings_text),errorFile=error_file)
}

quit(save="no",status=status)



