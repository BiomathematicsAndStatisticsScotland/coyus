
REPOSITORY_DIRECTORY<-"repo"
EXPECTED_R_VERSION<-"3.0.0"
EXPECTED_PACKAGE_VERSION<-"1.9-1"
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

            ## Respect the DUST plot options
            if (data_input$plot_options == 1) {
                ## Plot all the candidates on the per-year charts. For statistical purposes
                all_candidates_plot_file = getAllCandidatesPlotFile(data_input)
                
                discard<-COYU_plot_results(results[[1]],
                                           data_input$character_key,
                                           plot_file=all_candidates_plot_file,
                                           plot_options=data_input$plot_options)
            }

            if (data_input$plot_options == 2) {
                ## Plot each of the candidates on a per-year chart, without any of the others.
                ## Useful for showing to breeders
                for (candidate in data_input$coyu_parameters$candidates) {
                    per_candidate_plot_file = getPerCandidatePlotFile(data_input,candidate)
                    
                    discard = COYU_plot_results(results[[1]],
                                                data_input$character_key,
                                                plot_file=per_candidate_plot_file,
                                                candidates=c(candidate),
                                                plot_options=data_input$plot_options)
                }
            }
              
	    ## Format the results into a string variable first as this
	    ## speeds writing files on network drives. We could probably
	    ## also set blocking=FALSE on an ordinary file connection but
	    ## I don't like the lack of error handling
            con<-textConnection("formatted_results", local=TRUE, open="w")
            ignore<-formatResults(data_input, results, con)
            close(con)			
            cat(formatted_results, file=data_input$output_file, sep="\n")	

            if (data_input$plot_options >= 0) {
                ## output candidate and reference varieties data in CSV form
                results_df = COYU_results_as_dataframe(results[[1]])
	        con<-textConnection("formatted_CSV", local=TRUE, open="w")
	        write.csv(results_df, file=con)
		close(con)
		cat(formatted_CSV, file=data_input$csv_file, sep="\n")
            }
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



