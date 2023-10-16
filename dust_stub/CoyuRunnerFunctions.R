#library(devtools)

#load_all("package/coyu")
library(coyu)

script_dir = dirname(sys.frame(1)$ofile)

source(file.path(script_dir, "IORoutines.R"))

formatTrialHeader<-function(header) {
  return(sprintf("%02d%01d%02d%02d %s",header$crop_type,header$site,header$trial_year,header$trial_type,header$trial_title))
}

resultsCOYU<-function(results,data_input,probability_set,connection="") {
  
  if (length(results)<1) {
    return
  }
  
  current_width<-options("width")
  options(width=data_input$output_width) #TODO: width not fully obeyed here

  write(formatTrialHeader(data_input),file=connection)
  
  COYU_print_results(results,
                     data_input$coyu_parameters,
                     data_input$character_key,
                     probability_set,
                     connection=connection,
                     verbose=data_input$plot_options!=0) 

  
    
  if (data_input$plot_options == 1) {
      write("\nPLOT FILE: \n",file=connection)
      write(sprintf("\t%s", getAllCandidatesPlotFile(data_input)), file=connection)
  }
    
  if (data_input$plot_options == 2) {
      write("\nPLOT FILES: \n",file=connection)
      write(sprintf("\t%s",
                    getPerCandidatePlotFile(data_input,
                                            data_input$coyu_parameters$candidates[1])),
            file=connection)
      write("  to   ", file=connection)
      write(sprintf("\t%s",
                    getPerCandidatePlotFile(data_input,
                                            tail(data_input$coyu_parameters$candidates, n=1))),
            file=connection)
  }

  ## reset width to original setting
  options(current_width)
}
 
formatResults<-function(data_input,results,con="") {
  apply(data_input$probability_sets,1,function(x) {
    sapply(results,resultsCOYU,data_input,x,con)
  })
  return(results)
}

writeErrorFile<-function(e,errorFile) {
  try(
      {
        vberror<-file(errorFile,open="w")
        write(e,file=vberror)
        close(vberror)
      })
}         
