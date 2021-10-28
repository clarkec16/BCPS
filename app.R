shinyApp(
  ui = fluidPage(
    selectInput("graph1","Graph1",c("Vertical Bar","Donut","Horizontal Bar")),
    textInput('vec1', 'Enter a vector (comma delimited)', "0,1,2"),
    textInput("desc1",'Enter description data (comma delimited)',"Very, Satisfied,..."),
    downloadButton("report", "Generate report")
  ),
  server = function(input, output) {
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        # Adding another comment
        # Set up parameters to pass to Rmd document
        params <- list(
                       l = input$graph1, 
                       m = input$vec1,
                       p = input$desc1)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)
  
