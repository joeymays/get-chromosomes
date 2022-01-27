#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(BiocManager)
#options(repos = BiocManager::repositories())

source("getGeneMetadata.R")
source("metadataprocessing.R")

library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
    
HTML(r"(<p style="text-align:center;">Joey Mays 2022</p>)"),
                
    # Application title
    titlePanel("Get Chromosomes", windowTitle = "GetChromosomes"),

    mainPanel(
        p("This tool takes a `.csv` file with a column of gene symbols and returns the same file with added genomic metadata including chromsomes, cytobands, and locations for each gene."),
        p("The query may take a (literal) minute or two to complete after clicking", strong("\"Run\".")),
        p("Note: Currently supports gene symbols only (no ensembl IDs)"),
        
        hr(),
        
        fileInput(inputId = "inputCSV", label = "Upload File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            
        selectInput("columnNameInput", "Select Gene Name Column", choices = NULL),
        
        radioButtons("symbolClass", "Select Symbol Class", choices = c("Gene Symbol"),),
        
        
        actionButton(inputId = "runTool", label = "Run", icon("fas fa-running"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        
        hr(),
        
        textOutput(outputId = "readyFlag"),
        
        
        downloadButton(outputId = "outputCSV", label = "Download .CSV", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        ),
    
    )

# Define server logic
server <- function(input, output) {
    
    vals <- reactiveValues()

    #create object geneTable when input file is selected
    geneTable <- reactive({
        infile <- input$inputCSV
    if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
    }
    read.csv(infile$datapath, header = T, fileEncoding = "UTF-8-BOM")
    })
    
    #update Column Selector when GeneTable is input
    observeEvent(geneTable(), {
        print("Table Loaded")
        updateSelectInput(inputId = "columnNameInput", choices = colnames(geneTable()))
        output$readyFlag <- renderText("Output Not Ready...")
    })
 
    #wait for button press
    observeEvent(input$runTool, {
        output$readyFlag <- renderText("Output Not Ready...  Processing...")
        #print("PRESSED")
        #geneMetadata <- getGeneMetadata(geneTable()[,input$columnNameInput])
        #print("FINISHED")
        vals$geneMetadataOutput <- processMetadata(geneTable())
        #print("FINISHED 2")
        output$readyFlag <- renderText("Output Not Ready...  Processing...   Download Ready!")
    })
    
    #observeEvent(output$readyFlag, {
    #    vals$geneMetadataOutput <- processMetadata(geneTable())
    #    output$readyFlag <- renderText("Output Not Ready...  Processing...   Download Ready!")
    #})
    

    output$outputCSV <- downloadHandler(
        filename = function() {
            paste0("gene_metadata_",as.character(Sys.Date()),".csv")
        },
        content = function(file) {
            write.csv(vals$geneMetadataOutput, file, row.names = FALSE)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
