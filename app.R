#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("getGeneMetadata.R")
source("metadataprocessing.R")

library(shiny)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Get Chromosomes", windowTitle = "GetChromosomes"),

    mainPanel(
        p("This tool takes a `.csv` file with a column of genes"),
        p("and returns the same file with added genomic metadata including chromsomes, "),
        p("cytobands, and locations for each gene."),
        
        
        fileInput(inputId = "inputCSV", label = "Upload File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            
        selectInput("columnNameInput", "Select Gene Name Column", choices = NULL),
        
        p("Currently supports gene symbols only (no ensembl IDs)"),
        radioButtons("symbolClass", "Select Symbol Class", choices = c("Gene Symbol"),),
        
        
        actionButton(inputId = "runTool", label = "Run"),
        
        
        downloadButton(outputId = "outputCSV", label = "Download .CSV"),
        ),
    )

# Define server logic
server <- function(input, output) {

    #create object geneTable when input file is selected
    geneTable <- reactive({
        infile <- input$inputCSV
    if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
    }
    read.csv(infile$datapath, header = T)
    })
    
    #update Column Selector when GeneTable is input
    observeEvent(geneTable(), {
        print("Table Loaded")
        updateSelectInput(inputId = "columnNameInput", choices = colnames(geneTable()))
    })
 
    #wait for button press
    observeEvent(input$runTool, {
        print("Running! 2")
        geneMetadata <- getGeneMetadata(gene.list = geneTable()[,input$columnNameInput])
        print("Finished!")
        print("Reconfiguring!")
        geneMetadata <- processMetadata(geneTable(), geneMetadata)
        print("Finished!")
        output$geneMetadataOutput <- geneMetadata
    })
    
    
    
    output$outputCSV <- downloadHandler(
        filename = function() {
            paste0("gene_metadata_",as.character(Sys.Date()),".csv")
        },
        content = function(file) {
            write.csv(geneMetadataOutput(), file, row.names = FALSE)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
