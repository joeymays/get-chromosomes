options(repos = BiocManager::repositories())

source("getGeneMetadata.R")
source("metadataprocessing.R")
source("tabs.R")

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("darkly"),
                
                HTML(r"(<p style="text-align:center;">Joey Mays - Updated 2023-02-07</p>)"),
                
                # Application title
                titlePanel("Get Chromosomes", windowTitle = "GetChromosomes"),
                sidebarLayout(
                    sidebarPanel(fileInput(inputId = "inputCSV", label = "Upload File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv", ".tsv", ".txt")),
                                 
                                 selectInput("columnNameInput", "Select Symbol Name Column", choices = NULL),
                                 
                                 radioButtons("symbolClass", "Select Symbol Class", choices = c("Gene Symbol"),),
                                 
                                 radioButtons("Assembly", "Select Assembly", choices = c("hg19"),),
                                 
                                 actionButton(inputId = "runTool", label = "Run: Get Metadata", icon("fas fa-running"), style="color: #fff; background-color: #00bc8c; border-color: #00a87d"),
                                 
                                 ui.tabs,
                    ),
                    mainPanel(
                        p("This tool takes a .csv or tab-delimited .txt file with a column of gene symbols and adds genomic metadata including chromsomes, cytobands, and locations for each gene."),
                        p("It will also attempt to correct gene symbol aliases to the accepted HGNC symbol."),
                        p("Note: Currently supports only gene symbols as input (i.e. no ensembl IDs)."),
                        hr(),
                    ),
                )
)

server <- function(input, output) {
    
    hg19.gene.lookup <- reactive({readRDS("hg19-gene-lookup.RDS")})
    hgnc.table.human.20220621 <- reactive({readRDS(file = "hgnc.table.human.20220621.RDS")})
    
    #create object geneTable when input file is selected
    geneTable <- reactive({
            req(input$inputCSV)
            
            ext <- tools::file_ext(input$inputCSV$name)
            tableInput <- switch(ext,
                   csv = read.csv(input$inputCSV$datapath, sep = ",", header = T),
                   txt = read.table(input$inputCSV$datapath, sep = '\t', header = T),
                   tsv = read.table(input$inputCSV$datapath, sep = '\t', header = T),
                   validate("Invalid file; Please upload a .csv, .tsv, or .txt file")
            )
            
            updateTabsetPanel(inputId = "steps", selected = "beforeclick")
            return(tableInput)
        })
   
   # read.csv(infile$datapath, header = T)
    #})
    
    geneMetadataOutput <- reactive({
        req(input$inputCSV)
        processMetadata2(inputCSV = geneTable(), geneColumn = input$columnNameInput, assembly = "hg19", gene.map = hgnc.table.human.20220621(), lookup.table = hg19.gene.lookup())
    })
    
    #initialize session counter
    sessionCounter <- reactiveVal(as.numeric(0))
    
    #update Column Selector when GeneTable is input
    observeEvent(geneTable(), {
        print("Table Loaded")
        updateSelectInput(inputId = "columnNameInput", choices = colnames(geneTable()))
    })
 
    #wait for button press
    observeEvent(input$runTool, {
        progress <- shiny::Progress$new()
        progress$set(message = "Fetching data", value = 0.5)
       
         # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        
        #print("PRESSED")
        
        geneMetadataOutput()
        
        updateTabsetPanel(inputId = "steps", selected = "afterclick")
        
        sessionCounter(sessionCounter() + 1)
        print(sessionCounter())
    })
    
    output$outputCSV <- downloadHandler(
        filename = function() {
            paste0("gene_metadata_", as.character(Sys.Date()), "_", sprintf("%03d", sessionCounter()),".csv")
        },
        content = function(file) {
            write.csv(x = geneMetadataOutput(), file, row.names = FALSE)
        }
    )
}

shinyApp(ui = ui, server = server)
