#library(BiocManager)
options(repos = BiocManager::repositories())

#workaround for file encodings
#readr:: guess_encoding("~/Desktop/ORFlite_AG2.csv")



source("getGeneMetadata.R")
source("metadataprocessing.R")

library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("darkly"),
                
                HTML(r"(<p style="text-align:center;">Joey Mays - Updated 2022-06-17</p>)"),
                
                # Application title
                titlePanel("Get Chromosomes", windowTitle = "GetChromosomes"),
                sidebarLayout(
                    sidebarPanel(fileInput(inputId = "inputCSV", label = "Upload File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                 
                                 selectInput("columnNameInput", "Select Gene Name Column", choices = NULL),
                                 
                                 radioButtons("symbolClass", "Select Symbol Class", choices = c("Gene Symbol"),),
                                 
                                 
                                 actionButton(inputId = "runTool", label = "Run: Get Metadata", icon("fas fa-running"), style="color: #fff; background-color: #00bc8c; border-color: #00a87d"),
                                 
                                 hr(),
                                 
                                 textOutput(outputId = "readyFlag"),
                                 
                                 
                                 downloadButton(outputId = "outputCSV", label = "Download Metadata", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    ),
                    mainPanel(
                        p("This tool takes a .csv file with a column of gene symbols and adds genomic metadata including chromsomes, cytobands, ensembl IDs, and locations for each gene."),
                        p("It will also attempt to correct gene symbol aliases to the accepted HGNC symbol."),
                        p("The query may take a (literal) minute or two to complete after clicking", strong("\"Run\".")),
                        p("Note: Currently supports only gene symbols as input (i.e. no ensembl IDs)."),
                        hr(),
                    ),
                )
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
    read.csv(infile$datapath, header = T)
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
        progress <- shiny::Progress$new()
        progress$set(message = "Fetching data", value = 0.5)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        #print("PRESSED")
        #geneMetadata <- getGeneMetadata(geneTable()[,input$columnNameInput])
        #print("FINISHED")
        vals$geneMetadataOutput <- processMetadata(inputCSV = geneTable(), geneColumn = input$columnNameInput)
        #print(input$columnNameInput)
        output$readyFlag <- renderText("Download Ready!")
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
