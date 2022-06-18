ui.tabs <- tabsetPanel(
    id = "steps",
    type = "hidden",
    tabPanel("beforeclick",
    ),
    tabPanel("afterclick", 
             p(" "),
             downloadButton(outputId = "outputCSV", label = "Download Metadata", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
)