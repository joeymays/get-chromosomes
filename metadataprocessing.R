# gene.dup <- read.csv("gene-dup-constraints.csv")
# gene.dup.check <- checkGeneSymbols(gene.dup$Gene, unmapped.as.na = T)
# gene.dup$hgnc.symbol.suggestion <- gene.dup.check$Suggested.Symbol
# gene.dup.metadata <- getGeneMetadata(gene.list = gene.dup$hgnc.symbol.suggestion)
# gene.dup.metadata$arm <- paste0(gene.dup.metadata$chromosome_name, substr(gene.dup.metadata$band, 1, 1))
# gene.dup.metadata$band.imprecise <- substr(gene.dup.metadata$band, 1, 3)
# gene.dup.merge <- merge(gene.dup, gene.dup.metadata, by.x = "hgnc.symbol.suggestion", by.y = "hgnc_symbol", all.x = T)
# write.csv(gene.dup.merge, file = "gene-dup-constraints-metadata.csv")\

library(HGNChelper)

processMetadata <- function(inputCSV, geneColumn){
    
    inputCSV[,geneColumn] <- trimws(inputCSV[,geneColumn])
    inputCSV.check <- checkGeneSymbols(inputCSV[,geneColumn], unmapped.as.na = T)
    inputCSV$hgnc.symbol.suggestion <- inputCSV.check$Suggested.Symbol
    inputCSV.metadata <- getGeneMetadata(gene.list = inputCSV$hgnc.symbol.suggestion)
    inputCSV.metadata$arm <- paste0(inputCSV.metadata$chromosome_name, substr(inputCSV.metadata$band, 1, 1))
    inputCSV.metadata$arm.letter <-  substr(inputCSV.metadata$band, 1, 1)
    inputCSV.metadata$full.band <-  paste0(inputCSV.metadata$chromosome_name, inputCSV.metadata$band)
    inputCSV.metadata$band.short <- substr(inputCSV.metadata$band, 1, 3)
    inputCSV.metadata$full.band.short <- paste0(inputCSV.metadata$chromosome_name, inputCSV.metadata$band.short)
    colnames(inputCSV.metadata)[1] <- "chr"
    inputCSV.metadata <- inputCSV.metadata[,c(5,4,1,7,6,8,9,11,10,2,3)]
    inputCSV.merge <- merge(inputCSV, inputCSV.metadata, by.x = "hgnc.symbol.suggestion", by.y = "hgnc_symbol", all.x = T, sort = F)
    
    return(inputCSV.merge)
}