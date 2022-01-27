# gene.dup <- read.csv("gene-dup-constraints.csv")
# gene.dup.check <- checkGeneSymbols(gene.dup$Gene, unmapped.as.na = T)
# gene.dup$hgnc.symbol.suggestion <- gene.dup.check$Suggested.Symbol
# gene.dup.metadata <- getGeneMetadata(gene.list = gene.dup$hgnc.symbol.suggestion)
# gene.dup.metadata$arm <- paste0(gene.dup.metadata$chromosome_name, substr(gene.dup.metadata$band, 1, 1))
# gene.dup.metadata$band.imprecise <- substr(gene.dup.metadata$band, 1, 3)
# gene.dup.merge <- merge(gene.dup, gene.dup.metadata, by.x = "hgnc.symbol.suggestion", by.y = "hgnc_symbol", all.x = T)
# write.csv(gene.dup.merge, file = "gene-dup-constraints-metadata.csv")\

library(HGNChelper)

processMetadata <- function(inputCSV, metadata){
    
    inputCSV.check <- checkGeneSymbols(inputCSV$Gene, unmapped.as.na = T)
    inputCSV$hgnc.symbol.suggestion <- inputCSV.check$Suggested.Symbol
    metadata$arm <- paste0(metadata$chromosome_name, substr(metadata$band, 1, 1))
    metadata$band.imprecise <- substr(metadata$band, 1, 3)
    inputCSV.merge <- merge(inputCSV, metadata, by.x = "hgnc.symbol.suggestion", by.y = "hgnc_symbol", all.x = T)
    
    return(inputCSV.merge)
}