#uses biomaRt package to get chromosome metadata from a vector of gene symbols or ensembl IDs
#input:
  #gene.list - vector of gene symbols or ensembl IDs
  #name.type - either "symbol" or "ensembl"

getGeneMetadata <- function(gene.list, name.type = "symbol", sex.chr = c("X","Y"), mirror = "uswest"){
  
  if(name.type != "symbol" & name.type != "ensembl"){
    stop("'name.type' should be 'symbol' for HGNC Symbol or 'ensembl' for Ensembl ID.")
  }
  
  mart <- biomaRt::useEnsembl(biomart = "ensembl", 
                              dataset = "hsapiens_gene_ensembl", mirror = mirror)
  
  
  if(is.null(sex.chr)){
    chr_list <- c(1:22)
  } else if(all(c("X","Y") %in% sex.chr)){
    chr_list <- c(1:22,"X","Y")
  } else if(sex.chr == "X"){
    chr_list <- c(1:22,"X")
  } else if(sex.chr == "Y"){
    chr_list <- c(1:22,"Y")
  } else {
    chr_list <- c(1:22)
  }
  
  if(name.type == "symbol"){
    
    gene.metadata <- biomaRt::getBM(attributes = c("chromosome_name", "start_position","end_position",
                                                   "ensembl_gene_id","hgnc_symbol","band"), 
                                    filters = c("chromosome_name","hgnc_symbol"),
                                    mart = mart, 
                                    values = list("chromosome_name"=chr_list, 
                                                  "hgnc_symbol"=gene.list))
  }
  
  if(name.type == "ensembl"){
    
    gene.metadata <- biomaRt::getBM(attributes = c("chromosome_name", "start_position","end_position",
                                                   "ensembl_gene_id","hgnc_symbol","band"),
                                    filters = c("chromosome_name","ensembl_gene_id"), 
                                    mart = mart, 
                                    values = list("chromosome_name"=chr_list, 
                                                  "ensembl_gene_id"=gene.list))
  }
  
  gene.metadata <- gene.metadata[!duplicated(gene.metadata[,"hgnc_symbol"]),]
  gene.metadata <- gene.metadata[order(gene.metadata$chromosome_name, gene.metadata$start_position),]
  
  return(gene.metadata)
}

getGeneMetadata2 <- function(gene.vector, name.type = "symbol", sex.chr = c("X", "Y")){
    
    
}


#scratch

while(FALSE){
    
    
#things to look for
    #spanning several cytobands
    #NA gene symbols [x]
    #duplicate gene symbols
    #non-canonical chromosome seqnames [x]
    #gene name corrections with hugo (at end or for matching?)
    
    tx <- transcriptsBy(Homo.sapiens, columns = "SYMBOL")
    tx <- unlist(tx)
    tx$SYMBOL <- unlist(tx$SYMBOL)
    
    # remove non-canonical chromosomes
    tx <- tx[grep(x = seqnames(tx), pattern = "_", fixed = T, invert = T), ]
    
    # remove NAs
    tx <- tx[which(!is.na(tx$SYMBOL)),]
    
    
    
    
    
    
    sc.genes <- HGNChelper::checkGeneSymbols(tx.pre$SYMBOL, unmapped.as.na = F, species = "human")
summary(sc.genes$x != sc.genes$Suggested.Symbol) #TRUE is number of changed symbols
rownames(sc.raw.counts) <- sc.genes$Suggested.Symbol



HGNChelper::checkGeneSymbols("AAO6", unmapped.as.na = F, species = "human")


# cytobands don't require gene names

tx <- transcriptsBy(Homo.sapiens, columns = "SYMBOL")
tx <- unlist(tx)
tx$SYMBOL <- unlist(tx$SYMBOL)


tx.df <- as.data.frame(tx, row.names = NULL) %>% as_tibble()

keep <- grep(x = tx.df$SYMBOL, pattern = "_", fixed = T, invert = T)
tx.pre <- tx.pre[keep, ]




tx.pre <- tx.df %>% group_by(SYMBOL, seqnames, strand) %>% summarize(global.start = min(start), global.end = max(end))
tx.pre

#Get cytobands from UCSC hg19

cyto.hg19 <- read.table("cytoBand.hg19.txt", sep= '\t', skip = 1, col.names = c("chr", "start", "end", "cytoband", "gstain"))
cyto.hg19$strand <- c("*")
cyto.hg19.gr <- GRanges(seqnames = cyto.hg19$chr,
                        ranges = IRanges(cyto.hg19$start, cyto.hg19$end),
                        strand = cyto.hg19$strand,
                        cytoband = cyto.hg19$cytoband)

#Find overlaps between cytobands and amplicon coverage

overlap.hits <- findOverlaps(tx, cyto.hg19.gr)

#Add cytobands as metadata for matches

new.metadata <- character(length = queryLength(overlap.hits))
new.metadata[] <- NA
new.metadata[queryHits(overlap.hits)] <- mcols(cyto.hg19.gr)[,"cytoband"][subjectHits(overlap.hits)]
mcols(tx)$cytoband <- new.metadata
mcols(tx)$arm <- paste0(decode(seqnames(tx)), substr(tx$cytoband, 1, 1))

tx.df <- as.data.frame(tx, row.names = NULL) %>% as_tibble()
tx.pre <- tx.df %>% group_by(SYMBOL, seqnames, strand, cytoband, arm) %>% summarize(global.start = min(start), global.end = max(end))
tx.pre

tx.pre$SYMBOL[duplicated(tx.pre$SYMBOL)]
nrow(tx.pre)

tx.pre %>% filter(SYMBOL == "ABCF1")

tx.pre %>% filter(SYMBOL == "ABHD16A") # getting atypical seqnames, filter seqnames

levels(seqnames(tx))

keep <- grep(x = tx.pre$seqnames, pattern = "_", fixed = T, invert = T)

tx.pre <- tx.pre[keep, ]
nrow(tx.pre)

tx.pre %>% filter(SYMBOL == "AFF1")


}
