library(Homo.sapiens)
library(tidyverse)


#things to look for
#spanning several cytobands
#NA gene symbols [x]
#duplicate gene symbols
#non-canonical chromosome seqnames [x]
#gene name corrections with hugo (at end or for matching?)

hgnc.table.human.20220621 <- getCurrentHumanMap()
saveRDS(hgnc.table.human.20220621, file = "hgnc.table.human.20220621.RDS")


tx <- transcriptsBy(Homo.sapiens, columns = c("SYMBOL"))
tx <- unlist(tx)
tx$SYMBOL <- unlist(tx$SYMBOL)
#tx$ENSEMBL <- unlist(tx$ENSEMBL)

# remove non-canonical chromosomes
tx <- tx[grep(x = decode(seqnames(tx)), pattern = "_", fixed = T, invert = T), ]

# remove NAs
tx <- tx[which(!is.na(tx$SYMBOL)),]

tx.df <- as.data.frame(tx, row.names = NULL) %>% as_tibble()
tx.pre <- tx.df %>% group_by(SYMBOL, seqnames, strand) %>% summarize(global.start = min(start), global.end = max(end)) %>% ungroup()
tx.pre

tx.pre[duplicated(tx.pre$SYMBOL),]

## 152 duplicates, leaving in, probably fine

#Get cytobands from UCSC hg19

cyto.hg19 <- read.table("cytoBand.hg19.txt", sep= '\t', skip = 1, col.names = c("chr", "start", "end", "cytoband", "gstain"))
cyto.hg19$strand <- c("*")
cyto.hg19.gr <- GRanges(seqnames = cyto.hg19$chr,
                        ranges = IRanges(cyto.hg19$start, cyto.hg19$end),
                        strand = cyto.hg19$strand,
                        cytoband = cyto.hg19$cytoband)

tx.pre.gr <- GRanges(seqnames = tx.pre$seqnames, 
                     ranges = IRanges(tx.pre$global.start, tx.pre$global.end), 
                     strand = tx.pre$strand, 
                     symbol = tx.pre$SYMBOL)

#Find overlaps between cytobands and genes

overlap.hits <- findOverlaps(tx.pre.gr, cyto.hg19.gr)

#Add cytobands as metadata for matches

new.metadata <- character(length = queryLength(overlap.hits))
new.metadata[] <- NA
new.metadata[queryHits(overlap.hits)] <- mcols(cyto.hg19.gr)[,"cytoband"][subjectHits(overlap.hits)]
mcols(tx.pre.gr)$cytoband <- new.metadata

hg19.gene.lookup <- as.data.frame(tx.pre.gr, row.names = NULL) %>% as_tibble()
colnames(hg19.gene.lookup)[1] <- 'chr'



hugo.genes <- HGNChelper::checkGeneSymbols(hg19.gene.lookup$symbol, 
                                           unmapped.as.na = F, 
                                           species = "human",
                                           map = HGNChelper::getCurrentHumanMap())


#hugo.genes[which(hugo.genes$x != hugo.genes$Suggested.Symbol),] #TRUE is number of changed symbols
hg19.gene.lookup$symbol <- hugo.genes$Suggested.Symbol

hg19.gene.lookup <- hg19.gene.lookup[,c(6,1,7,2,3,5)]

saveRDS(object = hg19.gene.lookup, file = "hg19-gene-lookup.RDS")


