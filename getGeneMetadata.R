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

getGeneMetadata2 <- function(gene.vector, name.type = "symbol", assembly = "hg19", lookup.table){
    
    if(!exists("lookup.table")){
        stop("Lookup Table not loaded")
    }
    
    if(assembly == "hg19"){
        
        #check for genes not in the lookup table
        if(!all(gene.vector %in% lookup.table$symbol)){
            to.remove <- setdiff(gene.vector, lookup.table$symbol)
            gene.vector <- setdiff(gene.vector, to.remove)
            to.remove.message <- paste0(to.remove, ", ", collapse = "")
            warning(paste("Data for", to.remove.message, "not available."), call. = F)
        }
        
        lookup.subset <- lookup.table[lookup.table$symbol %in% gene.vector,]

    } else {
        stop(paste0("Assembly ", assembly, " not supported."))
    }
        
    return(lookup.subset)
    
}
