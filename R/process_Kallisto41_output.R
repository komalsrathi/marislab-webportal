load('data/kallisto_TPM_41cells.RData')

# rownames converted to genename_transcriptID
rownames(kallisto_TPM_41cells) <- paste(kallisto_TPM_41cells$GeneID, kallisto_TPM_41cells$EnsTranscritpID,sep="_")

# get biotype for transcripts
library(biomaRt)
grch37 = useMart(biomart = "ENSEMBL_MART_ENSEMBL", 
                 host = "grch37.ensembl.org", 
                 path="/biomart/martservice", 
                 dataset = "hsapiens_gene_ensembl")

filters <- listFilters(grch37)
attributes <- listAttributes(grch37)

res <- unique(getBM(attributes = c('ensembl_transcript_id','transcript_biotype'), 
                    filters = 'ensembl_transcript_id', 
                    values = as.character(kallisto_TPM_41cells$EnsTranscritpID), 
                    mart = grch37))

# only get protein coding and processed trancripts
tmp <- res[which(res$transcript_biotype=="protein_coding" | res$transcript_biotype=="processed_transcript"),]
kallisto_TPM_41cells_genes <- kallisto_TPM_41cells[which(kallisto_TPM_41cells$EnsTranscritpID %in% tmp$ensembl_transcript_id),]
kallisto_TPM_41cells_genes <- kallisto_TPM_41cells_genes[,-c(1,2,4,5)]
kallisto_TPM_41cells_genes <- kallisto_TPM_41cells_genes[-which(kallisto_TPM_41cells_genes$GeneID==""),]
kallisto_TPM_41cells_genes <- kallisto_TPM_41cells_genes[,-1]

save(kallisto_TPM_41cells_genes,file='data/kallisto_TPM_41cells_genes.RData')
