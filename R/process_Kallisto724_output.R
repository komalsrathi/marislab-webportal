library(dplyr)
load('Desktop/kaillisto_target724_matrix.rda')

# Transcript Level
# rownames converted to genename_transcriptID
tpmmat <- as.data.frame(tpmmat)
tpmmat <- merge(annot.info, tpmmat, by = "row.names")
rownames(tpmmat) <- paste(tpmmat$GeneID, tpmmat$EnsTranscriptID,sep="_")

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
                    values = as.character(tpmmat$EnsTranscriptID), 
                    mart = grch37))

# only get protein coding and processed trancripts
tmp <- res[which(res$transcript_biotype=="protein_coding" | res$transcript_biotype=="processed_transcript"),]
tpmmat <- tpmmat[which(tpmmat$EnsTranscriptID %in% tmp$ensembl_transcript_id),]
tpmmat <- tpmmat[-which(tpmmat$GeneID==""),]
tpmmat <- tpmmat[,-c(1:6)]

save(tpmmat,file='Projects/marislab-webportal/data/kallisto_TPM_Target724_genes.RData')

# Gene level 
gene_tpm <- merge(annot.info, gene_tpm, by.x = "EnsGeneID", by.y = "row.names")
gene_tpm <- unique(gene_tpm[,-c(1,2,4,5)])
gene_tpm <- gene_tpm[-which(gene_tpm$GeneID==""),]
gene_tpm$Mean <- rowMeans(gene_tpm[,2:ncol(gene_tpm)])
gene_tpm <- gene_tpm %>% group_by(GeneID) %>% filter(Mean == max(Mean)) %>% as.data.frame
rownames(gene_tpm) <- gene_tpm$GeneID
gene_tpm <- gene_tpm[,-c(1,ncol(gene_tpm))]
save(gene_tpm, file = "Projects/marislab-webportal/data/STAR_FPKM_Target724_genes.RData")
