library(reshape2)

TARGET_NBL_FPKM_mData <- read.csv('~/Desktop/TARGET_NBL_ClinicalData_20151124.csv', stringsAsFactors = F)
load('Projects/marislab-webportal/data/STAR_FPKM_Target724_genes.RData')
TARGET_NBL_FPKM_data <- STAR_FPKM_Target724_genes[,grep('TARGET-30-',colnames(STAR_FPKM_Target724_genes))]

tmp <- sub('[-][0-9]{2}[A-Z]{1}[-][0-9]{2}[A-Z]{1}','',colnames(TARGET_NBL_FPKM_data))
tt <- data.frame(TARGET.USI=tmp, TARGET.ID=colnames(TARGET_NBL_FPKM_data))
tt <- merge(tt, TARGET_NBL_FPKM_mData, by.x = 'TARGET.USI', by.y = 'TARGET.USI')
tt$Site <- sub('TARGET-30-[A-Z]{6}-','\\1',tt$TARGET.ID)
tt$Site[tt$Site=='01A-01R'] <- 'Primary tumor'
tt$Site[tt$Site=='02A-01R'] <- 'Secondary malignancy'
tt$Site[tt$Site=='04A-01R'] <- 'Bone marrow metastasis'
TARGET_NBL_FPKM_mData <- tt
rm(STAR_FPKM_Target724_genes, tmp, tt)
rownames(TARGET_NBL_FPKM_mData) <- as.character(TARGET_NBL_FPKM_mData$TARGET.ID)
TARGET_NBL_FPKM_mData$INSS.Stage <- sub('Stage ','',TARGET_NBL_FPKM_mData$INSS.Stage)
TARGET_NBL_FPKM_mData$MYCN.status <- ifelse(TARGET_NBL_FPKM_mData$MYCN.status == "Amplified","amplified","single_copy")
TARGET_NBL_FPKM_mData$COG.Risk.Group <- sub(' Risk','',TARGET_NBL_FPKM_mData$COG.Risk.Group)
colnames(TARGET_NBL_FPKM_mData)[c(14,15,24)] <- c('STAGE','MYCN','RISK')

TARGET_NBL_FPKM_All <- list(TARGET_NBL_FPKM_data, TARGET_NBL_FPKM_mData)
