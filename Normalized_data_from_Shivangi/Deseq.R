##Installing it
#source("http://bioconductor.org/biocLite.R")
#biocLite("DESeq")

##loading DESeq
.libPaths("/opt/R-3.4.2/library")
library("DESeq")
library(pheatmap)

#read the read counts file with headers.
countsTable <- read.table("miRNA_normal_vs_glio_input.csv",sep="\t",header=TRUE)
countsTable$Gene_ID = paste(countsTable$Gene_ID,1:nrow(countsTable),sep ="_")

##specify that the first column is the gene name.
rownames(countsTable) <- countsTable$Gene_ID
countsTable <- countsTable[,-1]
head(countsTable)

##specify the conditions
conditions <- factor(c(rep("Normal",3),rep("Glioblastoma",7)))

##create the main DESeq object
countDataSet <- newCountDataSet(countsTable,conditions)

##adjust for the difference in reads mapped to in each sample
countDataSet <- estimateSizeFactors(countDataSet)
sizeFactors(countDataSet)
head(counts(countDataSet))

normalized = counts(countDataSet,normalized=TRUE)
norm <- cbind("id"=rownames(normalized),normalized)

countDataSet <-estimateDispersions(countDataSet)

##plot dispersion
pdf("disp_DESeq.pdf")
plotDispEsts(countDataSet)
dev.off()

##all differential expression values
DEVal=nbinomTest(countDataSet,"Normal","Glioblastoma")

##plot the log2 fold changes (cancer v normal) against the mean normalised counts
pdf("plotMA_DESeq.pdf")
plotMA(DEVal,col = ifelse(DEVal$pval<=0.05, "gray32", "red"), linecol = "red")
dev.off()

##plot histogram of p values
pdf("hist_pVal_DESeq.pdf")
hist(DEVal$pval, breaks=100, col="skyblue", border="slateblue")
dev.off()

##plot Volcano
pdf("VolcanoPlot.pdf")
plot(DEVal$log2FoldChange, log10(DEVal$pval), cex = 0.5, col="blue")
dev.off()

##saving data
write.csv(DEVal,file="All_DE_miRNA_normal_vs_glio_output.csv")

## Filter from All_DE_miRNA_normal_vs_glio_ouput.csv based on Pval <= 0.05 and log2foldchange >= 2 and <= -2 using awk.
significant <- read.table("Significantly_DE_miRNA_normal_vs_glio_output.csv",sep="\t",header=TRUE)
geneFilt=merge(norm,significant,by="id")
gene_DE <- geneFilt[,-(12:18)]

rownames(gene_DE) <- gene_DE$id
gene_DE <- gene_DE[,-1]


gene <- as.matrix(gene_DE)
gene <- apply(gene_DE,2,as.numeric)
rownames(gene) <- (rownames(gene_DE))

##plot heatmap
pdf("HeatMap_sig.pdf")
heatmap(gene)
dev.off()


