hsa<-read.csv("normal_mirna_HUMAN/hsa.csv", stringsAsFactors = FALSE,sep = '\t')
hsa <-data.frame(hsa)
mmu_bcell_nrm<-read.csv("./miRNA_mouse_normal_tar/mmu_tar.csv", stringsAsFactors = FALSE,sep = '\t')
mmu_bcell_nrm <-data.frame(mmu_bcell_nrm)
mmu_tar <-read.csv("NORMAL_RAG_MOUSE/mirna/round2.csv", stringsAsFactors = FALSE,sep = '\t')
mmu <-data.frame(mmu_tar)
all_human_miR <-read.csv("TLL/TALL_RAG.miRs.csv",stringsAsFactors = FALSE,sep = ',')
all_human_miR <- data.frame(all_human_miR)
all_human_rag <-read.csv("TLL/TALL_RAG1.csv",stringsAsFactors = FALSE,sep = ',')
all_human_rag <- data.frame(all_human_rag)
cll_human_miR <-read.csv("CLL/CLL.RAG.miRs.tsv",stringsAsFactors = FALSE,sep = '\t')
cll_human_miR <- data.frame(cll_human_miR)
cll_human_rag <-read.csv("CLL/CLL_Rag1_29c-3p_expression.csv",stringsAsFactors = FALSE,sep = ',')
cll_human_rag <- data.frame(cll_human_rag)
nrm_mouse_Rag1 <-read.csv("NORMAL_RAG_MOUSE/rag/mouse_normal_Tcell_RAG1.csv", stringsAsFactors = FALSE,sep = ',')
nrm_mouse_rag1 <-data.frame(nrm_mouse_Rag1)
nrm_mouse_rag1 <- data.frame(nrm_mouse_rag1[,-1])
nrm_mouse_rag1 <-log(nrm_mouse_rag1, base = exp(2))
reg_rag_cll <- read.csv("regulator/CLL.RAG1_and_its_regulators.csv", stringsAsFactors = FALSE,sep = ',')
reg_rag_cll <- data.frame(reg_rag_cll)
reg_rag_bcell <- read.csv("regulator/Human.Bcell.stages.Rag1_regulators.csv", stringsAsFactors = FALSE,sep = ',')
reg_rag_bcell <- data.frame(reg_rag_bcell)
circrna <- read.table("sponge_candidates.tsv",header = TRUE, stringsAsFactors = FALSE,sep = '\t' )
circrna_nrm <- read.csv("Normalized_data_from_Shivangi/circRNA_normalized_readcount.csv", stringsAsFactors = FALSE,sep = ',' )
circrnadata <- read.table("sponge_candidates.tsv", stringsAsFactors = FALSE,sep = '\t')
mgidata <- read.table("mgidata/mgi_expr_data.csv",header = TRUE, stringsAsFactors = FALSE,sep="\t")

#common <- intersect(all_human_miR$miRNA, cll_human_miR$miRNA) 


#names2<-colnames(nrm_mouse_rag[,-1])
#up <- c(as.numeric(nrm_mouse_rag[1,2:12]))
#name2 <- colnames(up)
#down<-c(as.numeric(nrm_mouse_rag[4,2:12]))
#name1 <- colnames(down)
#df <-data.frame(up,down)
#x=c("sp","Sp1")
#names(df)<-c(names2,names2)
#barplot(as.matrix(down),cex.names=0.6, las=2,beside=T)
#barplot(as.matrix(up),cex.names=0.6, las=2,beside = T)
#barplot(as.matrix(df),beside = T,cex.names= 0.6)
#nrm_mouse_rag2 <- nrm_mouse_rag2[,1:9]
#mmu <- format(round(mmu[-1,-1],2),nsmall=2)
#tll_human <- format(round(tll_human[-1,-1],2),nsmall=2)
#on.exit(dbDisconnect(conn), add = TRUE)


# inputcircrna <- "hsa-miR-7-5p"
# circtable <- circrna[circrna$miRNA==inputcircrna,]
# circtable_circ <- data.frame(circtable[,4])
# names(circtable_circ)[1]="circrna"
#circtable_circ <- circtable_circ %>% rename( circrna = circtable...4.)
#circtable_circ
#circtable
# circrna_nrm <- read.csv("Normalized_data_from_Shivangi/circRNA_normalized_readcount.csv", stringsAsFactors = FALSE,sep = ',' )
# circdf=merge(circtable_circ,circrna_nrm,by="circrna")
#circrna_nrm[1:5,]
#circtable_circ[1:5,]
#if(dim(circdf)[1] == 0){print("No match in Expression Database")}

