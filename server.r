library(shiny)
library(pheatmap)
library(ggplot2)
library(DT)
library(BiocManager)
library(dplyr)
#library(tidyverse)
source("global.r")

server <- function(input, output) { 

  
  ##-------------------------------------##
  
  ## Render information Box (Dashboard)
  output$listmirna_mouse <- renderInfoBox({
    infoBox(
      "MOUSE", paste0(10," : miRNA associated with RAG"), icon=icon("list"), color="teal",fill = TRUE)
  })
  
  output$listmirna_human <- renderInfoBox({
    infoBox(
      "Human", paste0(10,"  :miRNA associated with RAG"), icon=icon("thumbs-up",lib="glyphicon"), color="yellow",fill=TRUE)
  })
  
  
  ##-------------------------------------##  
  ## Output all the tabels
  
  output$x1 <- DT::renderDataTable(hsa,server = FALSE)
  output$x3 <- DT::renderDataTable(mmu,server = FALSE,options = list(scrollX = TRUE))
  output$all_x4 <- DT::renderDataTable(all_human_miR,server = FALSE,options = list(scrollX = TRUE))
  output$cll_x5 <- DT::renderDataTable(cll_human_miR,server = FALSE,options = list(scrollX = TRUE))
  output$nrm_mouse_rag_table <-DT::renderDataTable(nrm_mouse_rag1,server = FALSE,options = list(scrollX = TRUE))
  output$rag_reg_cll <- DT::renderDataTable(reg_rag_cll,server = FALSE,options = list(scrollX = TRUE))
  output$rag_reg_bcell <- DT::renderDataTable(reg_rag_bcell,server = FALSE,options = list(scrollX = TRUE))
  output$cll_human_rag_tbl <- DT::renderDataTable(cll_human_rag,server = FALSE,options = list(scrollX = TRUE))
  output$all_human_rag_tbl <- DT::renderDataTable(all_human_rag,server = FALSE,options = list(scrollX = TRUE))
  output$mmu_bcell_nrm_tbl <- DT::renderDataTable(mmu_bcell_nrm,server = FALSE,options = list(scrollX = TRUE))
  
   ##-------------------------------------##
  
  ## Output / Selection tabel for heatmap
  
  output$x2 <- DT::renderDataTable(hsa,server = TRUE)

  
  ##-------------------------------------##
  
  ## Render tabel according input id (search bar)  
  output$result <- renderTable(hsa[hsa$id== input$id,])
  
  ##-------------------------------------##
  
  ## Render Plot for Human
  
  output$hsa_heat1 <- renderPlot({
    out4 <- data.frame(hsa[,])
    options(viewer = NULL)
    #dev.off()
    colnames(out4) <- c("id","Cen","PreG","Pls","Nav","Mem")
    rownames(out4) <-out4$id
    pheatmap(out4[,-1],
             scale = "row", 
             clustering_rows=FALSE, 
             cluster_cols=FALSE, 
             color = colorRampPalette(c("green","red"))(50),
             margins=c(3,25),fontsize = 12)
    #   dev.off()
  })
  
  ##-------------------------------------##
  
  ## Render Plot for mouse
  
  output$heat_mmu <- renderPlot({
    out_mmu <- data.frame(mmu[,])
    options(viewer = NULL)
    #dev.off()
    colnames(out_mmu) <- c("miRNA","DP","SP_CD4","SP_CD8","SP_nTreg","Th1","Th2","TH17")
    rownames(out_mmu) <-out_mmu$id
    pheatmap(out_mmu[,-1],
             scale = "row", 
            # clustering_rows=FALSE, 
             #cluster_cols=FALSE, 
             color = colorRampPalette(c("green","red"))(50),
             margins=c(3,25),fontsize = 12,show_rownames = TRUE)
  }) 
  output$plot <- renderPlot({
    options(viewer = NULL)
    #dev.off()
    out2 <- hsa[hsa$id== input$id,]
    barplot(as.matrix(out2[,2:6]))
  })
  
  ##-------------------------------------##
  ## Render Heatmap for mmu tsv ##
  
  output$heat_mmu_bcell_nrm <- renderPlot({
    out_mmu_bcell_nrm <- data.frame(mmu_bcell_nrm[,])
    options(viewer = NULL)
    #dev.off()
    colnames(out_mmu_bcell_nrm) <- c("miRNA_id","proB","preB","immatureB","matureB")
    rownames(out_mmu_bcell_nrm) <-out_mmu_bcell_nrm$miRNA_id
    pheatmap(out_mmu_bcell_nrm[,-1],
             scale = "row", 
             # clustering_rows=FALSE, 
             #cluster_cols=FALSE, 
             color = colorRampPalette(c("green","red"))(50),
             margins=c(3,25),fontsize = 12,show_rownames = TRUE)
  }) 
  
  ##---------------------------##
  ## Render plot according input Id (bar plot)
 
  
  ##-------------------------------------##  
  
  ## Render plot according to selection in table x2
  
  output$x2_pheat <- renderPlot({   
    out4 <- data.frame(hsa[input$x2_rows_selected,])
    options(viewer = NULL)
 # dev.off()
    colnames(out4) <- c("id","Cen","PreG","Pls","Nav","Mem")
    rownames(out4) <-out4$id
    pheatmap(out4[,-1],
             scale = "row", 
             clustering_rows=FALSE, 
             cluster_cols=FALSE, 
             color = colorRampPalette(c("black","red"))(50),
             margins=c(3,25),fontsize = 12)
  })
  
  ##-------------------------------------##
  
  
  ## Render Plot for all
  output$heat_all <- renderPlot({
    all_human_miR <- data.frame(all_human_miR[,])
    options(viewer = NULL)
   #dev.off()
    colnames(all_human_miR) <- c("miRNA","P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13",
                           "P14","P15","P16","P17","P18","P19","P20","P21","P22","P23","P24","P25",
                           "P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37",
                           "P38","P39","P40","P41","P42","P43","P44","P45","P46","P47","P48")
    rownames(all_human_miR) <-all_human_miR$miRNA
    pheatmap(all_human_miR[,-1],
             scale = "none", 
            # clustering_rows=FALSE, 
            # cluster_cols=FALSE, 
             color = colorRampPalette(c("green","red"))(50),
             margins=c(3,25),fontsize = 12,show_rownames = TRUE)
  })

  
  ##-------------------------------------##
  
  ## Render Plot for cll
  output$heat_cll <- renderPlot({
    #cll_human_miR <- data.frame(cll_human_miR[,])
    row.names(cll_human_miR) <- cll_human_miR[,1]
    options(viewer = NULL)
   #dev.off()
    colnames(cll_human_miR) <- c("miRNA","CLL_1","CLL_2","CLL_3","CLL_4","CLL_5","CLL_6","CLL_7","CLL_8","CLL_9","CLL_10","CLL_11","CLL_12","CLL_13","CLL_P14","CLL_15")
    pheatmap(cll_human_miR[,2:14],
             scale = "none", 
             #cluster_rows=TRUE, 
            # cluster_cols=FALSE, 
             color = colorRampPalette(c("white","red"))(50),
             margins=c(3,25),fontsize = 12,show_rownames = TRUE)
  })
  ##-------------------------------------##
  
  ## Render bar Plot for all
  output$bar_rag_all <- renderPlot({
    # Render a barplot
    barplot(as.matrix(all_human_rag[,2:47]),
            main="RAG",
            ylab="Expression",
            xlab="ALL : RAG")
  })
  
  ##-------------------------------------##
  
  ## Render bar Plot for cll
  ## Render bar Plot for all
  output$bar_rag_cll <- renderPlot({
    # Render a barplot
    barplot(as.matrix(cll_human_rag[2,2:15]),
            main="RAG",
            ylab="Expression",
            xlab="CLL : RAG")
  })

  output$nrm_mouse_rag_bar<- renderPlot({
    barplot(as.matrix(nrm_mouse_rag1),cex.names=0.6,las=2,main = "RAG",ylab = "logValues",xlab = "Samples",
          col = topo.colors(n = 12,alpha = 1),cex.lab=1)})


  ## Render Plot for Bcell RAG Regulator
  output$rag_reg_bcell_heat <- renderPlot({
    #cll_human_miR <- data.frame(cll_human_miR[,])
    row.names(reg_rag_bcell) <- reg_rag_bcell[,1]
    options(viewer = NULL)
    #dev.off()
    colnames(reg_rag_bcell) <- c("Ensemble_ID","EarlyB_1","EarlyB_2","ProB_1","ProB_2","PreB_1","PreB_2
","ImmatureB_1","immatureB_2","EarlyB_ped_1","EarlyB_ped_2","proB_ped_1","ProB_ped_2","PreB_ped_1","PreB_ped_2","ImmatureB_ped_1","ImmatureB_ped_2")
    pheatmap(reg_rag_bcell[,2:14],
             scale = "none", 
             #cluster_rows=TRUE, 
             # cluster_cols=FALSE, 
             color = colorRampPalette(c("green","red"))(50),
             margins=c(3,25),fontsize = 12,show_rownames = TRUE)
  })
  ##-------------------------------------##
  

  ## Render Plot for cll RAG regulator
  output$rag_reg_cll_heat <- renderPlot({
    #reg_rag_cll <- data.frame(reg_rag_cll[,])
    row.names(reg_rag_cll) <- reg_rag_cll[,1]
    options(viewer = NULL)
    #dev.off()
    colnames(reg_rag_cll) <- c("Gene","CLL_1","CLL_2","CLL_3","CLL_4","CLL_5","CLL_6","CLL_7","CLL_8","CLL_9","CLL_10","CLL_11","CLL_12","CLL_13","CLL_P14","CLL_15")
    pheatmap(reg_rag_cll[,2:14],
             scale = "none", 
             #cluster_rows=TRUE, 
             # cluster_cols=FALSE, 
             color = colorRampPalette(c("green","red"))(50),
             margins=c(3,25),fontsize = 12,show_rownames = TRUE)
  })
  ##-------------------------------------##

  output$circtable <- DT::renderDataTable(
    circrna[circrna$miRNA==input$circrna,], escape=FALSE,server = FALSE,options = list(scrollX = TRUE))
  
  
  
        output$compare1 <- DT::renderDataTable(hsa[hsa$id== input$miid,],server = FALSE,options = list(scrollX = TRUE))
        output$compare2 <-  DT::renderDataTable(all_human_miR[all_human_miR$miRNA== input$miid,],server = FALSE,options = list(scrollX = TRUE)) 
        output$compare3 <- DT::renderDataTable(cll_human_miR[cll_human_miR$miRNA == input$miid,],server = FALSE,options = list(scrollX = TRUE))

        
        output$compareheat1 <- renderPlot({
          options(viewer = NULL)
          #dev.off()
          outcompare1 <- hsa[hsa$id== input$miid,]
          barplot(as.matrix(outcompare1[,2:6]))
        })
        output$compareheat2 <- renderPlot({
          options(viewer = NULL)
          #dev.off()
          outcompare2 <- all_human_miR[all_human_miR$miRNA== input$miid,]
          barplot(as.matrix(outcompare2[,2:6]))
        })
        output$compareheat3 <- renderPlot({
          options(viewer = NULL)
          #dev.off()
          outcompare3 <- cll_human_miR[cll_human_miR$miRNA== input$miid,]
          barplot(as.matrix(outcompare3[,2:6]))
        })
        
        
  output$mgidatatable <- DT::renderDataTable(
    mgidata, escape=FALSE,server = FALSE,options = list(scrollX = TRUE))
  
  
  
   mydata <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
      return(NULL)
        tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
        return(tbl)
  })
  
  output$contents <- DT::renderDataTable(options = list(scrollX = TRUE),{
    mydata()
  })
  
  output$contents_plot <- renderPlot({
    pheatmap(data.matrix(mydata()[,2:10]),
           scale = "row", 
             color = colorRampPalette(c("green","red"))(50),
             margins=c(3,25),fontsize = 12,show_rownames = TRUE)
  })
  
  
  
  
 # output$uidf<- DT::renderDataTable(
      #dfchr<-circrna[circrna$miRNA==input$circrna,5],
    #df2data <-circrna[circrna$miRNA==input$circrna,6],
   #query <- data.frame("http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&org=human&position=",
                     #  circrna[circrna$miRNA==input$circrna,6],
                       "&hgt.customText=https://rth.dk/resources/mirnasponge/data/hg19_tracks/"
                     #  input$circrna,"_",circrna[circrna$miRNA==input$circrna,4], options = list(scrollX = TRUE))
   
   # )
  

  #output$uidf <- DT::renderDataTable(circrna, escape=FALSE,server = FALSE,options = list(scrollX = TRUE))
  
 
  
  # query <- gsub("-", "_", query),
   #query <- gsub(":", "_", query))
  
  
  #circrna <- read.csv("sponge_candidates.tsv", stringsAsFactors = FALSE,sep = '\t' )
 # inputcircrna <- "hsa-miR-7-5p"
  
  #  output$frame <- renderUI({
  #    datafram<-reactive({circrna[circrna$miRNA==input$circrna,]})
  #    query <- str_sub(datafram$Coordinate, 1, str_length(datafram$Coordinate)-3)
  #    query <- gsub("-", "_", query)
  #    query <- gsub(":", "_", query)
  #    test <<- paste0("http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg19&org=human&position=",datafram$Chromosome[1],"&hgt.customText=https://rth.dk/resources/mirnasponge/data/hg19_tracks/",input$circrna,"_",query[1])  
      
      # tags$iframe(src=test, height=500, width=1150)
     # observeEvent(circtable1,{
      ##    output$url <-renderUI(a(href=paste0('https://www.google.com/maps/place/',circtable$Coordinate[2]),"Show in Google Map",target="_blank"))
       #  })
      
   # })
    
    
  
 # circrna <- read.csv("sponge_candidates.tsv", stringsAsFactors = FALSE,sep = '\t' )
  
  
  
  
  #circrna <- data.frame(circrna)
  #inputcircrna <- "hsa-miR-7-5p"
  #circtable_find <- data.frame(circrna[circrna$miRNA==inputcircrna,])
 # observeEvent(input$showU,{
  #  output$url <-renderUI(a(href=paste0('https://www.google.com/maps/place/', input$state),"Show in Google Map",target="_blank"))
 # })
 # output$circdf<- DT::renderDataTable({
  #  observeEvent(
      
      #circdf<- circrna[circrna$miRNA==inputcircrna,],
      #circdf <- data.frame(circtable[,4]),
      #names(circdf)[1]="circrna",
     # circdf <- circdf %>% rename( circrna = circtable...4.),
     # circdf<-as.matrix(as.data.frame(merge(circdf,circrna_nrm,by="circrna")))
      
      #circtable1 <- data.frame(circtable[,4]),
      #names(circtable)[1]="circrna",
     # output$circdf<-merge(circtable_circ,circrna_nrm,by="circrna"),
     # if(dim(circdf)[1]==0){circdf<-data.frame(c("input$circrna","NOT FOUND"))}
      #cirtable 
  #  )
#  })
#
  # circtable <- circrna[circrna$miRNA==input$circrna,]
  #if(dim(circdf)[1] == 0){print("No match in Expression Database")}

  
  
  }

