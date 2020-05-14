
library(shiny)
library(shinydashboard)

#dashboard start

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "miRAGDB",
                                    dropdownMenu(
                                      type = "notifications",              #notification menu
                                      icon = icon("question-circle"),
                                      badgeStatus = NULL,
                                      headerText = "See also:",
                                      
                                      ## Notification item   
                                      
                                      notificationItem("How to Use", icon = icon("file"),
                                                       href = "http://shiny.rstudio.com/"),
                                      notificationItem("IBAB", icon = icon("file"),
                                                       href = "https://ibab.ac.in")
                                      
                                    ),  # 1 dropdown notification close
                                    
                                    #-----------------## 1 dropdown notification close ##-----------------------------------#
                                    
                                    dropdownMenu(
                                      type = "messages", 
                                      #icon = icon("question-circle"),
                                      badgeStatus = NULL,
                                      headerText = "See also:",
                                      messageItem(
                                        from = "Admin",
                                        message = "miRAGDB is Reday to use.")
                                    )
                    ),
                    
                    #-----------------## Header close ##-----------------------------------#
                    
                    #-----------------## Dashboard Sidebar Start Here ##-----------------------------------#
                    
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "dashboard", icon = icon("dashboard"),selected = TRUE),
                        menuItem("Mouse", tabName = "mouse", icon = icon("th")),
                        menuItem("Human", tabName = "human", icon = icon("th")),
                        menuSubItem("CLL", tabName = "cll", href = NULL, newtab = TRUE,
                                    icon = shiny::icon("angle-double-right"), selected = NULL),
                        menuSubItem("ALL", tabName = "all", href = NULL, newtab = TRUE,
                                    icon = shiny::icon("angle-double-right"), selected = NULL),
                        menuItem("RAG Regulator", tabName = "ragreg", icon = icon("th")),
                        menuItem("Circular RNA", tabName = "circrna", icon = icon("th")),
                        menuItem("MGI Data", tabName = "mgidatatab", icon = icon("th")),
                        menuItem("Download", tabName = "Download", icon = icon("th")),
                        menuItem("DATAVISualisation", tabName = "own", icon = icon("th")),
                        menuItem("Compare", tabName = "compare", icon = icon("th"))
                      
                      )
                      
                    ), 
                    
                    #-----------------## Dashboard Sidebar close ###--------------------------------------#
                    
                    #-----------------## Dashboard Main Body Start ###------------------------------------#
                    
                    dashboardBody(
                      tabItems(
                        
                        #-----------------## Dashboard Tab ###------------------------------------#
                        
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  infoBox("miRAG Database Entries",10*2, icon = icon("list")),
                                  infoBoxOutput("listmirna_mouse"),
                                  infoBoxOutput("listmirna_human")
                                ),
                                
                                fluidRow(
                                  box(title = "What is RAG1", status = "primary", solidHeader = T, collapsible = T, width=6,
                                      "
                                      The protein encoded by this gene is involved in activation of immunoglobulin
                                      V-D-J recombination. The encoded protein is involved in recognition of the DNA substrate, but stable 
                                      binding and cleavage activity also requires RAG2. The RAG-1/2 complex recognizes the Recombination 
                                      Signal Sequence (RSS) that flank the V, D and J regions in the gene that codes for the constant
                                      region of both the heavy chain and light chain in an antibody. The complex binds to the Recombination 
                                      Signal Sequences and nicks the DNA. This leads to the removal of the RSS and the eventual binding of 
                                      the V D and J sequences.[6] Defects in this gene can be the cause of several diseases",
                                      HTML('<br><br>'),
                                      tags$a(href="https://www.genenames.org/data/gene-symbol-report/#!/hgnc_id/HGNC:9831","HGNC(9831)"),
                                      HTML('&nbsp'),
                                      tags$a(href="https://www.ncbi.nlm.nih.gov/gene/5896","Entrez Gene(5896)"),
                                      HTML('&nbsp'),
                                      tags$a(href="https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=ENSG00000166349;r=11:36510709-36593156","Ensembl(ENSG00000166349))"),
                                      HTML('&nbsp'),
                                      tags$a(href="https://www.omim.org/entry/179615?search=RAG1&highlight=rag1","OMIM(l179615)"),
                                      HTML('&nbsp'),
                                      tags$a(href="https://www.uniprot.org/uniprot/P15918","UniProtKB(P15918))")),
                                  
                                  HTML('<p><img src="img.png", height=20%x, width=40%></p>')
                                ),
                                fluidRow(box(title = "Normal Function", status = "primary", solidHeader = T, collapsible = T, width=12,"
                                             
                                             The RAG2 gene provides instructions for making a member of a group of proteins called the RAG complex. This complex is active in immune system cells (lymphocytes) called B cells and T cells. These cells have special proteins on their surface that recognize foreign invaders and help protect the body from infection. These proteins need to be diverse to be able to recognize a wide variety of substances. The genes from which these proteins are made contain segments known as variable (V), diversity (D), and joining (J) segments. During protein production within lymphocytes, these gene segments are rearranged in different combinations to increase variability of the resulting proteins. The RAG complex is involved in this process, which is known as V(D)J recombination.
                                             
                                             During V(D)J recombination, the RAG complex attaches (binds) to a section of DNA called a recombination signal sequence (RSS), which is next to a V, D, or J segment. The RAG complex makes small cuts in the DNA between the segment and the RSS so the segment can be separated and moved to a different area in the gene. This process of DNA rearrangement within B cells and T cells is repeated multiple times in different areas so that the V, D, and J segments are arranged in various combinations. The variety of proteins produced throughout life following V(D)J recombination provides greater recognition of foreign invaders and allows the body to fight infection efficiently.
                                             "))
                                ),#dashboard tab
                        
                        #-----------------## Dashboard Tab close ###------------------------------------#
                        
                        #-----------------## Human Tab Start ###------------------------------------#
                        
                        ## First tab content
                        tabItem(tabName = "human",
                                h2("Human"),
                                
                                ## Table output and heatmap output for Human
                                
                                fluidRow(
                                  box(title = "Normal miRNA Expression B-Cell stages", status = "primary", solidHeader = TRUE, 
                                      collapsible = TRUE,
                                      DT::dataTableOutput("x1", height = 500)),
                                  
                                  box(title = "Normal miRNA Expression B-Cell stages", status = "primary", solidHeader = TRUE, 
                                      collapsible = TRUE,
                                      plotOutput("hsa_heat1", height = 500))                         
                                ),
                                ## Barplot output and input selection / table output for Human
                                fluidRow(
                                  box(title="Bar Plot", collapsible = TRUE, status="success", solidHeader=TRUE,width=12, footer = "Expression of each mirna in different stage
                                      of B Cell", 
                                      selectInput(inputId = 'id',
                                                  label = 'Select miRNA for Bar plot',
                                                  choices = unique(hsa[['id']])),
                                      box(title="Select miRNA to Show its Expression", collapsible = TRUE, status="success", solidHeader=TRUE,width=6, footer = "Expression of each mirna in different stage
                                          of B Cell", tableOutput("result")),
                                      box(title="Bar Plot ", collapsible = TRUE, status="success", solidHeader=TRUE,width=6, footer = "Expression of each mirna in different stage
                                          of B Cell",plotOutput("plot",height = 250)))
                                      ),
                                
                                ## Selectable tabeloutput and heatmap
                                
                                fluidRow(
                                  box(title = "Normal miRNA Expression B-Cell Stages", status = "warning", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("x2")),
                                  box(title="HeatMap of Selected Row", footer = "Expression of each miRNA", status = "warning", solidHeader=TRUE, collapsible = TRUE,plotOutput("x2_pheat", height = 450))
                                )
                                  ),#human tab
                        
                        #-----------------## Human Tab close ###------------------------------------#
                        
                        
                        #-----------------## Download Tab start ###------------------------------------#
                        
                        
                        ## Download tab content
                        tabItem(tabName = "Download",
                                h2("Download Section"),
                                fluidRow(
                                  infoBox("Download Mouse Dataset",HTML('<a href="hsa.csv">CLICK HERE</a>')),
                                  infoBox("Download Human Dataset",HTML('<a href="hsa.csv">CLICK HERE</a>')),
                                  infoBox("Download Whole Database",HTML('<a href="hsa.csv">CLICK HERE</a>'))
                                  # infoBoxOutput("prgressBox")
                                )
                        ),#download tab
                        
                        
                        #-----------------## Download Tab close ###------------------------------------#
                        
                        #-----------------## CLL Tab start ###------------------------------------#
                        
                        
                        ## CLL tab content
                        tabItem(tabName = "cll",
                                h2("Human CLL"),
                                fluidRow(
                                  box(title = "miRNA expression in CLL condition", status = "danger", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("cll_x5"),width = 12)),
                                fluidRow(box(title = "miRNA expression in CLL condition", status = "danger", solidHeader = TRUE, 
                                             collapsible = TRUE,plotOutput("heat_cll"),width = 12)),
                                fluidRow(box(title = "RAG expression in CLL condition", status = "danger", solidHeader = TRUE, 
                                             collapsible = TRUE,plotOutput("bar_rag_cll"),width = 12)),
                                fluidRow((box(title = "RAG expression in CLL condition", status = "danger", solidHeader = TRUE, 
                                              collapsible = TRUE,DT::dataTableOutput("cll_human_rag_tbl"),width = 12))
                                )),#cll tab close
                        
                        
                        #-----------------## cll Tab close ###------------------------------------#
                        
                        #-----------------## ALL Tab start ###------------------------------------#
                        
                        
                        ## ALL tab content
                        tabItem(tabName = "all",
                                h2("Human ALL"),
                                fluidRow(
                                  box(title = "miRNA expression in T-ALL condition", status = "danger", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("all_x4"),width = 12)
                                ),#all table tab
                                fluidRow(
                                  box(title = "miRNA expression in T-ALL condition", status = "danger", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("heat_all"),width = 12)),
                                
                                fluidRow(
                                  box(title = "RAG expression in T-ALL condition", status = "danger", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("bar_rag_all"),width = 12)),
                                fluidRow(
                                  box(title = "RAG expression in T-ALL condition", status = "danger", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("all_human_rag_tbl"),width = 12))                    
                                
                        ),#all heatmap tab
                        
                        #-----------------## ALL Tab close ###------------------------------------#
                        
                        #-----------------## Mouse Tab start ###------------------------------------#
                        
                        
                        # Third tab content
                        tabItem(tabName = "mouse",
                                h2("Mouse"),
                                fluidRow(
                                  box(title = "Normal miRNA Expression T-Cell Stages", status = "warning", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("x3")),
                                  box(title = "Normal miRNA Expression T-Cell Stages", status = "warning", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("heat_mmu", height = 700))
                                ),
                                fluidRow(
                                  box(title = "RAG Expression in Normal Condition T-Cell Stages", status = "info", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("nrm_mouse_rag_table"),width = 12)),
                                fluidRow(
                                  box(title = "RAG Expression in Normal Condition T-Cell Stages", status = "info", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("nrm_mouse_rag_bar"),width = 12)),
                                
                        
                        fluidRow(
                          box(title = "RAG Expression in B Cell", status = "info", solidHeader = TRUE, 
                              collapsible = TRUE,DT::dataTableOutput("mmu_bcell_nrm_tbl"),width = 6),
                          box(title = "RAG Expression in B Cell", status = "info", solidHeader = TRUE, 
                              collapsible = TRUE,plotOutput("heat_mmu_bcell_nrm"),width = 6,height = 550))
                        
                        ),
                        
                        #mouse tab
                        
                        
                        #-----------------## Mouse Tab close ###------------------------------------#
                        
                        #-----------------## Rag Regulator Tab Start ###------------------------------------#
                        
                        
                        tabItem(tabName = "ragreg",
                                h2("RAG Regulators"),
                                fluidRow(
                                  box(title = "RAG Regulator and Expression in B-Cell stages", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("rag_reg_bcell")),
                                  box(title = "RAG Regulator and Expression in B-Cell stages", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("rag_reg_bcell_heat", height = 500))
                                ),
                                fluidRow(
                                  box(title = "RAG Regulator and their Expression in CLL Patient Samples", status = "danger", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("rag_reg_cll"),width = 12)),
                                fluidRow(
                                  box(title = "RAG Regulator and their Expression CLL Patient Samples", status = "danger", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("rag_reg_cll_heat"),width = 12))
                                
                        ),
                        
                        
                        #-----------------## RegReg Tab close ###------------------------------------#
                        
                        #-----------------## Circrna Tab start ###------------------------------------#
                        
                        
                        tabItem(tabName = "circrna",
                                h2("Circular RNA"),
                                fluidRow(
                                  box(title="Circular RNA", collapsible = TRUE, status="success", solidHeader=TRUE, width=12, footer = "Check me", 
                                      selectInput("circrna", "Choose miRNA", choices = unique(circrna['miRNA'])))),
                                fluidRow(
                                  box(title = "Circualar RNA", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("circtable"),width = 12))
                        ),
                        
                        
                        
                        #-----------------## circrna Tab close ###------------------------------------#
                        
                        #-----------------## mgidatatab Tab start ###------------------------------------#
                        
                        
                        
                        
                        tabItem(tabName = "mgidatatab",
                                h2("MGI DATA"),
                                fluidRow(
                                  box(title = "MGI Data", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("mgidatatable"),width = 12))),
                        
                        tabItem(tabName = "own",
                                h2("Your Data"),
                                fluidRow(
                                  box(width = 12,
                                  fileInput("file1", "Choose CSV File",     accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                  tags$hr(),
                                  tags$hr(),
                                  checkboxInput('header', 'Header', TRUE),
                                  radioButtons('sep', 'Separator',
                                               c(Comma=',',
                                                 Semicolon=';',
                                                 Tab='\t'),
                                               'Comma')
                                  
                                 )),
                                fluidRow(
                                  box(title = "File Data", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("contents"),width = 12)),
                                fluidRow(
                                  box(title = "File Data", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("contents_plot"),width = 12))
                                ),
                        
                        tabItem(tabName = "compare",
                                h2("Compare Data"),
                                fluidRow(
                                  box(width = 12,
                                      selectInput(inputId = 'miid',
                                                  label = 'Select miRNA to Compare',
                                                  choices = unique(cll_human_miR[['miRNA']]))
                                      )),
                                fluidRow(
                                  box(title = "Normal miRNA Expression Human Normal Samples", status = "success", solidHeader = TRUE, 
                                  collapsible = TRUE,DT::dataTableOutput("compare1"),width = 12)),
                                fluidRow(
                                  box(title = "Normal Expression Human Normal Patient Samples", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("compareheat1"),width = 12)),
                                fluidRow(
                                  box(title = "ALL Condition miRNA Expression in Patient Samples", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("compare2"),width = 12)),
                                fluidRow(
                                  box(title = "ALL Condition miRNA Expression in Patient Samples", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,plotOutput("compareheat2"),width = 12)),
                                fluidRow(
                                  box(title = "CLL Condition miRNA Expression in Patient Samples", status = "success", solidHeader = TRUE, 
                                      collapsible = TRUE,DT::dataTableOutput("compare3"),width = 12)),
                                  fluidRow(
                                    box(title = "CLL Condition miRNA Expression in Patient Samples", status = "success", solidHeader = TRUE, 
                                        collapsible = TRUE,plotOutput("compareheat3"),width = 12))
                        )
                        )#dashboard
                        )#tabitems
                        )#dashboard page end


# fluidRow(
#  box(title = "CircualarRNA", status = "success", solidHeader = TRUE, 
#      collapsible = TRUE,dataTableOutput("uidf"),width = 6)
# ),
# fluidRow(
#box(title = "circ expression database", status = "info", solidHeader = TRUE, 
#  collapsible = TRUE,htmlOutput("url"),width = 12))
# )
