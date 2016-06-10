library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(reshape2)
library(ggplot2)
options(gsubfn.engine = "R")
library(sqldf)
library(limma)
library(gridExtra)
library(shinyIncubator)
library(survival)

dashboardPage(
  
  # dashboardHeader begins
  dashboardHeader(title = 'Maris\' Lab Web Portal', titleWidth = 400), # dashboardHeader ends
  
  # dashboardSidebar begins
  dashboardSidebar(width = 400,
                   
    # enable vertical scrolling
    div(style="overflow-y: scroll"),
    
    # sidebarMenu begin
    sidebarMenu(
      
      menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
      menuItem("Resources", icon = icon("database"), tabName = "rdb",
               menuSubItem("Internal", icon = icon("database"), tabName = "rdbi"),
               menuSubItem("External", icon = icon("database"), tabName = "rdbe")
      ),
      menuItem("Cell Lines Database", icon = icon("database"), tabName = "cldb"),
      menuItem("Visualization Tools", tabName = "celllines", icon = icon("gears"),
               menuSubItem("Gene Expression", icon = icon("bar-chart"), tabName = "clge"),
               menuSubItem("Gene/Gene Correlation", icon = icon("line-chart"), tabName = "clggc"),
               menuSubItem("Gene Copy Number plot", icon = icon("bar-chart"), tabName = "clgcn"),
               menuSubItem("CNA vs mRNA plot", icon = icon("line-chart"), tabName = "clcvm"),
               menuSubItem("Cell Line Heatmap", icon = icon("th"), tabName = "clh")
      ),
      menuItem("Analysis Tools", tabName = "microarray", icon = icon("gears"),
               menuSubItem("Mutation Table", icon = icon("table"), tabName = "clm"),
               menuSubItem("Cell Line Comparison Tool", icon = icon("table"), tabName = "clct")
      ),
      menuItem("Patient Sample Tools", tabName = "patientsamples", icon = icon("gears"),
               menuSubItem("Patient Gene Expression Histogram", icon = icon("bar-chart"), tabName = "pgeh"),
               menuSubItem("Patient Gene Expression Box Plot", icon = icon("bar-chart"), tabName = "pgebp"),
               menuSubItem("Patient Kaplan-meier", icon = icon("line-chart"), tabName = "pkm"),
               menuSubItem("Patient Gene/Gene Correlation", icon = icon("line-chart"), tabName = "pggc"),
               menuSubItem("Patient Most Correlated Genes", icon = icon("table"), tabName = "pmcg"),
               menuSubItem("Patient Gene Copy Number", icon = icon("bar-chart"), tabName = "pgcn"),
               menuSubItem("Patient Gene CNA vs mRNA", icon = icon("line-chart"), tabName = "pgcvm")
      ),
      menuItem("GTEx Comparison Tools", tabName = "targetdata", icon = icon("gears"),
               menuSubItem("Tumor vs Normal Boxplot", icon = icon("bar-chart"), tabName = "tvnb"),
               menuSubItem("Tumor vs Normal Boxplot Abstracted", icon = icon("bar-chart"), tabName = "tvnba")
      ),
      menuItem("Compendia Analysis", tabName = "compendiaanalysis", icon = icon("gears"),
               menuSubItem("Aim 3 Overexpression Analysis", icon = icon("table"), tabName = "aoa"),
               menuSubItem("Aim 3 Transmembrane Protein Calls", icon = icon("table"), tabName = "atpc"),
               menuSubItem("Aim 3 Tumor Normal Differential Analysis", icon = icon("table"), tabName = "atnda")
      ),
      menuItem("Other Tools", tabName = "analysistools", icon = icon("gears"),
               menuSubItem("Venn Diagrams (Ext)", icon = icon("pie-chart"), href = "http://bioinfogp.cnb.csic.es/tools/venny"),
               menuSubItem("Gene Set Enrichment", icon = icon("star"), tabName = "gse"),
               menuSubItem("IC50 Analysis", icon = icon("star"), tabName = "ia")
      ),
      menuItem("More Info", tabName = "moreinfo", icon = icon("th"),
               menuSubItem("Readme", icon = icon("navicon"), tabName = "readme"),
               menuSubItem("About", icon = icon("info-circle"), tabName = "about"),
               menuSubItem("Contact", icon = icon("envelope"), tabName = "contact")
      )
    ) # sidebarMenu
    ), # dashboardSidebar

  dashboardBody(
    div(style="overflow-x: scroll"),
    
    # tabItems begins
    tabItems(
      
      # dashboard content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Maris Lab", status = "danger", width = 12, solidHeader = TRUE, "Tools, analysis, and visualizations to support research on Neuroblastoma and other pediatric cancers.", br(), br(), actionButton(inputId='ab1', label="Learn More", icon = icon("th")))
              ),
              fluidRow(
                box(title = "Cell Lines", status = "warning", width = 4, collapsible = T, collapsed = F, solidHeader = TRUE, "Tools and visualizations to support finding a cell line or set of cell lines that expressed a particular gene or pathway, looking at relevent correlations between genes, and examining cell line mutation. Currently internal Neuroblastoma cell line data is used but in the future data from CLE and Sanger will be imported.", br(), br(), actionButton(inputId='ab2', label="View Details", icon = icon("th"))),
                box(title = "Patient Data", status = "warning", width = 4, collapsible = T, collapsed = F, solidHeader = TRUE, "Visualizations and tools to analyze patient data in multiple ways. One can look at Gene Expression across cohorts, kaplan-meier curves based on a gene or set of genes, most correlated genes, etc... Currently two public data sets are included, in the future, our internal data set and other relevent data can be displayed.", br(), br(), actionButton(inputId='ab3', label="View Details", icon = icon("th"))),
                box(title = "Analysis Tools", status = "warning", width = 4, collapsible = T, collapsed = F, solidHeader = TRUE, "Analytical generic bioinformatics tools such as Gene Set Enrichment Analysis, IC50 Analysis, Drug Synergy Analysis, etc... Starred tools are being prepped for production and will be incorporated shortly.", br(), br(), actionButton(inputId='ab4', label="View Details", icon = icon("th")))
              ),
              DT::datatable(data = get(load('data/data_summary.RData')))
      ),
      
      ######## Resources ###########
      # rdbi content
      tabItem(tabName = "rdbi",
              DT::datatable(data = read.csv('data/internal.txt'), escape = FALSE)
              #DT::dataTableOutput(outputId = "rdbitable1")
      ),
      
      tabItem(tabName = "rdbe",
              DT::datatable(data = read.csv('data/external.txt'), escape = FALSE)
      ),
      ######## Resources ###########
      
      # cldb content
      tabItem(tabName = "cldb",
              fluidRow(
                box(selectInput(inputId = 'cldbselectInput1', label = 'Select dataset',choices = c('Neuroblastoma RNAseq data'='rnaseqcelllinedata.RData',
                                                                                             'Neuroblastoma Microarray data'='celllinedata.RData')), width = 5, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'cldbsubmit1', label = "Load dataset"))), br(), br(),
              DT::dataTableOutput(outputId = "cldbtable1")
      ),
      
      ##### Cell Lines Utilities #####
      # clge content
      tabItem(tabName = "clge",
              fluidRow(
                box(selectInput(inputId = "clgeselectInput1", label = "Select dataset", choices = c('Neuroblastoma RNAseq data'='rnaseqcelllinedata.RData',
                                                                                                    'Neuroblastoma Microarray data'='celllinedata.RData')),
                    actionButton(inputId = "clgesubmit1", label = "Load dataset"), width = 4, background = "navy"),
                box(selectInput(inputId = "clgeselectInput2", label = "Select Gene", choices = "none"), width = 3, background = "navy"),
                box(checkboxInput(inputId = "clgecheckboxInput1", label = "Log", value = TRUE), width = 2, background = "navy"),
                box(selectInput(inputId = "clgeselectInput3", label = "Sort by", choices = c('Variable', 'Value')), width = 3, background = 'navy')
              ),
              fluidRow(column(5, actionButton(inputId = 'clgesubmit2', label = "Get Expression Plot"))), br(), br(),
              plotlyOutput(outputId = "clgeplot1", width = 800, height = 800)
      ),
      
      # clggc content
      tabItem(tabName = "clggc",
              fluidRow(
                box(selectInput(inputId = "clggcselectInput1", label = "Select dataset", choices = c('Neuroblastoma RNAseq data'='rnaseqcelllinedata.RData',
                                                                                                     'Neuroblastoma Microarray data'='celllinedata.RData')),
                    actionButton(inputId = "clggcsubmit1", label = "Load dataset"), width = 4, background = "navy"),
                box(selectInput(inputId = "clggcselectInput2", label = "Select Gene 1", choices = "none"), 
                    selectInput(inputId = "clggcselectInput3", label = "Select Gene 2", choices = "none"), width = 3, background = "navy"),
                box(checkboxInput(inputId = "clggccheckboxInput1", label = "Log", value = TRUE), width = 2, background = "navy"),
                box(selectInput(inputId = "clggcselectInput4", label = "Correlation", choices = c('Pearson' = 'pearson', 'Spearman' = 'spearman')), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clggcsubmit2', label = "Get Correlation Plot"))), br(), br(),
              plotlyOutput(outputId = "clggcplot1", width = 800, height = 800)
      ),
      
      # clm content
      tabItem(tabName = "clm",
              fluidRow(
                box(selectInput(inputId = "clmselectInput1", label = "Select dataset", choices = c("Exome Calls 85K8"="ExomeCalls85K8_10_13.txt")),
                    actionButton(inputId = "clmsubmit1", label = "Load dataset"), width = 3, background = "navy"),
                box(selectInput(inputId = "clmselectInput2", label = "Select Gene", choices = "none"), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clmsubmit2', label = "Get Mutation Table"))), 
              br(), br(),
              DT::dataTableOutput(outputId = 'clmtable1')
      ),
      
      # clgcn content
      tabItem(tabName = "clgcn",
              fluidRow(
                box(selectInput(inputId = "clgcnselectInput1", label = "Choose dataset", choices = c('Neuroblastoma Microarray data'='celllinedata')),
                    actionButton(inputId = "clgcnsubmit1", label = "Load dataset"), width = 3, background = "navy"),
                box(selectInput(inputId = "clgcnselectInput2", label = "Select Gene", choices = "none"), width = 3, background = "navy"),
                box(selectInput(inputId = "clgcnselectInput3", label = "Sort by", choices = c('Variable', 'Value')), width = 3, background = 'navy')
              ),
              fluidRow(column(5, actionButton(inputId = 'clgcnsubmit2', label = "Get Copy Number Barplot"))), 
              br(), br(),
              plotlyOutput(outputId = 'clgcnplot1', width = 800, height = 800)
      ),
      
      # clcvm content
      tabItem(tabName = "clcvm",
              fluidRow(
                box(selectInput(inputId = "clcvmselectInput1", label = "Choose dataset", choices = c('Neuroblastoma Microarray data'='celllinedata'), selected = NULL),
                    actionButton(inputId = "clcvmsubmit1", label = "Load dataset"), width = 3, background = "navy"),
                box(selectInput(inputId = "clcvmselectInput2", label = "Select Gene", choices = "none"), width = 3, background = "navy"),
                box(selectInput(inputId = "clcvmselectInput3", label = "Correlation", choices = c('Pearson' = 'pearson', 'Spearman' = 'spearman')), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clcvmsubmit2', label = "Get CN vs Mutation Plot"))), 
              br(), br(),
              plotlyOutput(outputId = 'clcvmplot1', width = 800, height = 800)
      ),
      
      # clh content
      tabItem(tabName = "clh",
              fluidRow(
                box(selectInput(inputId = "clhselectInput1", label = "Choose dataset", choices = c('Neuroblastoma Microarray data'='celllinedata')), width = 3, background = "navy"),
                box(fileInput(inputId = 'clhfileInput1', label = 'Upload list of genes:', accept = c('csv','tsv','txt')), width = 5, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clhsubmit1', label = "Create heatmaps"))),
              br(), br(),
              plotOutput(outputId = 'clhplot1', width = 800, height = 800)
      ),
      
      # clct content
      tabItem(tabName = "clct",
              fluidRow(
                box(selectInput(inputId = "clctselectInput1", label = "Choose dataset", choices = c('Neuroblastoma Microarray data'='celllinedata')), width = 3, background = "navy"),
                box(selectInput(inputId = "clctselectInput2", label = "Set 1", choices = c("BE2(e)"="BE2",
                                                                                           "BE2C(e)"="BE2C",
                                                                                           "CHLA136"="CHLA136",
                                                                                           "CHLA15"="CHLA15",
                                                                                           "CHLA150"="CHLA150",
                                                                                           "CHLA20"="CHLA20",
                                                                                           "CHP100"="CHP100",
                                                                                           "CHP134(e)"="CHP134",
                                                                                           "CHP212"="CHP212",
                                                                                           "COGN415"="COGN415",
                                                                                           "COGN426"="COGN426",
                                                                                           "EBC1(e)"="EBC1",
                                                                                           "IMR32"="IMR32",
                                                                                           "IMR5(e)"="IMR5",
                                                                                           "KELLY(e)"="KELLY",
                                                                                           "LAN1(e)"="LAN1",
                                                                                           "LAN5(e)"="LAN5",
                                                                                           "LAN6(e)"="LAN6",
                                                                                           "NB1(e)"="NB1",
                                                                                           "NB16(e)"="NB16",
                                                                                           "NB1643(e)"="NB1643",
                                                                                           "NB1691(e)"="NB1691",
                                                                                           "NB1771"="NB1771",
                                                                                           "NB69(e)"="NB69",
                                                                                           "NBLS(e)"="NBLS",
                                                                                           "NBSD(e)"="NBSD",
                                                                                           "NGP(e)"="NGP",
                                                                                           "NLF(e)"="NLF",
                                                                                           "NMB"="NMB",
                                                                                           "RPE1(e)"="RPE1",
                                                                                           "RPE1MYCN40HT(e)"="RPE1MYCN40HT",
                                                                                           "RPE1MYCNWT(e)"="RPE1MYCNWT",
                                                                                           "RPE1WT40HT(e)"="RPE1WT40HT",
                                                                                           "SKNAS(e)"="SKNAS",
                                                                                           "SKNBE1"="SKNBE1",
                                                                                           "SKNDZ(e)"="SKNDZ",
                                                                                           "SKNFI(e)"="SKNFI",
                                                                                           "SKNKAN(e)"="SKNKAN",
                                                                                           "SKNKANR"="SKNKANR",
                                                                                           "SKNSH(e)"="SKNSH",
                                                                                           "SMSKAN"="SMSKAN",
                                                                                           "SMSKCN"="SMSKCN",
                                                                                           "SMSKCNR"="SMSKCNR",
                                                                                           "SMSLHN"="SMSLHN",
                                                                                           "SMSSAN(e)"="SMSSAN",
                                                                                           "SY5Y(e)"="SY5Y"), multiple = TRUE), width = 3, background = "navy"),
                box(selectInput(inputId = "clctselectInput3", label = "Set 2", choices = c("BE2(e)"="BE2",
                                                                                           "BE2C(e)"="BE2C",
                                                                                           "CHLA136"="CHLA136",
                                                                                           "CHLA15"="CHLA15",
                                                                                           "CHLA150"="CHLA150",
                                                                                           "CHLA20"="CHLA20",
                                                                                           "CHP100"="CHP100",
                                                                                           "CHP134(e)"="CHP134",
                                                                                           "CHP212"="CHP212",
                                                                                           "COGN415"="COGN415",
                                                                                           "COGN426"="COGN426",
                                                                                           "EBC1(e)"="EBC1",
                                                                                           "IMR32"="IMR32",
                                                                                           "IMR5(e)"="IMR5",
                                                                                           "KELLY(e)"="KELLY",
                                                                                           "LAN1(e)"="LAN1",
                                                                                           "LAN5(e)"="LAN5",
                                                                                           "LAN6(e)"="LAN6",
                                                                                           "NB1(e)"="NB1",
                                                                                           "NB16(e)"="NB16",
                                                                                           "NB1643(e)"="NB1643",
                                                                                           "NB1691(e)"="NB1691",
                                                                                           "NB1771"="NB1771",
                                                                                           "NB69(e)"="NB69",
                                                                                           "NBLS(e)"="NBLS",
                                                                                           "NBSD(e)"="NBSD",
                                                                                           "NGP(e)"="NGP",
                                                                                           "NLF(e)"="NLF",
                                                                                           "NMB"="NMB",
                                                                                           "RPE1(e)"="RPE1",
                                                                                           "RPE1MYCN40HT(e)"="RPE1MYCN40HT",
                                                                                           "RPE1MYCNWT(e)"="RPE1MYCNWT",
                                                                                           "RPE1WT40HT(e)"="RPE1WT40HT",
                                                                                           "SKNAS(e)"="SKNAS",
                                                                                           "SKNBE1"="SKNBE1",
                                                                                           "SKNDZ(e)"="SKNDZ",
                                                                                           "SKNFI(e)"="SKNFI",
                                                                                           "SKNKAN(e)"="SKNKAN",
                                                                                           "SKNKANR"="SKNKANR",
                                                                                           "SKNSH(e)"="SKNSH",
                                                                                           "SMSKAN"="SMSKAN",
                                                                                           "SMSKCN"="SMSKCN",
                                                                                           "SMSKCNR"="SMSKCNR",
                                                                                           "SMSLHN"="SMSLHN",
                                                                                           "SMSSAN(e)"="SMSSAN",
                                                                                           "SY5Y(e)"="SY5Y"), multiple = TRUE), width = 3, background = "navy"),
                box(textInput(inputId = "clcttextInput1", label = "Enter Pvalue Cutoff", value = "0.0005"), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clctsubmit1', label = "Run Analysis"))), 
              br(), br(), 
              DT::dataTableOutput(outputId = 'clcttable1')
      ),
      ##### Cell Lines Utilities #####
      
      ##### Patient Samples Utilities #####
      tabItem(tabName = "pgeh",
              fluidRow(
                box(selectInput(inputId = "pgehselectInput1", label = "Select dataset", choices = c('NB88','HI51','IH250','OBER649')),
                    actionButton(inputId = "pgehsubmit1", label = "Load dataset"),width = 3, background = "navy"),
                box(checkboxInput(inputId = "pgehcheckboxInput1", label = "Sort Data"),
                    checkboxInput(inputId = "pgehcheckboxInput2", label = "Log Data"),
                    checkboxInput(inputId = "pgehcheckboxInput3", label = "Density"), width = 3, background = "navy"),
                box(selectInput(inputId = "pgehselectInput2", label = "Color by", choices = c("None","STAGE", "MYCN", "RISK")), width = 3, background = "navy"),
                box(selectInput(inputId = "pgehselectInput3", label = "Select gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pgehsubmit2', label = "Get Expression Plot"))), br(), br(),
              plotlyOutput(outputId = "pgehplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pgebp",
              fluidRow(
                box(selectInput(inputId = "pgebpselectInput1", label = "Select dataset", choices = c('NB88','HI51','IH250','OBER649')),
                    actionButton(inputId = "pgebpsubmit1", label = "Load dataset"), width = 3, background = "navy"),
                box(checkboxInput(inputId = "pgebpcheckboxInput1", label = "Log Data"), width = 3, background = "navy"),
                box(selectInput(inputId = "pgebpselectInput2", label = "Color by", choices = c("STAGE", "MYCN", "RISK")), width = 3, background = "navy"),
                box(selectInput(inputId = "pgebpselectInput3", label = "Select gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pgebpsubmit2', label = "Get Patient Boxplot"))), br(), br(),
              plotlyOutput(outputId = "pgebpplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pkm",
              fluidRow(
                box(selectInput(inputId = "pkmselectInput1", label = "Select dataset", choices = c('NB88','IH250')),
                    actionButton(inputId = 'pkmsubmit1', label = "Load dataset"), width = 3, background = "navy"),
                box(selectInput(inputId = "pkmselectInput2", label = "Select endpoint", choices = c("os", "efs")), width = 3, background = "navy"),
                box(selectInput(inputId = "pkmselectInput3", label = "Select gene", choices = c("none"), multiple = TRUE), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pkmsubmit2', label = "Get Kaplan-Meier Plot"))), br(), br(),
              plotlyOutput(outputId = "pkmplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pggc",
              fluidRow(
                box(selectInput(inputId = "pggcselectInput1", label = "Select dataset", choices = c('NB88','HI51','IH250','OBER649')),
                    actionButton(inputId = 'pggcsubmit1', label = "Load dataset"), width = 3, background = "navy"),
                box(checkboxInput(inputId = "pggccheckboxInput1", label = "Log Data"), width = 3, background = "navy"),
                box(selectInput(inputId = "pggcselectInput2", label = "Color by", choices = c("STAGE", "MYCN", "RISK")), width = 3, background = "navy"),
                box(selectInput(inputId = "pggcselectInput3", label = "Select gene 1", choices = c("none")), 
                    selectInput(inputId = "pggcselectInput4", label = "Select gene 2", choices = c("none")), width = 3, background = "navy"),
                box(selectInput(inputId = "pggcselectInput5", label = "Correlation", choices = c('Pearson' = 'pearson', 'Spearman' = 'spearman')), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pggcsubmit2', label = "Get Gene Correlation Plot"))), br(), br(),
              plotlyOutput(outputId = "pggcplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pmcg",
              fluidRow(
                box(selectInput(inputId = "pmcgselectInput1", label = "Select dataset", choices = c('NB88','HI51','IH250')),
                    actionButton(inputId = 'pmcgsubmit1', label = "Load dataset"), width = 3, background = "navy"),
                box(selectInput(inputId = "pmcgselectInput2", label = "Select gene", choices = c("none")), width = 3, background = "navy"),
                box(textInput(inputId = "pmcgtextInput1", label = "Number", value = "10"), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pmcgsubmit2', label = "Get Top Gene Correlations"))), br(), br(),
              DT::dataTableOutput(outputId = 'pmcgtable1')
      ),
      tabItem(tabName = "pgcn",
              fluidRow(
                box(selectInput(inputId = "pgcnselectInput1", label = "Select dataset", choices = c('IH250')),
                    actionButton(inputId = "pgcnsubmit1", label = "Load dataset"), width = 3, background = "navy"),
                box(checkboxInput(inputId = "pgcncheckboxInput1", label = "Sort Data"),
                    checkboxInput(inputId = "pgcncheckboxInput2", label = "Log Data"), width = 3, background = "navy"),
                box(selectInput(inputId = "pgcnselectInput2", label = "Select gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pgcnsubmit2', label = "Get Copy Number Barplot"))), br(), br(),
              plotOutput(outputId = "pgcnplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pgcvm",
              fluidRow(
                box(selectInput(inputId = "pgcvmselectInput1", label = "Select dataset", choices = c('IH251')),
                    actionButton(inputId = "pgcvmsubmit1", label = "Load dataset"), width = 3, background = "navy"),
                box(selectInput(inputId = "pgcvmselectInput2", label = "Select Gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pgcvmsubmit2', label = "Get mRNA/CNA Correlation Plot"))), br(), br(),
              plotlyOutput(outputId = "pgcvmplot1", width = 1000, height = 800)
      ),
      ##### Patient Samples Utilities #####
      
      ##### RNASeq Target Data #####
      tabItem(tabName = "tvnb",
              fluidRow(
                box(selectInput(inputId = "tvnbselectInput1", label = "Select dataset", choices = c("GTEx vs Tumor")),
                    actionButton(inputId = "tvnbsubmit1", label = "Load dataset"), width = 3, background = "navy"),
                box(selectInput(inputId = "tvnbselectInput2", label = "Select Gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'tvnbsubmit2', label = "Comparison with Normal GTEx data"))), br(), br(),
              plotlyOutput(outputId = "tvnbplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "tvnba",
              fluidRow(
                box(selectInput(inputId = "tvnbaselectInput1", label = "Select dataset", choices = c("GTEx vs Tumor")),
                    actionButton(inputId = "tvnbasubmit1", label = "Load dataset"), width = 3, background = "navy"),
                box(selectInput(inputId = "tvnbaselectInput2", label = "Select Gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'tvnbasubmit2', label = "Comparison with Normal GTEx data"))), br(), br(),
              plotlyOutput(outputId = "tvnbaplot1", width = 1000, height = 800)
      ),
      ##### RNASeq Target Data #####
      
      ##### Compendia Analysis #####
      tabItem(tabName = "aoa",
              fluidRow(
                box(selectInput(inputId = "aoaselectInput1", label = "Select dataset", choices = c("Aim3 Overexpression"="compAim3_OE")), width = 4, background = "navy"),
                box(textInput(inputId = "aoatextInput1", label = "Threshold", value = "8"), width = 2, background = "navy"),
                box(textInput(inputId = "aoatextInput2", label = "Frequency", value = "0"), width = 2, background = "navy"),
                box(textInput(inputId = "aoatextInput3", label = "Rank", value = "10000"), width = 2, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'aoasubmit1', label = "Get overexpression analysis"))), br(), br(),
              DT::dataTableOutput(outputId = 'aoatable1')
      ),
      
      tabItem(tabName = "atpc",
              fluidRow(
                box(selectInput(inputId = "atpcselectInput1", label = "Select dataset", choices = c("Aim3 TM Protein Calls"="compAim3_TM")), width = 4, background = "navy"),
                box(selectInput(inputId = "atpcselectInput2", label = "Select Gene", choices = "none"), width = 3, background = "navy"),
                box(selectInput(inputId = "atpcselectInput3", label = "Select Source", choices = c(0,1,2,3,4)), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'atpcsubmit1', label = "Get TM annotation"))), br(), br(),
              DT::dataTableOutput(outputId = 'atpctable1')
      ),
      
      tabItem(tabName = "atnda",
              fluidRow(
                box(selectInput(inputId = "atndaselectInput1", label = "Select dataset", choices = c("Aim3 Differential Analysis"="compAim3_DE")), width = 4, background = "navy"),
                box(textInput(inputId = "atndatextInput1", label = "LogFC", value = "-10"), width = 2, background = "navy"),
                box(textInput(inputId = "atndatextInput2", label = "Pvalue", value = "1"), width = 2, background = "navy"),
                box(textInput(inputId = "atndatextInput3", label = "Rank", value = "10000"), width = 2, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'atndasubmit1', label = "Get Tumor-Normal Diff. Analysis"))), br(), br(),
              DT::dataTableOutput(outputId = 'atndatable1')
      ),
      ##### Compendia Analysis #####
      
      # contact content
      tabItem(tabName = "contact",
              fluidRow(
                infoBox(title = "Contact", value = "Pichai Raman", href = "mailto:ramanp@email.chop.edu", icon = icon("send-o"), color = "yellow")
              ),
              fluidRow(
                infoBox(title = "Contact", value = "Komal Rathi", href = "mailto:rathik@email.chop.edu", icon = icon("send-o"), color = "red")
              )
      )
    ) # tabItems ends
  ) # dashboardBody ends
) # dashboardPage ends