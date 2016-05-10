library(shiny)
library(reshape2)
library(plotly)
library(ggplot2)
library(Rgraphviz)
library(DT)
library(shinydashboard)
library(shinyIncubator)

dashboardPage(
  
  # dashboardHeader begins
  dashboardHeader(title = 'Maris\' Lab Web Portal', titleWidth = 500), # dashboardHeader ends
  
  # dashboardSidebar begins
  dashboardSidebar(width = 500,
                   
    # enable vertical scrolling
    div(style="overflow-y: scroll"),
    
    # sidebarMenu begins
    sidebarMenu(
      
      menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
      menuItem("Cell Lines Database", icon = icon("database"), tabName = "cldb",
               menuSubItem("Neuroblastoma Cell Line", icon = icon("file"), tabName = "nbcl"),
               menuSubItem("Neuroblastoma RNAseq Cell Line", icon = icon("file"), tabName = "nbrcl")
               ),
      menuItem("Cell Lines Utilities", tabName = "celllines", icon = icon("gears"),
               menuSubItem("Cell Line Gene Expression", icon = icon("bar-chart"), tabName = "clge"),
               menuSubItem("Cell Line Gene/Gene Correlation", icon = icon("line-chart"), tabName = "clggc"),
               menuSubItem("Cell Line Mutation Table", icon = icon("table"), tabName = "clm"),
               menuSubItem("Cell Line Gene Copy Number", icon = icon("bar-chart"), tabName = "clgcn"),
               menuSubItem("Cell Line CNA vs mRNA", icon = icon("line-chart"), tabName = "clcvm"),
               menuSubItem("Cell Line Heatmap", icon = icon("th"), tabName = "clh"),
               menuSubItem("Cell Line Comparison Tool", icon = icon("table"), tabName = "clct")
               ),
      menuItem("Patient Samples Utilities", tabName = "patientsamples", icon = icon("gears"),
               menuSubItem("Patient Gene Expression Histogram", icon = icon("bar-chart"), tabName = "pgeh"),
               menuSubItem("Patient Gene Expression Box Plot", icon = icon("bar-chart"), tabName = "pgebp"),
               menuSubItem("Patient Kaplan-meier", icon = icon("line-chart"), tabName = "pkm"),
               menuSubItem("Patient Gene/Gene Correlation", icon = icon("line-chart"), tabName = "pggc"),
               menuSubItem("Patient Most Correlated Genes", icon = icon("table"), tabName = "pmcg"),
               menuSubItem("Patient Gene Copy Number", icon = icon("bar-chart"), tabName = "pgcn"),
               menuSubItem("Patient Gene CNA vs mRNA", icon = icon("line-chart"), tabName = "pgcvm")
               ),
      menuItem("RNASeq Target Data", tabName = "targetdata", icon = icon("gears"),
               menuSubItem("Tumor vs Normal Boxplot", icon = icon("bar-chart"), tabName = "tvnb"),
               menuSubItem("Tumor vs Normal Boxplot Abstracted", icon = icon("bar-chart"), tabName = "tvnba")
               ),
      menuItem("Compendia Analysis", tabName = "compendiaanalysis", icon = icon("gears"),
               menuSubItem("Aim 3 Overexpression Analysis", icon = icon("table"), tabName = "aoa"),
               menuSubItem("Aim 3 Transmembrane Protein Calls", icon = icon("table"), tabName = "atpc"),
               menuSubItem("Aim 3 Tumor Normal Differential Analysis", icon = icon("table"), tabName = "atnda")
               ),
      menuItem("Analysis Tools", tabName = "analysistools", icon = icon("gears"),
               menuSubItem("Venn Diagrams (Ext)", icon = icon("pie-chart"), tabName = "vd"),
               menuSubItem("Gene Set Enrichment", icon = icon("star"), tabName = "gse"),
               menuSubItem("IC50 Analysis", icon = icon("star"), tabName = "ia")
               ),
      menuItem("More Info", tabName = "moreinfo", icon = icon("th"),
               menuSubItem("Readme", icon = icon("navicon"), tabName = "readme"),
               menuSubItem("About", icon = icon("info-circle"), tabName = "about"),
               menuSubItem("Contact", icon = icon("envelope"), tabName = "contact")
               )
      
    ) # sidebarMenu ends
  ), # dashboardSidebar ends
  
  # dashboardBody begins
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
                box(title = "Cell Lines", status = "warning", width = 4, collapsible = T, collapsed = T, solidHeader = TRUE, "Tools and visualizations to support finding a cell line or set of cell lines that expressed a particular gene or pathway, looking at relevent correlations between genes, and examining cell line mutation. Currently internal Neuroblastoma cell line data is used but in the future data from CLE and Sanger will be imported.", br(), br(), actionButton(inputId='ab2', label="View Details", icon = icon("th"))),
                box(title = "Patient Data", status = "warning", width = 4, collapsible = T, collapsed = T, solidHeader = TRUE, "Visualizations and tools to analyze patient data in multiple ways. One can look at Gene Expression across cohorts, kaplan-meier curves based on a gene or set of genes, most correlated genes, etc... Currently two public data sets are included, in the future, our internal data set and other relevent data can be displayed.", br(), br(), actionButton(inputId='ab3', label="View Details", icon = icon("th"))),
                box(title = "Analysis Tools", status = "warning", width = 4, collapsible = T, collapsed = T, solidHeader = TRUE, "Analytical generic bioinformatics tools such as Gene Set Enrichment Analysis, IC50 Analysis, Drug Synergy Analysis, etc... Starred tools are being prepped for production and will be incorporated shortly.", br(), br(), actionButton(inputId='ab4', label="View Details", icon = icon("th")))
              )
      ),
      
      # cldb content
      
      
      # nbrcl content
      tabItem(tabName = "nbrcl",
              DT::dataTableOutput(outputId = "nbrcltable1")
              ),
      
      ##### Cell Lines Utilities #####
      # clge content
      tabItem(tabName = "clge",
              fluidRow(
                box(selectInput(inputId = "clgeselectInput1", label = "Select Gene", choices = "none"), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clgesubmit1', label = "Get Expression Plot"))), br(), br(),
              plotlyOutput(outputId = "clgeplot1", width = 1000, height = 800)
              ),
      
      # clggc content
      tabItem(tabName = "clggc",
              fluidRow(
                box(selectInput(inputId = "clggcselectInput1", label = "Select Gene 1", choices = "none"), width = 3, background = "navy"),
                box(selectInput(inputId = "clggcselectInput2", label = "Select Gene 2", choices = "none"), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clggcsubmit1', label = "Get Correlation Plot"))), br(), br(),
              plotlyOutput(outputId = "clggcplot1", width = 800, height = 800)
              ),
      
      # clm content
      tabItem(tabName = "clm",
              fluidRow(
                box(selectInput(inputId = "clmselectInput1", label = "Select Gene", choices = "none"), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clmsubmit1', label = "Get Mutation Table"))), 
              br(), br(),
              DT::dataTableOutput(outputId = 'clmtable1')
              ),
      
      # clgcn content
      tabItem(tabName = "clgcn",
              fluidRow(
                box(selectInput(inputId = "clgcnselectInput1", label = "Select Gene", choices = "none"), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clgcnsubmit1', label = "Get Copy Number Barplot"))), 
              br(), br(),
              plotOutput(outputId = 'clgcnplot1', width = 800, height = 800)
              ),
      
      # clcvm content
      tabItem(tabName = "clcvm",
              fluidRow(
                box(selectInput(inputId = "clcvmselectInput1", label = "Select Gene", choices = "none"), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clcvmsubmit1', label = "Get CN vs Mutation Plot"))), 
              br(), br(),
              plotOutput(outputId = 'clcvmplot1', width = 800, height = 800)
              ),
      
      # clh content
      tabItem(tabName = "clh",
              fluidRow(
                box(fileInput(inputId = 'clhfileInput', label = 'Upload list of genes:', accept = c('csv','tsv','txt')), width = 5, background = "navy"),
                box(selectInput(inputId='clhselectInput1', label = 'Genes Uploaded',choices = c('none'), selected = 'none'), width = 3, background = "navy"),
                box(selectInput(inputId='clhselectInput2', label = 'Count',choices = c('none'), selected = 'none'), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'clhsubmit1', label = "Create heatmaps"))),
              br(), br(),
              plotOutput(outputId = 'clhplot1', width = 800, height = 800)
              ),
      
      # clct content
      tabItem(tabName = "clct",
              fluidRow(
                box(selectInput(inputId = "clctselectInput1", label = "Set 1", choices = "none", multiple = TRUE), width = 3, background = "navy"),
                box(selectInput(inputId = "clctselectInput2", label = "Set 2", choices = "none", multiple = TRUE), width = 3, background = "navy"),
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
                box(selectInput(inputId = "pgehselectInput1", label = "Choose dataset", choices = c('NB88','HI51','IH250','OBER649')),width = 3, background = "navy"),
                box(checkboxGroupInput(inputId = "pgehcheckboxInput1", label = "Select Parameters", choices = c("Sort Data", "Log Data", "Density")), width = 3, background = "navy"),
                box(selectInput(inputId = "pgehselectInput2", label = "Color by", choices = c("STAGE", "MYCN", "RISK")), width = 3, background = "navy"),
                box(selectInput(inputId = "pgehselectInput3", label = "Select gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pgehsubmit1', label = "Get Expression Plot"))), br(), br(),
              plotOutput(outputId = "pgehplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pgebp",
              fluidRow(
                box(selectInput(inputId = "pgebpselectInput1", label = "Choose dataset", choices = c('NB88','HI51','IH250','OBER649')), width = 3, background = "navy"),
                box(checkboxInput(inputId = "pgebpcheckboxInput1", label = "Log Data"), width = 3, background = "navy"),
                box(selectInput(inputId = "pgebpselectInput2", label = "Color by", choices = c("STAGE", "MYCN", "RISK")), width = 3, background = "navy"),
                box(selectInput(inputId = "pgebpselectInput3", label = "Select gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pgebpsubmit1', label = "Get Patient Boxplot"))), br(), br(),
              plotOutput(outputId = "pgebpplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pkm",
              fluidRow(
                box(selectInput(inputId = "pkmselectInput1", label = "Choose dataset", choices = c('NB88','HI51','IH250','OBER649')), width = 3, background = "navy"),
                box(selectInput(inputId = "pkmselectInput2", label = "Choose endpoint", choices = c("os", "efs")), width = 3, background = "navy"),
                box(selectInput(inputId = "pkmselectInput3", label = "Select gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pkmsubmit1', label = "Get Kaplan-Meier Plot"))), br(), br(),
              plotOutput(outputId = "pkmplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pggc",
              fluidRow(
                box(selectInput(inputId = "pggcselectInput1", label = "Choose dataset", choices = c('NB88','HI51','IH250','OBER649')), width = 3, background = "navy"),
                box(checkboxInput(inputId = "pggccheckboxInput1", label = "Log Data"), width = 3, background = "navy"),
                box(selectInput(inputId = "pggcselectInput2", label = "Color by", choices = c("STAGE", "MYCN", "RISK")), width = 3, background = "navy"),
                box(selectInput(inputId = "pggcselectInput3", label = "Select gene 1", choices = c("none")), 
                    selectInput(inputId = "pggcselectInput4", label = "Select gene 2", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pggcsubmit1', label = "Get Gene Correlation Plot"))), br(), br(),
              plotOutput(outputId = "pggcpplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pmcg",
              fluidRow(
                box(selectInput(inputId = "pmcgselectInput1", label = "Choose dataset", choices = c('NB88','HI51','IH250','OBER649')), width = 3, background = "navy"),
                box(selectInput(inputId = "pmcgselectInput2", label = "Select gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pmcgsubmit1', label = "Get Top Gene Correlations"))), br(), br(),
              DT::dataTableOutput(outputId = 'pmcgtable1')
      ),
      tabItem(tabName = "pgcn",
              fluidRow(
                box(selectInput(inputId = "pgcnselectInput1", label = "Choose dataset", choices = c('NB88','HI51','IH250','OBER649')), width = 3, background = "navy"),
                box(checkboxGroupInput(inputId = "pgcncheckboxInput1", label = "Select Parameters", choices = c("Sort Data", "Log Data", "Density")), width = 3, background = "navy"),
                box(selectInput(inputId = "pgcnselectInput2", label = "Select gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pgcnsubmit1', label = "Get Copy Number Barplot"))), br(), br(),
              plotOutput(outputId = "pgcnplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "pgcvm",
              fluidRow(
                box(selectInput(inputId = "pgcvmselectInput1", label = "Select Gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'pgcvmsubmit1', label = "Get mRNA/CNA Correlation Plot"))), br(), br(),
              plotOutput(outputId = "pgcvmplot1", width = 1000, height = 800)
      ),
      ##### Patient Samples Utilities #####
      
      ##### RNASeq Target Data #####
      tabItem(tabName = "tvnb",
              fluidRow(
                box(selectInput(inputId = "tvnbselectInput1", label = "Select Gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'tvnbsubmit1', label = "Comparison with Normal GTEx data"))), br(), br(),
              plotOutput(outputId = "tvnbplot1", width = 1000, height = 800)
      ),
      tabItem(tabName = "tvnba",
              fluidRow(
                box(selectInput(inputId = "tvnbaselectInput1", label = "Select Gene", choices = c("none")), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'tvnbasubmit1', label = "Comparison with Normal GTEx data"))), br(), br(),
              plotOutput(outputId = "tvnbaplot1", width = 1000, height = 800)
      ),
      ##### RNASeq Target Data #####
      
      ##### Compendia Analysis #####
      tabItem(tabName = "aoa",
              fluidRow(
                box(selectInput(inputId = "aoaselectInput1", label = "Select Gene", choices = c("All")), 
                    selectInput(inputId = "aoaselectInput2", label = "Select Tissue", choices = "none"), width = 3, background = "navy"),
                box(checkboxInput(inputId = "aoacheckboxInput1", label = "Normals only?"), width = 3, background = "navy"),
                box(selectInput(inputId = "aoaselectInput1", label = "Threshold", choices = c("All", 8)), 
                    textInput(inputId = "aoatextInput1", label = "Frequency", value = 0), 
                    textInput(inputId = "aoatextInput2", label = "Rank", value = 10000), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'aoasubmit1', label = "Get overexpression analysis"))), br(), br(),
              DT::dataTableOutput(outputId = 'aoatable1')
      ),
  
      tabItem(tabName = "atpc",
              fluidRow(
                box(selectInput(inputId = "atpcselectInput1", label = "Select Gene", choices = "none"), width = 3, background = "navy"),
                box(selectInput(inputId = "atpcselectInput2", label = "Select Source", choices = c(0,1,2,3,4)), width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'atpcsubmit1', label = "Get TM annotation"))), br(), br(),
              DT::dataTableOutput(outputId = 'atpctable1')
      ),
      
      tabItem(tabName = "atnda",
              fluidRow(
                box(selectInput(inputId = "atndaselectInput1", label = "Select Gene", choices = "none"),
                    selectInput(inputId = "atndaselectInput2", label = "Select Disease Tissue", choices = "none"),
                    selectInput(inputId = "atndaselectInput3", label = "Select Normal Tissue", choices = "none"),
                    width = 3, background = "navy"),
                box(textInput(inputId = "atndatextInput1", label = "LogFC", value = -10),
                    textInput(inputId = "atndatextInput2", label = "Pvalue", value = 1),
                    textInput(inputId = "atndatextInput3", label = "Rank", value = 10000),
                    width = 3, background = "navy")
              ),
              fluidRow(column(5, actionButton(inputId = 'atndasubmit1', label = "Get Tumor-Normal Diff. Analysis"))), br(), br(),
              DT::dataTableOutput(outputId = 'atndatable1')
      ),
      
      ##### Compendia Analysis #####
      
      # contact content
      tabItem(tabName = "contact",
              fluidRow(
                box(title = "Contact Info", status = "danger", width = 4, solidHeader = TRUE)
              )
              )
      
    ) # tabItems ends
  ) # dashboardBody ends
) # dashboardPage ends