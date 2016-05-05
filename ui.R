library(shinydashboard)
library(shinyIncubator)
library(shiny)

dashboardPage(
  
  # dashboardHeader begins
  dashboardHeader(title = 'Maris\' Lab Web Portal', titleWidth = 500
                  ), # dashboardHeader ends
  
  # dashboardSidebar begins
  dashboardSidebar(width = 500,
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
    
    # tabItems begins
    tabItems(
      
      # dashboard content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Maris Lab", status = "danger", width = 12, solidHeader = TRUE, "Tools, analysis, and visualizations to support research on Neuroblastoma and other pediatric cancers.", br(), br(), actionButton(inputId='ab1', label="Learn More", icon = icon("th")))
              ),
              fluidRow(
                box(title = "Cell Lines", status = "warning", width = 4, collapsible = T, collapsed = T, solidHeader = TRUE, "Tools and visualizations to support finding a cell line or set of cell lines that expressed a particular gene or pathway, looking at relevent correlations between genes, and examining cell line mutation. Currently internal Neuroblastoma cell line data is used but in the future data from CLE and Sanger will be imported.", br(), br(), actionButton(inputId='ab2', label="View Details >>")),
                box(title = "Patient Data", status = "warning", width = 4, collapsible = T, collapsed = T, solidHeader = TRUE, "Visualizations and tools to analyze patient data in multiple ways. One can look at Gene Expression across cohorts, kaplan-meier curves based on a gene or set of genes, most correlated genes, etc... Currently two public data sets are included, in the future, our internal data set and other relevent data can be displayed.", br(), br(), actionButton(inputId='ab3', label="View Details >>")),
                box(title = "Analysis Tools", status = "warning", width = 4, collapsible = T, collapsed = T, solidHeader = TRUE, "Analytical generic bioinformatics tools such as Gene Set Enrichment Analysis, IC50 Analysis, Drug Synergy Analysis, etc... Starred tools are being prepped for production and will be incorporated shortly.", br(), br(), actionButton(inputId='ab4', label="View Details >>"))
              )
      ),
      
      # nbrcl content
      tabItem(tabName = "nbrcl",
              DT::dataTableOutput('table')
              ),
      
      # clggc content
      tabItem(tabName = "clggc",
              fluidRow(
                box(textInput(inputId = "gene1", label = "Enter Gene 1", value = "MYC"), width = 3, background = "navy"),
                box(textInput(inputId = "gene2", label = "Enter Gene 2", value = "MYCN"), width = 3, background = "navy")
              ),
              fluidRow(column(3, actionButton(inputId = 'submit1', label = "Get Correlation"))), br(), br(),
              plotOutput("plot1", width = 800, height = 800)
              ),
      
      # cldb content
      tabItem(tabName = "cldb",
              fluidRow(
                box(title = "Maris Lab", status = "danger", width = 12, solidHeader = TRUE, "Tools, analysis, and visualizations to support research on Neuroblastoma and other pediatric cancers.", br(), br(), actionButton(inputId='ab1', label="Learn More >>"))
              )
      ),
      
      # contact content
      tabItem(tabName = "contact",
              fluidRow(
                box(title = "Contact Info", status = "danger", width = 4, solidHeader = T)
              ))
      
    ) # tabItems ends
  ) # dashboardBody ends
) # dashboardPage ends