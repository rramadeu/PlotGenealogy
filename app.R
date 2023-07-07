#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#setRepositories(addURLs = c(BioC = "https://bioconductor.org/packages/3.8/bioc"))
#setwd("~/Dropbox (UFL)/PhD/Plot_Genealogy_Shiny")
## The code was built on top of the ggtree package, so, if we use this plot in anywhere we should cite:
## Yu G, Smith D, Zhu H, Guan Y, Lam TT (2017). “ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data.” Methods in Ecology and Evolution, 8, 28-36. doi: 10.1111/2041-210X.12628.

purrr::walk(c("shiny", "RColorBrewer", "tidyverse", 
              "ggtree"), ~{
                if (!.x %in% installed.packages()) install.packages(.x)
              })

suppressWarnings({
  suppressPackageStartupMessages({
    library(BiocManager)
    options(repos = BiocManager::repositories())
    library(shiny)
    ## Loading libraries
    library(RColorBrewer)
    library(tidyverse)
    library(ggtree)
    library(openxlsx)
    library(readxl)
  })
})


## Loading the plot_genealogy function
source("plot_genealogy.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Plot Genealogy"),
   fluidRow(
     column(10,
            hr(),
            HTML("<p>App created by <a href='https://rramadeu.github.io'>Rodrigo Amadeu</a> for internal usage of <a href='https://www.blueberrybreeding.com'>UF Blueberry Breeding Lab</a></p>"),
            HTML("<p></p>"),
     )),
   fluidRow(
     column(1,
            fileInput('file1', 'Choose xlsx file',
                      accept = c(".xlsx")
            ),
            
            textInput('Genotype', 'Genotype',
                      value = ""),
            
            sliderInput("generations",
                        "Generations to plot:",
                        min = 1,
                        max = 20,
                        value = 4),
           checkboxInput("color", "Color", value = FALSE, width = NULL),
           checkboxInput("flip", "Flip", value = FALSE, width = NULL),
           checkboxInput("slanted", "Slanted", value = FALSE, width = NULL),
           downloadLink('downloadData', 'xlsx example'),
     
     #tags$div(class="header", checked=NA,
    #          tags$p("App created by"),
    #          tags$a(href="rramadeu.github.io", "Rodrigo Amadeu")),
     #tags$div(class="header", checked=NA,
      #        tags$a(href="www.blueberrybreeding.com", "UF Blueberry Breding Lab"))
    ),
     
  # Sidebar with a slider input for number of bins 
   column(11,
                   plotOutput("distPlot"))),
  
  fluidRow(
    column(1,
           hr(),
           HTML("<p>It uses  <a href='https://cran.r-project.org/'>R</a> , <a href='https://guangchuangyu.github.io/software/ggtree/'>ggtree</a> and <a href='https://www.tidyverse.org/'>tidyverse</a>  packages</p> No data is saved here, you're safe."),
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$downloadData <- downloadHandler(
     filename = function() ("example-pedigree.xlsx"),
     content = function(con){
       data = data.frame(Cultivar=c("Ancestral1",
                                    "Ancestral2",
                                    "CultivarA",
                                    "CultivarB",
                                    "CultivarC",
                                    "CultivarD"),
                         Parent1=c("0",
                                   "0",
                                   "Ancestral1",
                                   "Ancestral1",
                                   "CultivarB",
                                   "CultivarC"),
                         Parent2=c("0",
                                   "0",
                                   "Ancestral2",
                                   "0",
                                   "CultivarA",
                                   "Ancestral2"))
       write.xlsx(data, con)
     }
   )
  
  pedigreedf <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df <- as.data.frame(read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)[,1:3])
    return(df)
  })

   output$distPlot <- renderPlot({
     pedigree <- pedigreedf()
     # generate bins based on input$bins from ui.R
     plot_genealogy(pedigree,input$Genotype,
                    gens=input$generations,
                    color=input$color,
                    flip=input$flip,
                    slanted=input$slanted)
     },
     height="auto", width="auto")
   
}

# Run the application 
shinyApp(ui = ui, server = server)

