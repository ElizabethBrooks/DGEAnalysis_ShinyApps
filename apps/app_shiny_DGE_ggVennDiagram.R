# created by: Elizabeth Brooks
# date: 12 October 2023

#### Setup ####

# load packages
library(shiny)
library(shinythemes)
library(ggVennDiagram)
library(ggplot2)
library(rcartocolor)

# color blind safe plotting palettes
plotColors <- carto_pal(12, "Safe")

#### UI ####

# Define UI 
ui <- fluidPage(
  # view available themes
  #shinythemes::themeSelector(),
  
  # use a theme
  theme = shinytheme("yeti"),
  #theme = shinytheme("superhero"),
  
  # add application title
  titlePanel("Venn Diagrams in ggVennDiagram"),
  
  # setup sidebar layout
  sidebarLayout(
    
    # setup sidebar panel
    sidebarPanel(
      
      # file uploads
      tags$p(
        "Enter set one name:"
      ),
      textInput(
        "setOne", 
        label = NULL,
        value = "Set One"
      ),
      tags$p(
        "Upload set one table (*.csv):"
      ),
      fileInput(
        "oneTable", 
        label = NULL,
        multiple = FALSE,
        accept = ".csv"
      ),
      tags$hr(),
      tags$p(
        "Enter set two name:"
      ),
      textInput(
        "setTwo", 
        label = NULL,
        value = "Set Two"
      ),
      tags$p(
        "Upload set two table (*.csv):"
      ),
      fileInput(
        "twoTable", 
        label = NULL,
        multiple = FALSE,
        accept = ".csv"
      ),
      conditionalPanel(
        condition = "output.twoDataUploaded",
        tags$hr(),
        tags$p(
          "Enter set three name:"
        ),
        textInput(
          "setThree", 
          label = NULL,
          value = "Set Three"
        ),
        tags$p(
          "Upload set three table (*.csv):"
        ),
        fileInput(
          "threeTable", 
          label = NULL,
          multiple = FALSE,
          accept = ".csv"
        ),
        conditionalPanel(
          condition = "output.threeDataUploaded",
          tags$hr(),
          tags$p(
            "Enter set four name:"
          ),
          textInput(
            "setFour", 
            label = NULL,
            value = "Set Four"
          ),
          tags$p(
            "Upload set four table (*.csv):"
          ),
          fileInput(
            "fourTable", 
            label = NULL,
            multiple = FALSE,
            accept = ".csv"
          )
        )
      )
    ),
    
    # Output: Show plots
    mainPanel(
      
      # getting started text
      conditionalPanel(
        condition = "!output.twoDataUploaded",
        tags$h1("Getting Started", align = "center"),
        tags$br(),
        tags$p(
          HTML("<b>Hello!</b>"),
          HTML("Start by entering the names of the sets for comparison and uploading two <i>.csv</i> files in the left-hand sidebar.")
        ),
        tags$p(
          HTML("The first column of the <i>.csv</i> files are expected to contain the values for comparison (e.g., gene IDs).")
        ),
        tags$br(),
        tags$p(
          align="center",
          HTML("<b>Helpful Tips</b>")
        ),
        tags$p(
          HTML("<b>Tip 1</b> It is possible to upload files that contain only a single column of values, since every column after the first is ignored.")
        ),
        tags$p(
          HTML("<b>Tip 2:</b> Two files must be uploaded for a minimum comparison between two sets of values.")
        ),
        tags$p(
          HTML("<b>Tip 3:</b> After uploading the first two files, it will be possible to compare up to four sets of values.")
        ),
      ),
      
      # results text and plots
      conditionalPanel(
        condition = "output.twoDataUploaded",
        # set of tab panels
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tips",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Helpful Tips</b>")
            ),
            tags$p(
              HTML("<b>Tip 1:</b> The results may take several moments to appear depending on the size of the input data tables.")
            ),
            tags$p(
              HTML("<b>Tip 2:</b> Navigate to the <i>Venn Diagrams</i> by clicking the tab at the top of the page.")
            ),
            tags$p(
              HTML("<b>Tip 3:</b> It is possible to change the sets of values for comparison by changing the uploaded files in the left-hand side bar.")
            ),
            tags$p(
              HTML("<b>Tip 4:</b> It is possible to compare up to four sets of values by uploading additional <i>.csv</i> files in the left-hand side bar.")
            ),
            tags$p(
              HTML("<b>Tip 5:</b> Enter or change the names of each set in the left-hand side bar.")
            )
          ),
          
          # Venn Diagrams tab
          tabPanel(
            "Venn Diagrams",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Two-Way Venn Diagram</b>")
            ),
            plotOutput(outputId = "twoWayVenn"),
            downloadButton(outputId = "twoWayVennDownload", label = "Download Plot"),
            # three-way venn
            conditionalPanel(
              condition = "output.threeDataUploaded",
              tags$br(),
              tags$p(
                align="center",
                HTML("<b>Three-Way Venn Diagram</b>")
              ),
              plotOutput(outputId = "threeWayVenn"),
              downloadButton(outputId = "threeWayVennDownload", label = "Download Plot")
            ),
            # four-way venn
            conditionalPanel(
              condition = "output.fourDataUploaded",
              tags$br(),
              tags$p(
                align="center",
                HTML("<b>Four-Way Venn Diagram</b>")
              ),
              tags$br(),
              plotOutput(outputId = "fourWayVenn"),
              downloadButton(outputId = "fourWayVennDownload", label = "Download Plot")
            )
          ),
          
          # information tab
          tabPanel(
            "Information",
            tags$br(),
            tags$p(
              align="center",
              HTML("<b>Helpful Information</b>")
            ),
            tags$p(
              "This application for creating venn diagrams to compare sets of values was created by",
              tags$a("Elizabeth Brooks",href = "https://www.linkedin.com/in/elizabethmbrooks/"),
              "."
            ),
            tags$p(
              "The latest version of this application may be downloaded from",
              tags$a("GitHub",href = "https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps"),
              "."
            ),
            tags$p(
              "Example sets of gene IDs are also provided on",
              tags$a("GitHub", href = "https://github.com/ElizabethBrooks/DGEAnalysis_ShinyApps/tree/main/data/ggVennDiagram"),
              "."
            )
          )
        )
      )
    )
  )
)

#### Server ####

# Define server 
server <- function(input, output, session) {
  
  ##
  # Data Setup
  ##
  
  # retrieve input data one
  inputOneTable <- reactive({
    # require input data
    req(input$oneTable)
    # check the input table is not null
    if(is.null(input$oneTable)){
      return(NULL)
    }
    # read the file
    dataTableInput <- read.csv(file = input$oneTable$datapath, row.names=1)
    # return data
    namesSet <- rownames(dataTableInput)
  })
  
  # retrieve input data two
  inputTwoTable <- reactive({
    # require input data
    req(input$twoTable)
    # check the input table is not null
    if(is.null(input$twoTable)){
      return(NULL)
    }
    # read the file
    dataTableInput <- read.csv(file = input$twoTable$datapath, row.names=1)
    # return data
    namesSet <- rownames(dataTableInput)
  })
  
  # retrieve input data three
  inputThreeTable <- reactive({
    # require input data
    req(input$threeTable)
    # check the input table is not null
    if(is.null(input$threeTable)){
      return(NULL)
    }
    # read the file
    dataTableInput <- read.csv(file = input$threeTable$datapath, row.names=1)
    # return data
    namesSet <- rownames(dataTableInput)
  })
  
  # retrieve input data four
  inputFourTable <- reactive({
    # require input data
    req(input$fourTable)
    # check the input table is not null
    if(is.null(input$fourTable)){
      return(NULL)
    }
    # read the file
    dataTableInput <- read.csv(file = input$fourTable$datapath, row.names=1)
    # return data
    namesSet <- rownames(dataTableInput)
  })
  
  # check if two files have been uploaded
  output$twoDataUploaded <- function(){
    # check the input tables are not null
    if(is.null(inputOneTable())){
      return(FALSE)
    }else if(is.null(inputTwoTable())){
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'twoDataUploaded', suspendWhenHidden=FALSE)
  
  # check if three files have been uploaded
  output$threeDataUploaded <- function(){
    # check the input tables are not null
    if(is.null(inputOneTable())){
      return(FALSE)
    }else if(is.null(inputTwoTable())){
      return(FALSE)
    }else if(is.null(inputThreeTable())){
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'threeDataUploaded', suspendWhenHidden=FALSE)
  
  # check if four files have been uploaded
  output$fourDataUploaded <- function(){
    # check the input tables are not null
    if(is.null(inputOneTable())){
      return(FALSE)
    }else if(is.null(inputTwoTable())){
      return(FALSE)
    }else if(is.null(inputThreeTable())){
      return(FALSE)
    }else if(is.null(inputFourTable())){
      return(FALSE)
    }
    return(TRUE)
  }
  outputOptions(output, 'fourDataUploaded', suspendWhenHidden=FALSE)
  
  
  ## 
  # Venn Diagrams
  ##
  
  # function to create two-way venn diagrams
  createTwoWay <- reactive({
    # retrieve set of row names
    namesSetOne <- inputOneTable()
    namesSetTwo <- inputTwoTable()
    # create combined list of names
    namesList <- list(setOne = namesSetOne, 
                      setTwo = namesSetTwo)
    # setup name
    #plotName <- paste(input$setOne, "vs", input$setTwo, sep=" ")
    # create venn diagram
    ggVennDiagram(namesList, label_alpha=0.25, category.names = c(input$setOne,input$setTwo)) +
      scale_x_continuous(expand = expansion(mult = .2)) +
      scale_fill_gradientn(colors = c(plotColors[1], plotColors[2]))# + 
      #labs(
        #title = plotName,
        #caption = Sys.Date()
      #)
  })
  
  # function to render two-way venn
  output$twoWayVenn <- renderPlot({
    # create plot
    createTwoWay()
  })
  
  # download handler for two-way venn
  output$twoWayVennDownload <- downloadHandler(
    filename = function() {
      exportFile <- paste(input$setOne, input$setTwo, sep="_")
      exportFile <- paste(exportFile, "twoWayVenn.png", sep="_")
    },
    content = function(file) {
      # save the plot
      outVenn <- createTwoWay()
      ggsave(file, plot = outVenn, device = "png")
    }
  )
  
  # function to create three-way venn diagrams
  createThreeWay <- reactive({
    # retrieve set of row names
    namesSetOne <- inputOneTable()
    namesSetTwo <- inputTwoTable()
    namesSetThree <- inputThreeTable()
    # create combined list of names
    namesList <- list(setOne = namesSetOne, 
                      setTwo = namesSetTwo,
                      setThree = namesSetThree)
    # setup name
    #plotName <- paste(input$setOne, "vs", input$setTwo, "vs", input$setThree, sep=" ")
    # create venn diagram
    ggVennDiagram(namesList, label_alpha=0.25, category.names = c(input$setOne,input$setTwo,input$setThree)) +
      scale_x_continuous(expand = expansion(mult = .2)) +
      scale_fill_gradientn(colors = c(plotColors[1], plotColors[2]))# + 
      #labs(
        #title = plotName,
        #caption = Sys.Date()
      #)
  })
  
  # function to render three-way venn
  output$threeWayVenn <- renderPlot({
    # create plot
    createThreeWay()
  })
  
  # download handler for three-way venn
  output$threeWayVennDownload <- downloadHandler(
    filename = function() {
      exportFile <- paste(input$setOne, input$setTwo, sep="_")
      exportFile <- paste(exportFile, input$setThree, sep="_")
      exportFile <- paste(exportFile, "threeWayVenn.png", sep="_")
    },
    content = function(file) {
      # save the plot
      outVenn <- createThreeWay()
      ggsave(file, plot = outVenn, device = "png")
    }
  )
  
  # function to create four-way venn diagrams
  createFourWay <- reactive({
    # retrieve set of row names
    namesSetOne <- inputOneTable()
    namesSetTwo <- inputTwoTable()
    namesSetThree <- inputThreeTable()
    namesSetFour <- inputFourTable()
    # create combined list of names
    namesList <- list(setOne = namesSetOne, 
                      setTwo = namesSetTwo,
                      setThree = namesSetThree,
                      setFour = namesSetFour
                      )
    # setup name
    #plotName <- paste(input$setOne, "vs", input$setTwo, "vs", input$setThree, "vs", input$setFour, sep=" ")
    # create venn diagram
    ggVennDiagram(namesList, label_alpha=0.25, category.names = c(input$setOne,input$setTwo,input$setThree,input$setFour)) +
      scale_x_continuous(expand = expansion(mult = .2)) +
      scale_fill_gradientn(colors = c(plotColors[1], plotColors[2]))# + 
      #labs(
        #title = plotName,
        #caption = Sys.Date()
      #)
  })
  
  # function to render four-way venn
  output$fourWayVenn <- renderPlot({
    # create plot
    createFourWay()
  })
  
  # download handler for four-way venn
  output$fourWayVennDownload <- downloadHandler(
    filename = function() {
      exportFile <- paste(input$setOne, input$setTwo, sep="_")
      exportFile <- paste(exportFile, input$setThree, sep="_")
      exportFile <- paste(exportFile, input$setFour, sep="_")
      exportFile <- paste(exportFile, "fourWayVenn.png", sep="_")
    },
    content = function(file) {
      # save the plot
      outVenn <- createFourWay()
      ggsave(file, plot = outVenn, device = "png")
    }
  )
  
}

#### App Object ####

# create the Shiny app object 
shinyApp(ui = ui, server = server)

