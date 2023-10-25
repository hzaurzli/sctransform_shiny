library(shiny)

ui <- tagList(
  fluidPage(
    titlePanel("Sctransform"),
    sidebarLayout(
      sidebarPanel(
        # uiOutput 做上传文件的 ui, 对应后面的 output$file1
        uiOutput('file1'),
        
        actionButton('reset', 'RESET'),
        hr(),
        downloadButton("downloadData", "Download"),
        hr(),
        h5('Developer:'),
        h6('Small runze (shiny app)'),
        br(),
        h5('Github: '),
        h6('https://github.com/hzaurzli (Small runze)'),
        br(),
        h5('Cition:'),
        h6('https://github.com/satijalab/sctransform')
      ),
      mainPanel(
        h4("Sctransform normalization"),
        br(),
        br(),
        shinycssloaders::withSpinner(
          plotOutput("detectfig",width = "100%",height = '600px')
        )
      )
    )
  )
)



server <- function(input, output, session) {
  options(shiny.maxRequestSize=1024*1024*1024^2)
  
  values <- reactiveValues(
    file = NULL
  )
  
  
  expression <- reactive({
    sessionEnvir <- sys.frame()
    if (!is.null(input$file1)) eval(parse(text = load(input$file1$datapath, sessionEnvir)))
  })
  
  
  # observeEvent(input$reset), 代表点击 RESET 时触发的动作,此时重新渲染 fileInput 的 ui
  observeEvent(input$reset, {
    values$file <- NULL
    output$file1 <- renderUI({
      fileInput("file1", "Step 1: Choose scRNA RData",
                accept=c('.RData, .Rds')
      )
    })
  }, ignoreNULL = F)
  
  
  output$detectfig <- renderPlot({
    library(Seurat)
    library(sctransform)
    
    expression <- expression()
    
    if(is.null(expression)){
      warning("Please upload files!")
    } 
    else{
      pbmc <- SCTransform(pbmc,vars.to.regress = "percent.mt",verbose = FALSE)
      
      pbmc <- RunPCA(pbmc, verbose = FALSE)
      pbmc <- RunUMAP(pbmc, dims = 1:30, verbose = FALSE)
      
      pbmc <- FindNeighbors(pbmc, dims = 1:30, verbose = FALSE)
      pbmc <<- FindClusters(pbmc, verbose = FALSE)
      DimPlot(pbmc, label = TRUE) + NoLegend()
    }
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".RData", sep = "")
    },
    content = function(file) {
      save(pbmc,file = file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

