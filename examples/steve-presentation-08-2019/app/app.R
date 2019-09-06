  # output to log file
  cat('starting\n', file = stderr())

  debugMessage = TRUE

  library(shiny)
  library(leaflet)
  library(raster)

  source('helpers.R')
  source('config.R')

  ## load starting data
  load(infile)
  source('prepData.R',local=TRUE)



  ## User Interface 
ui <- bootstrapPage(

    ## set styles
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

    ## create Map placeholder
    leafletOutput("mymap", width = "100%", height = "100%"),

    ## Map toggle
    absolutePanel(top = 50, right = 10,
      selectInput("raster", "overlay", c('SATVI',names(master)), selected = 'SATVI')
    ),

    ## Time toggle
    absolutePanel(top = 120, right = 10,
      splitLayout(
            actionButton('back', '<'),
            actionButton('forward', '>')
      )
    ),
    
    # Hideable Plot pannel
    absolutePanel( 
      style = "opacity: 0.92", draggable= TRUE, bottom = 0, right = "auto",  width = "90%",
      HTML('<button data-toggle="collapse" data-target="#demo">Graph</button>'),
      tags$div(id='demo', class="collapse" ,
        wellPanel(
          splitLayout(
            actionButton('SATVITimeseries', 'SATVI-Timeseries'),
            actionButton('SATVITrend', 'SATVI-Trendlines'),
            actionButton('SyntheticControl', 'Synthetic Control'),
            actionButton('Relative', 'Relative Method'),
            actionButton('DID', 'Difference in Difference')                        
          ),
          plotOutput("myplot", click = 'plot_click')
          
        )
      )
    ),
    
    # Navigation
    absolutePanel( 
      style = "opacity: 0.92", draggable= TRUE, top = 10, left = 50,  width = 150,
      HTML('<button data-toggle="collapse" data-target="#demox">Navigate</button>'),
      tags$div(id='demox', class="collapse" ,
              #wellPanel(# selectInput("navigation", "", basename(ff), 'ID_3044.RData'),
               actionButton("navigation", "Go"),
               DT::dataTableOutput("mytable")
              # )
       )
      ),
    
    if(debugMessage){  
      # Messages
      absolutePanel( 
        style = "opacity: 0.92", draggable= TRUE, bottom = 10, right = 50,  width = 150,
        HTML('<button data-toggle="collapse" data-target="#demoz">Message</button>'),
        tags$div(id='demoz', class="collapse" ,
              verbatimTextOutput('message')
        )
      )
    }

)


## "Back End"

server <- function(input, output, session) {
  
  # Dynamic objects that trigger dependent functions when changed
  v <- reactiveValues(year = 1, plotType = 'trend', pp = pp, master = master, quants = quants, ags =ags, extraction=extraction, metadata = metadata, ev = ev)
  
  # When another site selected, load different data
  observeEvent(input$navigation, {

     ## find which row in table is selected
     i <- input$mytable_rows_selected
     
     if( is.null(i)) {return()}
          
     id <- paste0('ID_', v$metadata$id[i], '.RData')
     infile = ff[grep(id, basename(ff))]
   
     print(infile)
     load(infile)
     
     ## Reload and process data, update reactive values
     extraction <- fixExtraction(extraction)
     v$extraction <- extraction
     
     aindex <- unique(c(toposim$index))
     tb <- table(toposim$index[])
     allpixels$freq <- NA
     allpixels$freq[ match(as.numeric( names(tb) ) , aindex) ] <- tb
     padpixels$freq <- 0
     
     master <- rbind(padpixels , allpixels)
     v$master <- master
     
     pp <- spTransform(padpoly, projection(raster()))
     pp$meta <- paste(paste0(names(unlist(pp@data)), ' : ', unlist(pp@data)), collapse = '</br>')
     v$pp <- pp
   
     ags <- makeAgs(extraction, padpixels, allpixels)
     v$ags <- ags
   
      quants <- quantile( extraction$extractedTarget[], na.rm = TRUE, c(0, 1))
      v$quants <- quants

      v$ev <- ev

  })


    # debug messaging
    output$message <- renderPrint({
   
        i = v$metadata$id[input$mytable_rows_selected]
        iname = grep(i, ff)
        print(iname)
    })

  # change raster layer if 'raster' input changes
   pd <- reactive({
      if(input$raster == 'SATVI'){

          v$master$SATVI <- c( v$extraction$extractedTarget[,v$year], v$extraction$extractedReference[,v$year])
          raster(v$master[input$raster])

      } else if (input$raster == 'SYNTH') {

          v$master$SYNTH <- c( getGsRast(v$ev, v$year), rep(0, nrow(v$extraction$extractedReference)))
          raster(v$master[input$raster])

      } else {

          raster(v$master[input$raster])
      }
  })

  # dynamic color palate
  pal <- reactive({
    if(input$raster != 'SATVI'){
      colorNumeric(cols, values(pd()), na.color = "transparent")
    } else {
      colorNumeric(cols, v$quants, na.color = "transparent")
    }
  })


  
  
  # For toggling through time
  observeEvent(input$back, { if(v$year == 1) {
                                v$year = ncol(v$extraction$extractedTarget)
                             } else { 
                                v$year <- v$year -1 
                             }
                           })
  observeEvent(input$forward,{if(v$year == ncol(v$extraction$extractedTarget)) {
                                v$year = 1
                             } else { 
                                v$year <- v$year +1 
                             }
                           })
                           
  #change year based on graph click
  observeEvent(input$plot_click, { yy <- round(input$plot_click$x); 
                                   if(yy > 0 & yy <= ncol(v$extraction$extractedTarget)) {
                                      v$year <- yy
                                   }
                                  }
                                  )
                                  
                                  
  ## change plot type on button click                                  
  observeEvent(input$SATVITimeseries, {v$plotType <- 'timeseries'})
  observeEvent(input$SATVITrend, {v$plotType <- 'trend'})
  observeEvent(input$SyntheticControl, {v$plotType <- 'Synthetic Control'})
  observeEvent(input$Relative, {v$plotType <- 'Relative'})
  observeEvent(input$DID, {v$plotType <- 'DID'})      
  
  
  
  # Generate plot
  output$myplot <- renderPlot( 
    if(v$plotType == 'timeseries'){
      tsPlot(v$ags, v$year) 
    } else if (v$plotType == 'trend'){
      trendPlot2(v$ags,v$year, v$pp)
    } else if (v$plotType == 'Synthetic Control'){
        synthPlot2(v$ev, v$year)
    } else if (v$plotType == 'Relative'){
        plotRel(v$ev, v$year)
    } else if (v$plotType == 'DID'){
        plotDID(v$ev, v$year)
    }
  )
  
  # Table
  output$mytable = DT::renderDataTable({
    metadata
  }, selection = 'single')

    
  # Maps / Rasters
  output$mymap <- renderLeaflet({
      leaflet() %>%
      addTiles(urlTemplate = '//server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')
      

  })

  ## change polygon if a new dataset loaded
  observe({
      leafletProxy('mymap') %>%
      clearShapes() %>%
      addPolygons(data = v$pp, fillColor='transparent', popup =  ~ meta, group = "Polygons") %>%
      setView(lng = coordinates(v$pp)[1], lat = coordinates(v$pp)[2], zoom = 16)
  })

   ## change raster if a new overlay / dataset is loaded
  observe({
      leafletProxy('mymap') %>%
      clearImages() %>%
      addRasterImage(pd(),colors = pal(), opacity = 0.5, group = 'Overlay') %>% 
      addLayersControl(overlayGroups = c("Overlay", "Polygons"))%>%
      clearControls() %>%
      addLegend(pal = pal(), values = values(pd()),title = "", position = 'bottomright')
  
  })
  
  
}


shinyApp(ui = ui, server = server)
