#' Open and explore your photo-ID catalog
#'
#' @param photos Path to folder with catalog ID photos. The default follows the instructions for the `catRlog` system setup.
#' @param key Path to catalog key `.csv`. The default follows the instructions for the `catRlog` system setup.
#' @param events Path to folder with events spreadsheets. The default follows the instructions for the `catRlog` system setup.
#' @param filters Character vector of column names within the catalog key that can be used to filter the catalog. Up to three can be used.
#'
#' @return Shiny app. See the [vignette](https://ericmkeen.github.io/catRlog/) for a detailed user guide.
#' @import shiny
#' @import DT
#' @import shinyjs
#' @import dplyr
#' @import tidyselect
#' @export
#'
catalog <- function(photos = 'catalog/catalog/',
                    key = 'catalog/catalog key.csv',
                    events = 'events/',
                    filters = c('injury', 'nick', 'hole')){

  if(FALSE){
    photos = 'catalog/catalog/'
    key = 'catalog/catalog key.csv'
    events = 'events/'
    filters = c('injury', 'nick', 'hole')
    rv <- list()
  }
  #########################################################
  #########################################################
  # Setup photo key

  rvkey <- read.csv(key,stringsAsFactors=FALSE)
  (lf <- list.files(photos))
  (lf <- paste0(photos,lf))

  # Feature options
  (splits <- lapply(strsplit(lf, '\\.'), '[[', 1) %>% unlist)
  (features <- sapply(splits, function(x){substr(x, nchar(x), nchar(x))}))
  (ids <- gsub(photos, '', sapply(splits, function(x){substr(x, 1, (nchar(x)-1))})))

  # Build photo key
  (photo_key <- data.frame(file=lf, id=ids, feature=features))
  (key2join <-
      rvkey %>%
      dplyr::select(id, local, tidyselect::any_of(filters)))
  (photo_key <- dplyr::left_join(photo_key, key2join))
  #print(head(photo_key))

  # Compile all events
  all_events <- compile_events(events)

  # Setup filter options
  ops_feature <- sort(unique(photo_key$feature))
  ops_filter1 <- ops_filter2 <- ops_filter3 <- NA
  if(length(filters)>=1){
    ops_filter1 <- sort(unique(photo_key[ ,which(names(photo_key)==filters[1])]))
  }
  if(length(filters)>=2){
    ops_filter2 <- sort(unique(photo_key[ ,which(names(photo_key)==filters[2])]))
  }
  if(length(filters)>=3){
    ops_filter3 <- sort(unique(photo_key[ ,which(names(photo_key)==filters[3])]))
  }


  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values

    rv <- reactiveValues()
    rv$filter_set <- list()
    rv$key <- rvkey
    rv$photo_key <- photo_key
    rv$filter_feature <- rv$filter1 <- rv$filter2 <- rv$filter3 <- 1:nrow(photo_key)
    rv$refi <- 1
    rv$maybes <- c()

    #########################################################
    # Filter UIs

    output$filter_feature <- renderUI({
      if(!is.null(photo_key$feature)){
        #print('updating filter feature options...')
        column(3, selectInput('filter_feature',
                              label='Features:',
                              choices = ops_feature,
                              selected = ops_feature,
                              multiple = TRUE,
                              width = '100%'))}})

    output$filter1 <- renderUI({
      if(length(filters)>=1){
        #print('updating filter 1 options...')
        column(3, selectInput('filter1',
                              label=filters[1],
                              choices = ops_filter1,
                              selected = ops_filter1,
                              multiple = TRUE,
                              width = '100%'))}})

    output$filter2 <- renderUI({
      if(length(filters)>=2){
        #print('updating filter 2 options...')
        column(3, selectInput('filter2',
                              label=filters[2],
                              choices = ops_filter2,
                              selected = ops_filter2,
                              multiple = TRUE,
                              width = '100%'))}})

    output$filter3 <- renderUI({
      if(length(filters)>=3){
        #print('updating filter 3 options...')
        column(3, selectInput('filter3',
                              label=filters[3],
                              choices = ops_filter3,
                              selected = ops_filter3,
                              multiple = TRUE,
                              width = '100%'))}})

    #########################################################
    # Filter reference catalog

    # Record updates to any filter settings
    observe({ if(!is.null(input$filter_feature)){
      rv$filter_feature <- which(photo_key$feature %in% input$filter_feature)
    }else{
      rv$filter_feature <- 1:nrow(photo_key)
    }})
    observe({ if(!is.null(input$filter1)){
      rv$filter1 <- which(photo_key[,which(names(photo_key)==filters[1])] %in% input$filter1)
    }else{
      rv$filter1 <- 1:nrow(photo_key)
    }})
    observe({ if(!is.null(input$filter2)){
      rv$filter2 <- which(photo_key[,which(names(photo_key)==filters[2])] %in% input$filter2)
    }else{
      rv$filter2 <- 1:nrow(photo_key)
    }})
    observe({ if(!is.null(input$filter3)){
      rv$filter3 <- which(photo_key[,which(names(photo_key)==filters[3])] %in% input$filter3)
    }else{
      rv$filter3 <- 1:nrow(photo_key)
    }})

    observe({
      list(input$search,
           input$searchname,
           rv$filter_feature,
           rv$filter1,
           rv$filter2,
           rv$filter3)

      dfilter <- photo_key

      search_adds <- 1:nrow(photo_key)
      if(nchar(input$search)>1){
        search_adds <- grep(tolower(input$search),tolower(as.character(dfilter$id)))
      }

      searchname_adds <- 1:nrow(photo_key)
      if(nchar(input$searchname)>1){
        print('filtering photo_key by search name')
        searchname_adds <- grep(tolower(input$searchname),tolower(as.character(dfilter$local)))
      }

      print('combining all filter returns...')
      all_adds <- 1:nrow(photo_key)
      adds <- all_adds[which(all_adds %in% rv$filter_feature &
                               all_adds %in% rv$filter1 &
                               all_adds %in% rv$filter2 &
                               all_adds %in% rv$filter3 &
                               all_adds %in% search_adds &
                               all_adds %in% searchname_adds)]

      adds <- sort(unique(adds))
      #print(adds)
      dfilter <- dfilter[adds,]
      #print(nrow(dfilter))

      rv$photo_key <- dfilter
      rv$refi <- 1
    })

    #########################################################
    # Navigation

    observeEvent(input$refnext, {
      newi <- rv$refi + 1
      if(newi > nrow(rv$photo_key)){newi <- 1}
      rv$refi <- newi
    })

    observeEvent(input$refback, {
      newi <- rv$refi - 1
      if(newi < 1){newi <- nrow(rv$photo_key)}
      rv$refi <- newi
    })

    #########################################################
    # Reference picture

    output$refid <- renderText(rv$photo_key$id[rv$refi])
    output$reflocal <- renderText(rv$photo_key$local[rv$refi])
    output$refstatus <- renderText({paste0("Image ",rv$refi," out of ",nrow(rv$photo_key),' in catalog') })
    output$refpic <- renderImage({list(src = rv$photo_key$file[rv$refi],width="95%")}, deleteFile = FALSE)

    #########################################################
    # Produce sighting history table for selected ID

    output$sithist <- renderDataTable({
      all_events %>%
        filter(id == rv$photo_key$id[rv$refi])
    })

    #########################################################
    # Create data table of catalog key for review in app

    output$key <- renderDataTable(rv$key)

    #########################################################
    # Navigation buttons
    output$refnext <- renderUI({ actionButton("refnext",h4("Next")) })
    output$refback <- renderUI({ actionButton("refback",h4("Back")) })

  }

  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Photo-ID Catalog",

                           #########################################################
                           tabPanel("Flipbook",
                                    br(),
                                    fluidRow(column(2,h3(textOutput("refid"))),
                                             column(2,h3(textOutput("reflocal"))),
                                             column(1,uiOutput("refback",inline=TRUE)),
                                             column(1,uiOutput("refnext",inline=TRUE)),
                                             column(2,textInput("search",h4("Search IDs:"))),
                                             column(2,textInput("searchname",h4("Search names:"))),
                                             column(2,h4(textOutput("refstatus",inline=TRUE)))),
                                    br(),
                                    fluidRow(column(12,imageOutput("refpic"))),
                                    br(),br(),
                                    fluidRow(uiOutput('filter_feature'),
                                             uiOutput('filter1'),
                                             uiOutput('filter2'),
                                             uiOutput('filter3'),
                                    ),
                                    br(),br(),
                                    fluidRow(column(12,h4("Recorded in the following events:"))),
                                    fluidRow(column(12,dataTableOutput("sithist")))

                           ),

                           #########################################################
                           tabPanel("Review",
                                    br(),
                                    fluidRow(column(12,dataTableOutput("key"))))
  ))

  #########################################################
  #########################################################
  #########################################################

  shinyApp(ui = ui, server = server)

  #########################################################
  #########################################################
}

