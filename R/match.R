#' Match new photo collections to your reference catalog
#'
#' @param collections_to_match Path to folder containing subfolders of photo collections that you wish to match against your reference catalog. The default follows the instructions for the `catRlog` system setup.
#' @param catalog_photos Path to folder with catalog ID photos. The default follows the instructions for the `catRlog` system setup.
#' @param catalog_key Path to catalog key `.csv`. The default follows the instructions for the `catRlog` system setup.
#' @param match_sessions Path to folder where matching sessions will be saved. The default follows the instructions for the `catRlog` system setup.
#' @param filters Character vector of column names within the catalog key that can be used to filter the catalog. Up to three can be used.
#' @return Shiny app. See the [vignette](https://ericmkeen.github.io/catRlog/) for a detailed user guide.
#' @export
#' @import shiny
#' @import DT
#' @import shinyjs
#' @import dplyr
#' @import tidyselect
#'
match <- function(collections_to_match = 'photos/photos/',
                  catalog_photos = 'catalog/catalog/',
                  catalog_key = 'catalog/catalog key.csv',
                  match_sessions = "matches/match sessions/",
                  filters = c('injury', 'nick', 'hole')){

  if(FALSE){
    collections_to_match = 'photos/photos/'
    catalog_photos = 'catalog/catalog/'
    catalog_key = 'catalog/catalog key.csv'
    filters = c('injury', 'nick', 'hole')
    rv <- list()
  }
  #########################################################
  #########################################################

  #########################################################
  # Setup photo key

  rvkey <- read.csv(catalog_key,stringsAsFactors=FALSE)
  (lf <- list.files(catalog_photos))
  (lf <- paste0(catalog_photos,lf))

  # Feature options
  (splits <- lapply(strsplit(lf, '\\.'), '[[', 1) %>% unlist)
  (features <- sapply(splits, function(x){substr(x, nchar(x), nchar(x))}))
  (ids <- gsub(catalog_photos, '', sapply(splits, function(x){substr(x, 1, (nchar(x)-1))})))

  # Build photo key
  (photo_key <- data.frame(file=lf, id=ids, feature=features))
  (key2join <-
      rvkey %>%
      dplyr::select(id, local, tidyselect::any_of(filters)))
  (photo_key <- dplyr::left_join(photo_key, key2join))
  #print(head(photo_key))

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
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values
    rv <- reactiveValues()
    rv$key <- rvkey
    rv$refi <- 1
    rv$dfilter <- photo_key
    rv$filter_feature <- rv$filter1 <- rv$filter2 <- rv$filter3 <- 1:nrow(photo_key)
    rv$worklfull <- NULL
    rv$worklf <- NULL
    rv$worki <- 1
    rv$matchfile <- NULL
    rv$matchdata <- data.frame()
    rv$maybes <- c()
    rv$id <- ""
    rv$local <- ""

    #########################################################
    # Filter UIs

    output$filter_feature <- renderUI({
      if(!is.null(photo_key$feature)){
        #print('updating filter feature options...')
        column(2, selectInput('filter_feature',
                              label='Features:',
                              choices = ops_feature,
                              selected = ops_feature,
                              multiple = TRUE,
                              width = '100%'))}})

    output$filter1 <- renderUI({
      if(length(filters)>=1){
        #print('updating filter 1 options...')
        column(2, selectInput('filter1',
                              label=filters[1],
                              choices = ops_filter1,
                              selected = ops_filter1,
                              multiple = TRUE,
                              width = '100%'))}})

    output$filter2 <- renderUI({
      if(length(filters)>=2){
        #print('updating filter 2 options...')
        column(2, selectInput('filter2',
                              label=filters[2],
                              choices = ops_filter2,
                              selected = ops_filter2,
                              multiple = TRUE,
                              width = '100%'))}})

    output$filter3 <- renderUI({
      if(length(filters)>=3){
        #print('updating filter 3 options...')
        column(2, selectInput('filter3',
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

      #print('combining all filter returns...')
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

      rv$dfilter <- dfilter
      rv$refi <- 1
    })

    #########################################################
    # Stage matching session record

    # Create match session file
    observe({
      if(!is.null(input$session)){
        if(input$session == "Start new session"){
          matchdir <- match_sessions
          if(input$note != ""){
            newfile <- paste0(matchdir,input$analyst," ",gsub(" ","",input$note)," ",gsub(":","",gsub("-","",as.character(Sys.time()))),".csv")
          }else{
            newfile <- paste0(matchdir,input$analyst," ",gsub(":","",gsub("-","",as.character(Sys.time()))),".csv")
          }
          rv$matchfile <- newfile
        }else{
          rv$matchfile <- input$session
        }
        #print(rv$matchfile)
        if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      }})

    # Stage photos to match
    observe({input$workdir
      if(!is.null(input$workdir)){
        lf <- list.files(input$workdir)
        lf <- paste0(input$workdir,'/',lf)
        rv$worklfull <- lf
        rv$worklf <- lf
      }
    })

    #########################################################
    # Image display

    # Reference picture
    output$refid <- renderText(rv$dfilter$id[rv$refi])
    output$reflocal <- renderText(rv$dfilter$local[rv$refi])
    output$refstatus <- renderText({paste0("Image ",rv$refi," out of ",nrow(rv$dfilter),' in catalog') })
    output$refpic <- renderImage({list(src = rv$dfilter$file[rv$refi],width="95%")}, deleteFile = FALSE)

    # Photo to match
    output$workid <- renderText({ if(!is.null(input$workdir)){gsub(input$workdir,"",rv$worklf[rv$worki])} })
    output$workstatus <- renderText({ if(!is.null(input$workdir)){paste0("Image ",rv$worki," out of ",length(rv$worklf))} })
    output$workpic <- renderImage({list(src = rv$worklf[rv$worki],width="95%")}, deleteFile = FALSE)

    #########################################################
    # Data table for review

    output$matchdata <- renderDataTable(rv$matchdata)

    #########################################################
    # Reactive UI options

    # List of current match sessions to choose from
    output$session <- renderUI({
      if(input$analyst != ""){
        matchdir <- match_sessions
        lf <- list.files(matchdir) ; lf
        lf <- paste0(matchdir,lf)
        sessops <- c("Start new session",lf)
        selectInput("session","Start new matching session, or add to existing session?",selected=sessops[1],choices=sessops,width="50%")
      }
    })

    # List of photo collections to choose from
    output$workdir <- renderUI({
      workdir <- collections_to_match ; workdir
      (dirops <- list.files(workdir))
      dirops <- paste0(workdir,dirops)
      if(length(dirops)>0){
        selectInput("workdir",label=h4("Select photo collection to work on:"),choices=dirops,selected=1)
      }else{ "No photo collections found! Check the path you supplied in the input collections_to_match" }
    })

    # Setup button to step forward in photo collection
    output$worknext <- renderUI({
      if(length(rv$worklf)>0){ actionButton("worknext",h4("Next")) }
    })

    # Setup button to step backward in photo collection
    output$workback <- renderUI({
      if(length(rv$worklf)>0){ actionButton("workback",h4("Back")) }
    })

    # Setup button to step forward in reference catalog
    output$refnext <- renderUI({ actionButton("refnext",h4("Next")) })

    # Setup button to step backward in reference catalog
    output$refback <- renderUI({ actionButton("refback",h4("Back")) })

    # Setup button if Can't match
    output$matchcant <- renderUI({
      if(!is.null(rv$matchfile)){
        actionButton("matchcant",h4("Photo too poor!"))  }else{"Setup match file first!"}
    })

    # Setup button if New Arrival
    output$matchnew <- renderUI({
      if(!is.null(rv$matchfile)){
        actionButton("matchnew",h4("No match! New whale!"))  }else{"Setup match file first!"}
    })

    # Setup button if uncertain match
    output$matchguess <- renderUI({
      if(!is.null(rv$matchfile)){
        actionButton("matchguess",h4("Low-confidence Match"))  }else{"Setup match file first!"}
    })

    # Setup button if 100% match
    output$matchyes <- renderUI({
      if(!is.null(rv$matchfile)){
        actionButton("matchyes",h4("100% Match!"))  }else{"Setup match file first!"}
    })

    # Option to subset reference catalog to list of Maybes
    output$maybes <- renderUI({
      if(length(rv$maybes)>0){
        checkboxInput("maybefilter","Filter to Maybes?",value=FALSE)
      }
    })

    #########################################################
    # Filter photo collection

    # On setup page: filter to unmatched photos?
    observe({
      input$notdone
      if(input$notdone){
        if(nrow(rv$matchdata)>0){
          matches <- rv$matchdata
          dones <- matches[,7]
          dones <- which(rv$worklfull %in% dones)
          rv$worklf <- rv$worklfull[-dones]
        }}
    })

    #########################################################
    # What to do when buttons are pressed

    # Step forward in photo collection
    observeEvent(input$worknext, {
      newi <- rv$worki + 1
      if(newi > length(rv$worklf)){
        showModal(modalDialog(title="You have reached the end of the match set!",
                              "Bringing you back to the first file...",
                              size="m",easyClose=TRUE))
        newi <- 1
      }
      rv$worki <- newi
    })

    # Step backward in photo collection
    observeEvent(input$workback, {
      newi <- rv$worki - 1
      if(newi < 1){newi <- length(rv$worklf)}
      rv$worki <- newi
    })

    # Step forward in reference collection
    observeEvent(input$refnext, {
      newi <- rv$refi + 1
      if(newi > nrow(rv$dfilter)){newi <- 1}
      rv$refi <- newi
    })

    # Step backward in reference collection
    observeEvent(input$refback, {
      newi <- rv$refi - 1
      if(newi < 1){newi <- nrow(rv$dfilter)}
      rv$refi <- newi
    })

    # Cant match
    observeEvent(input$matchcant, {
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub(collections_to_match,"",input$workdir),",",
                         "CANT,",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         NA,",",rv$worklf[rv$worki],",",NA,"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)
      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      rv$maybes <- c()
      newi <- rv$refi + 1 ; if(newi > nrow(rv$dfilter)){newi <- 1} ; rv$refi <- newi
    })

    # New arrival
    observeEvent(input$matchnew, {
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub(collections_to_match,"",input$workdir),",",
                         "NEW,",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         NA,",",rv$worklf[rv$worki],",",NA,"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)
      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      rv$maybes <- c()
      newi <- rv$refi + 1 ; if(newi > nrow(rv$dfilter)){newi <- 1} ; rv$refi <- newi
    })

    # Guess match
    observeEvent(input$matchguess, {
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub(collections_to_match,"",input$workdir),",",
                         "GUESS,",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         rv$dfilter$id[rv$refi],",",
                         rv$worklf[rv$worki],",",rv$dfilter$file[rv$refi],"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)
      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      rv$maybes <- c()
      newi <- rv$refi + 1 ; if(newi > nrow(rv$dfilter)){newi <- 1} ; rv$refi <- newi
    })

    # 100% match
    observeEvent(input$matchyes, {
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub(collections_to_match,"",input$workdir),",",
                         "MATCH,",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         rv$dfilter$id[rv$refi],",",
                         rv$worklf[rv$worki],",",rv$dfilter$file[rv$refi],"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)
      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      rv$maybes <- c()
      newi <- rv$refi + 1 ; if(newi > nrow(rv$dfilter)){newi <- 1} ; rv$refi <- newi
    })

    # Add a reference ID to the list of maybes
    observeEvent(input$maybe,{
      rv$maybes <- c(rv$maybes,rv$dfilter$file[rv$refi]) #; print(rv$maybes)
    })

  }

  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Photo-ID Matching",
                           #########################################################
                           tabPanel("set up",
                                    fluidRow(column(12,h3("Analyst details"),
                                                    br(),textInput("analyst","Enter your name:",value=""),
                                                    br(),textInput("note","Session description:",value=""),
                                                    br(),
                                                    uiOutput("session"))),br(),
                                    fluidRow(column(12,h3("Setup Photos to Match"),
                                                    br(),uiOutput("workdir"),
                                                    br(),
                                                    checkboxInput("notdone","Filter to as-yet unprocessed photos?",value=FALSE))),

                                    br()),

                           #########################################################
                           tabPanel("GO MATCH",
                                    fluidRow(column(9,imageOutput("workpic")),
                                             column(3,h3("Photos to Match"),
                                                    textOutput("workid"),br(),
                                                    uiOutput("workback",inline=TRUE),
                                                    uiOutput("worknext",inline=TRUE),
                                                    textOutput("workstatus",inline=FALSE),
                                                    br(),
                                                    uiOutput("matchcant"),br(),
                                                    uiOutput("matchnew"))),
                                    fluidRow(column(1, h4('Filters:')),
                                             uiOutput('filter_feature'),
                                             uiOutput('filter1'),
                                             uiOutput('filter2'),
                                             uiOutput('filter3'),
                                             column(3, h3("Reference Catalog"))
                                    ),
                                    fluidRow(column(9,imageOutput("refpic")),
                                             column(3,
                                                    textOutput("refid"),
                                                    br(),br(),
                                                    uiOutput("refback",inline=TRUE),
                                                    uiOutput("refnext",inline=TRUE),
                                                    textOutput("refstatus"),
                                                    br(),br(),
                                                    uiOutput("matchyes"),br(),br(),
                                                    actionButton("maybe","(Add to Maybes)"),
                                                    uiOutput("maybes"))),
                                    fluidRow(column(4,textInput("search",h4("Search IDs:"))),
                                             column(4,textInput("searchname",h4("Search names:"))),
                                             column(4,uiOutput("matchguess")))
                           ),

                           #########################################################
                           tabPanel("review",
                                    br(),
                                    fluidRow(column(12,dataTableOutput("matchdata"))))
  ))

  #########################################################
  #########################################################
  #########################################################

  shinyApp(ui = ui, server = server)

  #########################################################
  #########################################################

}
