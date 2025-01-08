#' Register new IDs in your catalog
#'
#' @return desc
#' @export
#'
register_new_ids <- function(){

  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values

    rv <- reactiveValues()
    rv$key <- read.csv("../4 catalog/catalog key.csv",stringsAsFactors=FALSE) #; head(rv$key)
    rv$refi <- 1
    rv$worklfull <- NULL
    rv$worklf <- NULL
    rv$worki <- 1
    rv$matchdata <- data.frame()
    rv$news <- data.frame()
    rv$newarrivals <- data.frame()

    #########################################################
    # Select file of reviewed matches

    output$matchfiles <- renderUI({
      workdir <- "../3 matches/reviewed matches/" ; workdir
      dirops <- list.files(workdir)
      dirops <- paste0(workdir,dirops)
      if(length(dirops)>0){
        selectInput("matchfiles",label=h4("Select file of reviewed matches:"),
                    choices=dirops,selected=1,multiple=FALSE,width="90%")
      }else{ "No reviewed match files found! Look in catRlog > 3 matches > reviewed matches" }
    })

    #########################################################
    # Import reviewed matches

    output$import <- renderUI({
      if(!is.null(input$matchfiles) && length(input$matchfiles)>0){
        actionButton("import",h4("Import reviewed matches"),width="40%")
      }
    })

    observeEvent(input$import,{
      #lf <- list.files(workdir) ; lf
      #lf <- paste0(workdir,lf) ; lf
      lf <- input$matchfiles
      ms <- data.frame()
      for(i in 1:length(lf)){
        msi <- read.csv(lf[i],stringsAsFactors=FALSE)
        ms <- rbind(ms,msi)
      }
      #print(ms)
      rv$matchdata <- ms
    })

    #########################################################
    # Review data table and status report

    output$matchdata <- renderDataTable(rv$matchdata)
    output$key <- renderDataTable(rv$key)

    output$status <- renderUI({
      HTML(paste("New whales = ",nrow(rv$matchdata[rv$matchdata$action=="NEW",])))
    })

    #########################################################
    # Stage photos of new whales

    # Get list of new whales from match session
    observe({
      if(!is.null(rv$matchdata) && nrow(rv$matchdata)>0){
        news <- which(rv$matchdata$action=="NEW")
        nas <- which(is.na(rv$matchdata$id))
        news <- nas[which(nas %in% news)]
        rv$worklf <- rv$matchdata$path[news]
      }
    })

    # Determine current last catalog ID
    output$catalogstatus <- renderText({paste0("Current last catalog ID: ",rv$key$id[nrow(rv$key)]) })

    #########################################################
    # Image display

    # Display picture of new arrivals recently registered
    output$refid <- renderText(as.character(rv$newarrivals$newid[rv$refi]))
    output$refstatus <- renderText({paste0("Image ",rv$refi," out of ",nrow(rv$newarrivals)) })
    output$refpic <- renderImage({filename <- as.character(rv$newarrivals$path)[rv$refi]
    list(src = filename,width="80%")}, deleteFile = FALSE)

    # Display picture of new arrival that still needs to be registered
    output$workid <- renderText({ if(length(rv$worklf)>0){rv$worklf[rv$worki]} })
    output$workstatus <- renderText({ if(length(rv$worklf)>0){paste0("Image ",rv$worki," out of ",length(rv$worklf))} })
    output$workpic <- renderImage({filename <- rv$worklf[rv$worki]
    list(src = filename,width="90%")}, deleteFile = FALSE)

    #########################################################
    # Navigation

    # Flip forward in new arrivals that need to be registered
    output$worknext <- renderUI({
      if(length(rv$worklf)>0){ actionButton("worknext",h4("Next")) }
    })

    observeEvent(input$worknext, {
      newi <- rv$worki + 1
      if(newi > length(rv$worklf)){newi <- 1}
      rv$worki <- newi
    })

    # Flip backward in new arrivals that need to be registered
    output$workback <- renderUI({
      if(length(rv$worklf)>0){ actionButton("workback",h4("Back")) }
    })

    observeEvent(input$workback, {
      newi <- rv$worki - 1
      if(newi < 1){newi <- length(rv$worklf)}
      rv$worki <- newi
    })

    # Flip forward in new arrivals that were just recently registered
    output$refnext <- renderUI({
      if(nrow(rv$newarrivals)>0){actionButton("refnext",h4("Next")) }})

    observeEvent(input$refnext, {
      newi <- rv$refi + 1
      if(newi > nrow(rv$newarrivals)){newi <- 1}
      rv$refi <- newi
    })

    # Flip forward in new arrivals that were just recently registered
    output$refback <- renderUI({
      if(nrow(rv$newarrivals)>0){actionButton("refback",h4("Back")) }})

    observeEvent(input$refback, {
      newi <- rv$refi - 1
      if(newi < 1){newi <- nrow(rv$newarrivals)}
      rv$refi <- newi
    })

    #########################################################
    # Register ID

    # Button to assign ID and add to catalog
    output$add <- renderUI({
      if(nchar(input$id) > 1){
        actionButton("add",label=h4("Assign ID (and add to catalog, if needed)"))
      }
    })

    observeEvent(input$add,{
      #path <- ms$path[ms$action=="NEW"][1] ; path
      #id <- "fw0094"
      #newname <- "FN-2187"
      #ftr <- "R"

      path <- rv$worklf[rv$worki]
      id <- input$id
      ftr <- input$ftr
      newname <- input$name
      key <- rv$key

      #print(key$id)
      #print(id)
      if(tolower(as.character(id)) %in% tolower(as.character(key$id))){
        print("FYI! This ID was already taken in the catalog -- the key was not updated, but the photo feature was added to the catalog folder.")
      }else{
        # Save current copy of catalog key as backup
        write.csv(key,file=paste0("../4 catalog/old keys/catalog key ",as.character(Sys.Date()),".csv"),
                  quote=FALSE,row.names=FALSE,na="")

        # Update catalog key
        blankline <- rep("",times=ncol(key)) ; blankline
        blankline[which(names(key)=="id")] <- id
        blankline[which(names(key)=="local")] <- newname
        key <- rbind(key,blankline) ; key
        write.csv(key,file="../4 catalog/catalog key.csv",quote=FALSE,row.names=FALSE,na="")
        rv$key <- read.csv("../4 catalog/catalog key.csv",stringsAsFactors=FALSE)
      }

      # Update catalog folder of photos
      pathext <- tools::file_ext(path) ; pathext
      pathext <- paste0(".",pathext) ; pathext
      newid <- paste0(id,ftr,pathext) ; newid
      newcatfile <- paste0("../4 catalog/catalog/",newid)  ; newcatfile
      file.copy(from=path,to=newcatfile)

      # Add this to the growing list of new arrivals during the match session
      newarr <- data.frame(newid=as.character(newid),path=as.character(path))
      rv$newarrivals <- rbind(rv$newarrivals,newarr)

      # Update the match session file with this id
      ms <- rv$matchdata
      ms$id[which(ms$path==path)] <- newid
      rv$matchdata <- ms
      write.csv(rv$matchdata,file=input$matchfiles,quote=FALSE,row.names=FALSE)

    })

    #output$addna <- renderUI({
    #  if(nrow(rv$newarrivals) > 0){
    #    actionButton("addna",label=h4(HTML("Link this new whale <br/> to photo above")))
    #  }
    #})

  }


  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Register new whales",
                           #########################################################
                           tabPanel("set up",
                                    fluidRow(column(12,
                                                    br(),uiOutput("matchfiles"),
                                                    br())),
                                    fluidRow(column(12,uiOutput("import"))),
                                    br(),
                                    fluidRow(column(12,htmlOutput("status"))),
                                    br()),

                           #########################################################
                           tabPanel("REGISTRATION!",
                                    br(),
                                    fluidRow(column(12,textOutput("catalogstatus"))),
                                    br(),
                                    fluidRow(column(9,imageOutput("workpic")),
                                             column(3,h3("New whales to registered"),
                                                    textOutput("workid"),br(),
                                                    uiOutput("workback",inline=TRUE),
                                                    uiOutput("worknext",inline=TRUE),
                                                    textOutput("workstatus",inline=FALSE),
                                                    br(),
                                                    radioButtons("ftr",label="Feature:",choices=c("L","R","F"),inline=TRUE,selected=NULL,width="100%"),
                                                    textInput("id",label="New ID:"),
                                                    textInput("name",label="New name"),
                                                    uiOutput("add")
                                             )),
                                    fluidRow(column(9,imageOutput("refpic")),
                                             column(3,h3("New arrivals from this registration session"),
                                                    textOutput("refid"),br(),
                                                    uiOutput("refback",inline=TRUE),
                                                    uiOutput("refnext",inline=TRUE),
                                                    textOutput("refstatus",inline=TRUE),
                                                    br(),br()))
                           ),

                           #########################################################
                           tabPanel("review",
                                    tabsetPanel(
                                      tabPanel("Reviewed matches", br(), fluidRow(column(12,dataTableOutput("matchdata")))),
                                      tabPanel("Catalog key", br(), fluidRow(column(12,dataTableOutput("key"))))
                                    ))
  ))

  #########################################################
  #########################################################
  #########################################################

  shinyApp(ui = ui, server = server)

  #########################################################
  #########################################################

}
