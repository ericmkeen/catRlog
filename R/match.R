#' Match new photos to your catalog
#'
#' @return desc
#' @export
#'
match <- function(){

  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values
    rv <- reactiveValues()
    rv$key <- read.csv("../4 catalog/catalog key.csv",stringsAsFactors=FALSE)
    rv$refdir <- "../4 catalog/catalog/"
    rv$reflfull <- NULL
    rv$dfilter <- data.frame()
    rv$reflf <- NULL
    rv$refi <- 1
    rv$worklfull <- NULL
    rv$worklf <- NULL
    rv$worki <- 1
    rv$matchfile <- NULL
    rv$matchdata <- data.frame()
    rv$maybes <- c()
    rv$id <- ""
    rv$local <- ""

    #########################################################
    # Stage reference catalog

    observe({rv$refdir
      lf <- list.files(rv$refdir)
      lf <- paste0(rv$refdir,lf)
      rv$reflfull <- lf
      rv$reflf <- lf

      # Key columns
      key <- rv$key  ; head(key)
      locals <- as.character(key$local)

      # Setup filter parameters
      dfilter <- data.frame(lf=lf,id=NA,local=NA,fl=0,ld=0,rd=0,nicks=0,holes=0,injury=0) ; dfilter

      ls <- c() ; i=1
      for(i in 1:nrow(dfilter)){
        lfi <- as.character(dfilter$lf[i]) ; lfi

        # LDs
        splits <- strsplit(lfi,"")[[1]] ; splits
        dotchar <- which(splits==".")[3] ; dotchar
        dfilter$id[i] <- substr(lfi,1,(dotchar-2))
        dorsal <- substr(lfi,(dotchar-1),(dotchar-1)) ; dorsal
        if(dorsal=="L"){dfilter$ld[i] <- 1}
        if(dorsal=="R"){dfilter$rd[i] <- 1}
        if(dorsal=="F"){dfilter$fl[i] <- 1}

        # Key attributes
        keymatch <- which(as.character(key$id)==gsub(rv$refdir,"",dfilter$id[i])) ; keymatch
        if(length(keymatch)>0){
          dfilter$nicks[i] <- as.character(key$nick[keymatch])
          dfilter$holes[i] <- as.character(key$hole[keymatch])
          dfilter$injury[i] <- as.character(key$injury[keymatch])
          dfilter$local[i] <- as.character(locals[keymatch])
        }
      }
      print(dfilter)
      dfilter$clean <- as.numeric(as.character(dfilter$injury)) +
        as.numeric(as.character(dfilter$nicks)) +
        as.numeric(as.character(dfilter$holes))
      rv$dfilter <- dfilter
    })

    # Get ID and Local name for currently selected Reference ID
    observe({
      id <- gsub(rv$refdir,"",rv$reflf[rv$refi])
      key <- rv$key
      print(id)
      fext <- paste0(".",tools::file_ext(id))
      id <- gsub(fext,"",id)
      id <- substr(id,1,(nchar(id)-1))
      matchi <- which(key$id==id)
      #print(matchi)
      if(length(matchi)>0){
        rv$local <- as.character(key$local[matchi])
      }else{
        rv$local <- ""
      }
      #print(rv$local)
    })

    #########################################################
    # Stage matching session record

    # Create match session file
    observe({
      if(!is.null(input$session)){
        if(input$session == "Start new session"){
          matchdir <- "../3 matches/match sessions/"
          if(input$note != ""){
            newfile <- paste0(matchdir,input$analyst," ",gsub(" ","",input$note)," ",gsub(":","",gsub("-","",as.character(Sys.time()))),".csv")
          }else{
            newfile <- paste0(matchdir,input$analyst," ",gsub(":","",gsub("-","",as.character(Sys.time()))),".csv")
          }
          rv$matchfile <- newfile
        }else{
          rv$matchfile <- input$session
        }
        print(rv$matchfile)
        if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      }})

    # Stage photos to match
    observe({input$workdir
      if(!is.null(input$workdir)){
        lf <- list.files(input$workdir)
        lf <- paste0(input$workdir,lf)
        rv$worklfull <- lf
        rv$worklf <- lf
      }
    })

    #########################################################
    # Image display

    # Reference picture
    output$refid <- renderText({paste0(gsub(rv$refdir,"",rv$reflf[rv$refi]),"  |  ",rv$local)})
    output$refstatus <- renderText({paste0("Image ",rv$refi," out of ",length(rv$reflf)) })
    output$refpic <- renderImage({filename <- rv$reflf[rv$refi]
    list(src = filename,width="80%")}, deleteFile = FALSE)

    # Photo to match
    output$workid <- renderText({ if(!is.null(input$workdir)){gsub(input$workdir,"",rv$worklf[rv$worki])} })
    output$workstatus <- renderText({ if(!is.null(input$workdir)){paste0("Image ",rv$worki," out of ",length(rv$worklf))} })
    output$workpic <- renderImage({filename <- rv$worklf[rv$worki]
    list(src = filename,width="90%")}, deleteFile = FALSE)

    #########################################################
    # Data table for review

    output$matchdata <- renderDataTable(rv$matchdata)

    #########################################################
    # Reactive UI options

    # List of current match sessions to choose from
    output$session <- renderUI({
      if(input$analyst != ""){
        matchdir <- "../3 matches/match sessions/"
        lf <- list.files(matchdir) ; lf
        lf <- paste0(matchdir,lf)
        sessops <- c("Start new session",lf)
        selectInput("session","Start new matching session, or add to existing session?",selected=sessops[1],choices=sessops,width="50%")
      }
    })

    # List of photo collections to choose from
    output$workdir <- renderUI({
      workdir <- "../0 photos/photos/" ; workdir
      dirops <- list.files(workdir)
      dirops <- paste0(workdir,dirops,"/")
      if(length(dirops)>0){
        selectInput("workdir",label=h4("Select photo collection to work on:"),choices=dirops,selected=1)
      }else{ "No photo collections found! Look in catRlog > 0 photos > photos" }
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
    # Filter reference catalog

    observe({
      (input$search=="trigger") + (input$searchname=="trigger") + input$maybefilter + input$FL + input$LD + input$RD + input$nicks + input$holes + input$injury
      lf <- rv$reflfull
      dfilter <- rv$dfilter

      if(nchar(input$search)>1){
        adds <- grep(tolower(input$search),tolower(as.character(dfilter$id)))
        dfilter <- dfilter[adds,]
      }

      if(nchar(input$searchname)>1){
        adds <- grep(tolower(input$searchname),tolower(as.character(dfilter$local)))
        dfilter <- dfilter[adds,]
      }

      if(!is.null(input$maybefilter) && input$maybefilter && length(rv$maybes)>0){
        adds <- which(dfilter$lf %in% rv$maybes)
        newlf <- as.character(dfilter$lf[adds])

      }else{
        adds <- c()
        if(input$LD){adds <- c(adds,which(dfilter$ld==1)) ; print(adds)} ; adds
        if(input$RD){adds <- c(adds,which(dfilter$rd==1))} ; adds
        if(input$FL){adds <- c(adds,which(dfilter$fl==1))} ; adds
        adds <- sort(unique(adds))
        dfilter <- dfilter[adds,]

        nicks <- holes <- injury <- clean <- c()
        if(input$nicks){nicks <- which(dfilter$nicks==1)} #; nicks
        if(input$holes){holes <- which(dfilter$holes==1)} #; holes
        if(input$injury){injury <- which(dfilter$injury==1)} #; injury
        if(input$clean){clean <- which(dfilter$clean==0)} #; clean
        adds <- sort(unique(c(nicks,holes,injury,clean)))
        newlf <- as.character(dfilter$lf[adds])
      }

      rv$reflf <- newlf
      rv$refi <- 1
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
      if(newi > length(rv$reflf)){newi <- 1}
      rv$refi <- newi
    })

    # Step backward in reference collection
    observeEvent(input$refback, {
      newi <- rv$refi - 1
      if(newi < 1){newi <- length(rv$reflf)}
      rv$refi <- newi
    })

    # Cant match
    observeEvent(input$matchcant, {
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub("../0 photos/photos","",input$workdir),",",
                         "CANT,",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         NA,",",rv$worklf[rv$worki],",",NA,"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)
      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      rv$maybes <- c()
      newi <- rv$refi + 1 ; if(newi > length(rv$reflf)){newi <- 1} ; rv$refi <- newi
    })

    # New arrival
    observeEvent(input$matchnew, {
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub("../0 photos/photos","",input$workdir),",",
                         "NEW,",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         NA,",",rv$worklf[rv$worki],",",NA,"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)
      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      rv$maybes <- c()
      newi <- rv$refi + 1 ; if(newi > length(rv$reflf)){newi <- 1} ; rv$refi <- newi
    })

    # Guess match
    observeEvent(input$matchguess, {
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub("../0 photos/photos","",input$workdir),",",
                         "GUESS,",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         gsub(rv$refdir,"",rv$reflf[rv$refi]),",",
                         rv$worklf[rv$worki],",",rv$reflf[rv$refi],"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)
      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      rv$maybes <- c()
      newi <- rv$refi + 1 ; if(newi > length(rv$reflf)){newi <- 1} ; rv$refi <- newi
    })

    # 100% match
    observeEvent(input$matchyes, {
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub("../0 photos/photos","",input$workdir),",",
                         "MATCH,",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         gsub(rv$refdir,"",rv$reflf[rv$refi]),",",
                         rv$worklf[rv$worki],",",rv$reflf[rv$refi],"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)
      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}
      rv$maybes <- c()
      newi <- rv$refi + 1 ; if(newi > length(rv$reflf)){newi <- 1} ; rv$refi <- newi
    })

    # Add a reference ID to the list of maybes
    observeEvent(input$maybe,{
      rv$maybes <- c(rv$maybes,rv$reflf[rv$refi]) #; print(rv$maybes)
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
                                                    br(),br(),
                                                    uiOutput("matchcant"),br(),
                                                    uiOutput("matchnew"))),
                                    fluidRow(column(2,checkboxInput("LD","Left dorsals",value=TRUE)),
                                             column(2,checkboxInput("RD","Right dorsals",value=TRUE)),
                                             column(2,checkboxInput("FL","Flukes",value=TRUE)),
                                             column(1,checkboxInput("clean","Clean",value=TRUE)),
                                             column(1,checkboxInput("nicks","Nicks",value=TRUE)),
                                             column(1,checkboxInput("holes","Holes",value=TRUE)),
                                             column(3,checkboxInput("injury","Injury",value=TRUE))),
                                    fluidRow(column(9,imageOutput("refpic")),
                                             column(3,h3("Reference Catalog"),
                                                    textOutput("refid"),
                                                    br(),br(),
                                                    uiOutput("refback",inline=TRUE),
                                                    uiOutput("refnext",inline=TRUE),
                                                    textOutput("refstatus",inline=TRUE),
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
