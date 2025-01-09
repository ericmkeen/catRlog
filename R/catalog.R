#' Open and explore your photo-ID catalog
#'
#' @return desc
#' @import shiny
#' @import DT
#' @import shinyjs
#' @export
#'
catalog <- function(){

  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values

    rv <- reactiveValues()
    rv$refdir <- "catalog/catalog/"
    rv$reflfull <- NULL
    rv$dfilter <- data.frame()
    rv$reflf <- NULL
    rv$refi <- 1
    rv$maybes <- c()
    rv$key <- read.csv("catalog/catalog key.csv",stringsAsFactors=FALSE)

    #########################################################
    # Stage reference catalog

    observe({rv$refdir
      lf <- list.files(rv$refdir)
      lf <- paste0(rv$refdir,lf)
      rv$reflfull <- lf
      rv$reflf <- lf
      rv$sithist <- data.frame()

      # Key columns
      key <- rv$key
      locals <- as.character(key$local)
      #print(class(key$local))

      # Setup filter parameters
      dfilter <- data.frame(lf=lf,id=NA,local=NA,ld=0,rd=0,fl=0,nicks=0,holes=0,injury=0) ; dfilter

      ls <- c() ; i=1
      for(i in 1:nrow(dfilter)){
        lfi <- as.character(dfilter$lf[i]) ; lfi

        # LDs
        splits <- strsplit(lfi,"")[[1]] ; splits
        (dotchar <- which(splits=="."))
        (dotchar <- dotchar[length(dotchar)])
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
      #print(dfilter)
      dfilter$clean <- as.numeric(as.character(dfilter$injury)) +
        as.numeric(as.character(dfilter$nicks)) +
        as.numeric(as.character(dfilter$holes))
      rv$dfilter <- dfilter
    })

    #########################################################
    # Reference picture

    output$refid <- renderText(gsub(rv$refdir,"",rv$reflf[rv$refi]))
    output$refstatus <- renderText({paste0("Image ",rv$refi," out of ",length(rv$reflf)) })
    output$refpic <- renderImage({filename <- rv$reflf[rv$refi]
    list(src = filename,width="95%")}, deleteFile = FALSE)

    #########################################################
    # Produce sighting history table for selected ID

    observe({
      id <- gsub(rv$refdir,"",rv$reflf[rv$refi])
      ext <- paste0(".",tools::file_ext(rv$reflf[rv$refi]))
      id <- gsub(ext,"",id)
      id <- substr(id,1,(nchar(id)-1))
      #print(id)
      workdir <- "events/" ; workdir
      lf <- list.files(workdir) ; lf
      lf <- lf[lf!="backups"]
      mr <- data.frame()
      if(length(lf)>0){
      lf <- paste0(workdir,lf) ; lf
      i=5
      mr <- data.frame(stringsAsFactors=FALSE)
      for(i in 1:length(lf)){
        lfi <- lf[i] ; lfi
        mri <- read.csv(lfi,stringsAsFactors=FALSE) ; head(mri)
        matchi <- which(mri$id==id) ; matchi
        if(length(matchi)>0){
          mri <- data.frame(event=lfi)
          mr <- rbind(mr,mri)
        }
      }
      print(mr)
      }
      rv$sithist <- mr
    })

    output$sithist <- renderDataTable({rv$sithist})

    #########################################################
    # Create data table of catalog key for review in app

    output$key <- renderDataTable(rv$key)

    #########################################################
    # Navigation buttons

    output$refnext <- renderUI({ actionButton("refnext",h4("Next")) })

    output$refback <- renderUI({ actionButton("refback",h4("Back")) })

    #########################################################
    # Filter reference catalog

    observe({
      (input$search=="trigger") + (input$searchname=="trigger") + input$LD + input$RD
      + input$FL + input$nicks + input$holes + input$injury

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

      adds <- c()
      if(input$LD){adds <- c(adds,which(dfilter$ld==1))  } ; adds
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
      rv$reflf <- newlf
      rv$refi <- 1
    })

    observeEvent(input$refnext, {
      newi <- rv$refi + 1
      if(newi > length(rv$reflf)){newi <- 1}
      rv$refi <- newi
    })

    observeEvent(input$refback, {
      newi <- rv$refi - 1
      if(newi < 1){newi <- length(rv$reflf)}
      rv$refi <- newi
    })

  }

  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Photo-ID Catalog",

                           #########################################################
                           tabPanel("Flipbook",
                                    br(),
                                    fluidRow(column(2,h3(textOutput("refid"))),
                                             column(1),
                                             column(1,uiOutput("refback",inline=TRUE)),
                                             column(1,uiOutput("refnext",inline=TRUE)),
                                             column(1),
                                             column(2,textInput("search",h4("Search IDs:"))),
                                             column(2,textInput("searchname",h4("Search names:"))),
                                             column(2,h4(textOutput("refstatus",inline=TRUE)))),
                                    br(),
                                    fluidRow(column(12,imageOutput("refpic"))),
                                    br(),br(),
                                    fluidRow(column(2,checkboxInput("LD","Left dorsals",value=TRUE)),
                                             column(2,checkboxInput("RD","Right dorsals",value=TRUE)),
                                             column(2,checkboxInput("FL","Fluke",value=TRUE)),
                                             column(1,checkboxInput("clean","Clean",value=TRUE)),
                                             column(1,checkboxInput("nicks","Nicks",value=TRUE)),
                                             column(1,checkboxInput("holes","Holes",value=TRUE)),
                                             column(3,checkboxInput("injury","Injury",value=TRUE))),
                                    br(),br(),
                                    fluidRow(column(12,h4("Recorded in the following event files:"))),
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
