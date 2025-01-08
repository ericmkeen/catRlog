#' Launch analysis dashboard
#'
#' @return desc
#' @export
#'
analysis_dashboard <- function(){

  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values

    rv <- reactiveValues()
    rv$events <- data.frame()
    rv$groups <- data.frame()
    rv$ilv <- data.frame()
    rv$dyads <- data.frame()
    rv$mx <- data.frame()
    rv$caphist <- data.frame()

    #########################################################
    # Data product observes - update data tables in app with latest versions of datasets

    observe({
      input$begin
      datadir <- "../6 analysis/datasets/" ; datadir
      lf <- list.files(datadir) ; lf
      lf <- paste0(datadir,lf) ; lf
      lf <- lf[grep("csv",lf)] ; lf

      lfmatch <- grep("events",lf)
      if(length(lfmatch)>0){rv$events <- read.csv(lf[lfmatch],sep=",",stringsAsFactors=FALSE)}

      lfmatch <- grep("groups",lf)
      if(length(lfmatch)>0){rv$groups <- read.csv(lf[lfmatch],sep=",",stringsAsFactors=FALSE)}

      lfmatch <- grep("ILV",lf)
      if(length(lfmatch)>0){rv$ilv <- read.csv(lf[lfmatch],sep=",",stringsAsFactors=FALSE)}

      lfmatch <- grep("capture",lf)
      if(length(lfmatch)>0){rv$caphist <- read.csv(lf[lfmatch],sep=",",stringsAsFactors=FALSE)}

      lfmatch <- grep("dyads",lf)
      if(length(lfmatch)>0){rv$dyads <- read.csv(lf[lfmatch],sep=",",stringsAsFactors=FALSE)}

      lfmatch <- grep("matrix",lf)
      if(length(lfmatch)>0){rv$mx <- read.csv(lf[lfmatch],sep=",",stringsAsFactors=FALSE)}
    })

    #########################################################
    # Datatable Outputs

    output$events <- DT::renderDataTable(rv$events)
    output$groups <- DT::renderDataTable(rv$groups)
    output$ilv <- DT::renderDataTable(rv$ilv)
    output$caphist <- DT::renderDataTable(rv$caphist)
    output$dyads <- DT::renderDataTable(rv$dyads)
    output$mx <- DT::renderDataTable(rv$mx)

    #########################################################
    # Add option to set minimum sighting threshold

    output$sitmin <- renderUI({
      if(input$min!="all"){
        textInput("sitmin","Only use whales with X or more sightings:",value="1",width="95%")
      }
    })

    #########################################################
    #########################################################
    #########################################################
    # PROCESS

    observeEvent(input$begin,{
      rds <- TRUE
      csv <- TRUE
      var="sri"

      #########################################################
      # Harvest settings

      historical <- input$historical
      demog <- input$demo
      distinct <- input$distinct ;
      if(distinct!="all"){distinct <- strsplit(distinct,"")[[1]]}
      score <- input$score
      quality <- input$quality
      aindex <- input$ai
      minsit <- 1
      if(!is.null(input$sitmin)){minsit <- as.numeric(input$sitmin)}
      print(paste0("Min sit setting = ",minsit))
      filetype <- input$filetype

      #########################################################
      # Stage base datasets

      key <- read.csv("../4 catalog/catalog key.csv",stringsAsFactors=FALSE)
      head(key)

      suppressWarnings(events <- compile.events()) ; head(events) ; nrow(events)
      events$id <- as.character(events$id)
      ids <- unique(events$id) ; ids

      #suppressWarnings(ilv <- compile.ILV(events)) ;  head(ilv)
      #ilv$id <- as.character(ilv$id)

      #########################################################
      # Assign scores to all photos

      print("Gather quality scores of photos: initializing . . . ")

      scores <- gather.quality.scores()
      events$score <- events$distinct <- NA
      i=600
      for(i in 1:nrow(events)){
        evi <- events[i,] ; evi
        epath <- paste0(evi$path,evi$file) ; epath
        scori <- find.quality.score(scores,epath=epath)
        events$score[i] <- scori$score
        events$distinct[i] <- scori$distinct
      }
      events$score
      events$distinct

      print(paste0("Number of photo-identifications with quality scores = ",nrow(events[!is.na(events$score),])))
      print("Gathering quality scores: complete!")
      print(".") ; print(".") ; print(".")

      #########################################################
      # Apply filters

      print("Apply settings filters: initializing . . . ")
      print(paste0("Initial number of photo-identifications = ",nrow(events)))

      #historical <- "id"
      #distinct <- c("1","2")
      #demog <- "no calves"
      #minsit <- 3

      # Only include whales in historical catalog
      keeps <- 1:length(ids)
      if(historical=="id"){
        keeps <- which(ids %in% as.character(key$id)) ; keeps
        if(length(keeps)>0){ids <- ids[keeps]}
      }

      # Include calves?
      keeps <- 1:length(ids) ; length(ids)
      if(demog=="no calves"){
        keeps <- which(as.character(key$calf)  == "0" & key$id %in% ids) ; length(keeps)
        if(length(keeps)>0){ids <- ids[keeps]}
      }
      length(ids)

      # Distinctiveness
      keeps <- 1:length(ids) ; length(ids)
      if(distinct != "all"){
        keeps <- which(as.character(key$distinctiveness) %in% distinct & key$id %in% ids) ; length(keeps)
        if(length(keeps)>0){ids <- ids[keeps]}
      }
      length(ids)

      # Subset events to these IDs
      nrow(events) ; events <- events[events$id %in% ids,] ; nrow(events)

      # Include only photos that have been scored?
      if(score!="all"){events <- events[!is.na(events$score),]}

      # Photo quality
      if(quality=="1"){events <- events[events$score == 1,]}
      if(quality=="2"){events <- events[events$score == 2,]}
      if(quality=="3"){events <- events[events$score == 3,]}
      if(quality=="3if12"){
        keeps <- which(events$score %in% c(1,2)) ; length(keeps)
        threes <- which(events$score==3) ; threes
        keep3 <- threes[which(events$distinct[threes]%in%c(1,2))] ; length(keep3)
        keeps <- sort(c(keeps,keep3)) ; length(keeps)
        events <- events[keeps,]
      }
      if(quality=="3if1"){
        keeps <- which(events$score %in% c(1,2)) ; length(keeps)
        threes <- which(events$score==3) ; threes
        keep3 <- threes[which(events$distinct[threes]%in%c(1))] ; length(keep3)
        keeps <- sort(c(keeps,keep3)) ; length(keeps)
        events <- events[keeps,]
      }

      # Sighting threshold
      uid <- paste0(events$groupid,"-",events$id)
      uid <- unique(uid)
      uid <- strsplit(uid,"-")
      uid <- sapply(uid, "[[", 2)
      keeps <- which(sort(table(uid))>=minsit) ; keeps
      idkeep <- names(keeps) ; idkeep
      nrow(events) ; events <- events[events$id %in% idkeep,] ; nrow(events)

      print(paste0("Filtered number of photo-identifications = ",nrow(events)))
      print("Apply settings filters: complete!")
      print(".") ; print(".") ; print(".")

      if(nrow(events)>0){

        #########################################################
        # Events

        if("events" %in% input$sets){
          print("Compile Events: initializing . . . ")
          print(paste0("Number of photo-identifications = ",nrow(events)))

          fn <- "../6 analysis/datasets/events" ; fn
          if(filetype=="rds"){saveRDS(events,file=paste0(fn,".RDS"))}
          if(filetype=="csv"){write.csv(events,file=paste0(fn,".csv"),quote=FALSE,row.names=FALSE)}

          print("Compile Events: complete!")
          print(".") ; print(".") ; print(".")
        }

        #########################################################
        # Groups

        if("groups" %in% input$sets){
          print("Compile Groups: initializing . . . ")
          suppressWarnings(grp <- compile.groups(events))
          print(paste0("Number of encounters = ",nrow(grp)))

          fn <- "../6 analysis/datasets/groups" ; fn
          if(filetype=="rds"){saveRDS(grp,file=paste0(fn,".RDS"))}
          if(filetype=="csv"){write.csv(grp,file=paste0(fn,".csv"),quote=FALSE,row.names=FALSE)}

          print("Compile Groups: complete!")
          print(".") ; print(".") ; print(".")
        }

        #########################################################
        # ILV

        if("ilv" %in% input$sets){
          print("Individual-Level Variables: initializing . . . ")
          suppressWarnings(ilv <- compile.ILV(events))
          print(paste0("Number of unique individuals = ",nrow(ilv)))

          fn <- "../6 analysis/datasets/ILV" ; fn
          if(filetype=="rds"){saveRDS(ilv,file=paste0(fn,".RDS"))}
          if(filetype=="csv"){write.csv(ilv,file=paste0(fn,".csv"),quote=FALSE,row.names=FALSE)}

          print("Individual-Level Variables: complete!")
          print(".") ; print(".") ; print(".")
        }

        #########################################################
        # Dyads

        if("dyads" %in% input$sets){
          print("Compile Dyads list: initializing . . . ")
          suppressWarnings(ilv <- compile.ILV(events))
          suppressWarnings(dyads <- compile.dyads(ilv=ilv,mr=events))
          print(paste0("Number of dyads possible = ",nrow(dyads)))
          print(paste0("Number of dyads realized = ",nrow(dyads[dyads$X>0,])))

          fn <- "../6 analysis/datasets/dyads" ; fn
          if(filetype=="rds"){saveRDS(dyads,file=paste0(fn,".RDS"))}
          if(filetype=="csv"){write.csv(dyads,file=paste0(fn,".csv"),quote=FALSE,row.names=FALSE)}

          print("Compile Dyads list: complete!")
          print(".") ; print(".") ; print(".")
        }

        #########################################################
        # Association matrix

        if("mx" %in% input$sets){
          print("Produce Association Matrix: initializing . . . ")
          suppressWarnings(ilv <- compile.ILV(events))
          suppressWarnings(dyads <- compile.dyads(ilv=ilv,mr=events))
          suppressWarnings(am <- association.matrix(dyads,var=aindex))

          if(input$zero){diag(am$mx) <- 0}

          fn <- "../6 analysis/datasets/association-matrix" ; fn
          if(filetype=="rds"){saveRDS(am,file=paste0(fn,".RDS"))}
          if(filetype=="csv"){write.csv(am$mx,file=paste0(fn,".csv"),quote=FALSE,row.names=TRUE)}

          print("Produce Association Matrix: complete!")
          print(".") ; print(".") ; print(".")
        }

        #########################################################
        # Capture histories

        if("ch" %in% input$sets){
          print("Produce Capture Histories: initializing . . . ")
          suppressWarnings(ilv <- compile.ILV(events))
          suppressWarnings(groups <- compile.groups(events))
          suppressWarnings(ch <- caphist(ILV=ilv,groups=groups,samp.period="year"))

          fn <- "../6 analysis/datasets/capture-history" ; fn
          if(filetype=="rds"){saveRDS(ch,file=paste0(fn,".RDS"))}
          if(filetype=="csv"){write.csv(ch,file=paste0(fn,".csv"),quote=FALSE,row.names=FALSE)}

          print("Produce Capture Histories: complete!")
          print(".") ; print(".") ; print(".")
        }
        #########################################################
        # Update data tables

        print("Updating datatables in app: initializing . . . ")

        datadir <- "../6 analysis/datasets/" ; datadir
        lf <- list.files(datadir) ; lf
        lf <- paste0(datadir,lf) ; lf
        lf <- lf[grep("csv",lf)] ; lf

        lfmatch <- grep("events",lf)
        if(length(lfmatch)>0){rv$events <- read.csv(lf[lfmatch],stringsAsFactors=FALSE)}

        lfmatch <- grep("groups",lf)
        if(length(lfmatch)>0){rv$groups <- read.csv(lf[lfmatch],stringsAsFactors=FALSE)}

        lfmatch <- grep("ILV",lf)
        if(length(lfmatch)>0){rv$ilv <- read.csv(lf[lfmatch],stringsAsFactors=FALSE)}

        lfmatch <- grep("dyads",lf)
        if(length(lfmatch)>0){rv$dyads <- read.csv(lf[lfmatch],stringsAsFactors=FALSE)}

        lfmatch <- grep("matrix",lf)
        if(length(lfmatch)>0){rv$mx <- read.csv(lf[lfmatch],stringsAsFactors=FALSE)}

        print("Updating datatables in app: Complete!")
        print(".") ; print(".") ; print(".")
        print("Process complete! Check out files in 6 analysis > datasets. Ready for next task!")

      } # end of if nrow(events)>0

    })
  }


  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Analysis Dashboard",
                           #########################################################
                           tabPanel("Compile Datasets for Analysis",
                                    br(),
                                    fluidRow(column(12,
                                                    checkboxGroupInput("sets", label=h4("Select data products to compile:"),
                                                                       choices = list("Events (each row is a unique ID in a unique event)"="events",
                                                                                      "Groups (each row is a unique event)"="groups",
                                                                                      "Individual-Level Variables (each row is a unique whale)"="ilv",
                                                                                      "List of dyads & association weights (each row is a dyad pair)"="dyads",
                                                                                      "Association matrix for igraph (an NxN matrix with association weights)"="mx",
                                                                                      "Capture histories (for RMark)"="ch"
                                                                       ),
                                                                       selected = NULL,width = "90%"))),
                                    br(),hr(),br(),
                                    fluidRow(column(12,h4("Settings based upon historical catalog"))),
                                    br(),
                                    fluidRow(column(12,radioButtons("historical","Only use whales tracked in the historical catalog?",
                                                                    choices=list("No -- Use all photos (this could include whales that are unique within a season (UIS) but not tracked across years"="all",
                                                                                 "Only use individuals who have been assigned a historical ID"="id"
                                                                    ),selected="all",width="95%"))),
                                    br(),
                                    fluidRow(column(12,radioButtons("demo","Only use adult whales?",
                                                                    choices=list("No -- Use all whales"="all",
                                                                                 "Do not use whales that were originally seen as calves or first-years"="no calves"),
                                                                    selected="all",width="95%"))),
                                    br(),
                                    fluidRow(column(12,radioButtons("distinct","Only use distinct whales?",
                                                                    choices=list("No -- Use all whales"="all",
                                                                                 "Only use whales with a distinctiveness score of 3"="3",
                                                                                 "Only use whales with a distinctiveness score of 2"="2",
                                                                                 "Only use whales with a distinctiveness score of 2 or 3"="23",
                                                                                 "Only use whales with a distinctiveness score of 1 or 2"="12",
                                                                                 "Only use whales with a distinctiveness score of 1"="1"
                                                                    ),selected="all",width="95%"))),
                                    br(),hr(),br(),
                                    fluidRow(column(12,h4("Settings based upon sighting event photos"))),
                                    br(),
                                    fluidRow(column(12,radioButtons("score","Only use sighting photos that have been scored for quality?",
                                                                    choices=list("No -- Use all photos"="all",
                                                                                 "Yes -- do not include any photo that has not been scored"="scored"),
                                                                    selected="all",width="95%"))),
                                    br(),
                                    fluidRow(column(12,radioButtons("quality","Only use sighting photos of a certain quality?",
                                                                    choices=list("No -- Use all photos"="all",
                                                                                 "Use photos with a score of 3 ONLY IF that photo has a distinctiveness score of 1 or 2"="3if12",
                                                                                 "Use photos with a score of 3 ONLY IF that photo has a distinctiveness score of 1"="3if1",
                                                                                 "Only use photos with a score of 3 in one or more quality categories"="3only",
                                                                                 "Only use photos with a score of 1 or 2 in all quality categories"="12",
                                                                                 "Only use photos with a score of 2 in all quality categories"="2",
                                                                                 "Only use photos with a score of 1 in all quality categories"="1"
                                                                    ),selected="all",width="95%"))),
                                    br(),
                                    fluidRow(column(12,radioButtons("min","Set a minimum sighting threshold?",
                                                                    choices=list("No -- Use all whales"="all",
                                                                                 "Only use whales seen a minimum number of times"="min"),
                                                                    selected="all",width="95%"))),
                                    fluidRow(column(12,uiOutput("sitmin"))),
                                    br(),
                                    hr(),
                                    br(),
                                    fluidRow(column(12,h4("Settings for processing"))),
                                    br(),
                                    fluidRow(column(12,radioButtons("ai","Which association index do you want to use when creating the dyad list and association matrix?",
                                                                    choices=list("Simple Ratio Index (SRI)"="sri",
                                                                                 "Half-Weight Index (HWI)"="hwi"),
                                                                    selected="sri",width="95%"))),
                                    br(),
                                    fluidRow(column(12,checkboxInput("zero","Zero-out the diagonal in the association matrix?",value=TRUE,width="90%"))),
                                    br(),
                                    fluidRow(column(12,radioButtons("filetype","Which file format do you want to use?",
                                                                    choices=list(".csv (Comma-Separated Values)"="csv",
                                                                                 ".rds (R data object)"="rds"),
                                                                    selected="csv",width="95%"))),
                                    br(),hr(),br(),
                                    fluidRow(column(12,actionButton("begin",h4("Begin!")))),
                                    br(),br(),br(),br(),br(),br()),

                           #########################################################
                           tabPanel("review",
                                    tabsetPanel(
                                      tabPanel("Events", br(), fluidRow(column(12,DT::dataTableOutput("events")))),
                                      tabPanel("Groups", br(), fluidRow(column(12,DT::dataTableOutput("groups")))),
                                      tabPanel("Individual-Level Variables", br(), fluidRow(column(12,DT::dataTableOutput("ilv")))),
                                      tabPanel("Capture Histories", br(), fluidRow(column(12,DT::dataTableOutput("caphist")))),
                                      tabPanel("Dyads", br(), fluidRow(column(12,DT::dataTableOutput("dyads")))),
                                      tabPanel("Association matrix", br(), fluidRow(column(12,DT::dataTableOutput("mx"))))
                                    ))
  ))

  #########################################################
  #########################################################
  #########################################################

  shinyApp(ui = ui, server = server)

  #########################################################
  #########################################################

}
