#' Update events spreadsheets with matched IDs
#'
#' @param events Path to folder with events spreadsheets. The default follows the instructions for the `catRlog` system setup.
#' @param photo_collections Path to folder containing subfolders of photo collections that you wish to match against your reference catalog. The default follows the instructions for the `catRlog` system setup.
#' @param reviewed_matches Path for where to look for set of final reconciled/reviewed matching decisions. The default follows the instructions for the `catRlog` system setup.
#'
#' @return Shiny app. See the [vignette](https://ericmkeen.github.io/catRlog/) for a detailed user guide.
#' @export
#' @import shiny
#' @import DT
#' @import shinyjs
#' @import dplyr
#' @import tidyselect
#'
update_events <- function(events = 'events/',
                          photo_collections = 'photos/photos/',
                          reviewed_matches = 'matches/reviewed matches/'){

  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values

    rv <- reactiveValues()
    rv$matchlf <- NULL
    rv$eventlf <- NULL
    rv$matchdata <- data.frame()
    rv$eventdata <- data.frame()

    #########################################################
    # Select reviewed match file

    output$matchfile <- renderUI({
      workdir <- reviewed_matches ; workdir
      dirops <- list.files(workdir)
      dirops <- paste0(workdir,dirops)
      if(length(dirops)>0){
        selectInput("matchfile",label=h4("Select reviewed matches to assign:"),
                    choices=dirops,selected=1,multiple=FALSE,width="90%")
      }else{ "No match sessions found! Look in catRlog > matches > reviewed matches" }
    })

    observe({
      #md <- dirops[1]
      #md <- read.csv(md,stringsAsFactors=FALSE) ; head(md)
      input$matchfile
      events <- c()
      if(!is.null(input$matchfile) && nchar(input$matchfile)>0){
        md <- read.csv(input$matchfile,stringsAsFactors=FALSE)
        rv$matchdata <- md
        if(nrow(md)>0){
          i=1
          for(i in 1:nrow(md)){
            pathi <- md$path[i] ; pathi
            psplit <- strsplit(pathi,"")[[1]] ; psplit
            slashes <- grep("/",psplit)
            eventi <- substr(pathi,
                             (slashes[length(slashes)-1]+1),
                             (slashes[length(slashes)]-1)) ; eventi
            events <- c(events,eventi)
          }
          unique(events)
        }
      }
      rv$events <- unique(events)
      print(rv$events)
    })

    #########################################################
    # Update data table for review

    output$matchdata <- renderDataTable(rv$matchdata)

    #########################################################
    # Create drop down menu of photo collections to update

    output$eventfile <- renderUI({
      if(!is.null(rv$events) && length(rv$events)>0){
        workdir <- events ; workdir
        dirops <- list.files(workdir) ; dirops
        dirsub <- gsub(".csv","",dirops) ; dirsub
        matchi <- which(dirsub %in% rv$events) ; matchi
        if(length(matchi)>0){
          dirops <- dirops[matchi] ; dirops
          dirops <- paste0(workdir,dirops) ; dirops
          selectInput("eventfile",label=h4("Select event table to update with IDs:"),
                      choices=dirops,selected=1,multiple=FALSE,width="90%")
        }else{ "No match sessions found! Look in catRlog > matches > reviewed matches" }
      }else{"No match sessions found! Look in catRlog > matches > reviewed matches"}
    })

    #########################################################
    # Read-in events table that has been chosen

    observe({
      input$eventfile
      if(!is.null(input$eventfile) && nchar(input$eventfile)>3){
        ef <- read.csv(input$eventfile,stringsAsFactors=FALSE)
        rv$eventdata <- ef
      }
    })

    #########################################################
    # Create data table to review event data

    output$eventdata <- renderDataTable(rv$eventdata)

    #########################################################
    # Button to assign matches to this event file

    output$assign <- renderUI({
      if(!is.null(input$matchfile) && length(input$matchfile)>0){
        actionButton("assign",h4("Assign matches to these event file"),width="40%")
      }
    })

    observeEvent(input$assign,{
      #eventfile <- "../1 events/2015 Elemiah.csv"
      #edf <- read.csv(eventfile,stringsAsFactors=FALSE) ; head(edf)
      #mdf <- read.csv( "../3 matches/reviewed matches/Reviewed Matches 20200725 151644.csv",stringsAsFactors=FALSE) ; head(mdf)

      eventfile <- input$eventfile
      print(paste0("Beginning to assign IDs to event file ",eventfile," . . . "))
      mdf <- rv$matchdata
      edf <- rv$eventdata

      # First store backup
      ef <- basename(eventfile)
      ef <- gsub(".csv","",ef) ; ef
      backupname <- paste0(events,"backups/",ef,"-",
                           gsub("-","",gsub(":","",as.character(Sys.time()))),".csv") ; backupname
      write.csv(edf,file=backupname,quote=FALSE,row.names=FALSE)

      # Add id column if necessary
      idcol <- which(names(edf)=="id") ; idcol
      if(length(idcol)>0){
        old.ids <- edf$id
        edf$id <- NA
        edf$placehold <- old.ids
        names(edf)[names(edf)=="placehold"] <- paste0("id.",
                                                      gsub(" ","",gsub("-","",gsub(":","",as.character(Sys.time())))))
      }else{
        edf$id <- NA
      }
      idcol <- which(names(edf)=="id") ; idcol
      head(edf)

      # Subset matchfile to this event file
      photopath <- gsub(events,photo_collections,eventfile) ; photopath
      photopath <- gsub(".csv","/",photopath) ; photopath

      matches <- grep(photopath,mdf$path) ; matches
      if(length(matches)>0){
        # Now go through each match
        me <- mdf[matches,]
        head(me)
        i=1
        for(i in 1:nrow(me)){

          mi <- me[i,] ; mi
          exts <- c(".JPG",".JPEG",".jpg",".JPEG",".png",".PNG") ; exts

          # Simplify ID
          idi <- mi$id ; idi
          if(!is.na(idi)){
            for(j in 1:length(exts)){idi <- gsub(exts[j],"",idi)}
            idi
            idi <- substr(idi,1,(nchar(idi)-1)) ; idi
          }

          # Simplify image name
          pici <- mi$path ; pici
          pici <- gsub(photopath,"",pici)
          for(j in 1:length(exts)){pici <- gsub(exts[j],"",pici)}
          pici

          # Find image name in event file
          head(edf)
          matchi <- grep(pici,edf$file) ; matchi
          if(length(matchi)>0){
            edf$id[matchi] <- idi
          }
        }
        edf$id
        write.csv(edf,file=eventfile,quote=FALSE,row.names=FALSE)
      }else{
        "No matches in this file correspond to this event file! Backup file made, but no other actions taken."
      }
      print("Done!")
    })

  }


  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Assign Matches to Event Data",
                           #########################################################
                           tabPanel("ASSIGN",
                                    fluidRow(column(12,
                                                    br(),uiOutput("matchfile"),
                                                    br())),
                                    fluidRow(column(12,uiOutput("eventfile"))),
                                    br(),
                                    fluidRow(column(12,uiOutput("assign"))),
                                    br(),
                                    br()),

                           #########################################################
                           tabPanel("review",
                                    tabsetPanel(
                                      tabPanel("Reviewed matches to assign", br(), fluidRow(column(12,dataTableOutput("matchdata")))),
                                      tabPanel("Event file to be updated", br(), fluidRow(column(12,dataTableOutput("eventdata"))))
                                    ))
  ))

  #########################################################
  #########################################################
  #########################################################

  shinyApp(ui = ui, server = server)

  #########################################################
  #########################################################

}
