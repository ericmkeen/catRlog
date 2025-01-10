#' Review matches from multiple analysts & reconcile discrepancies
#'
#' @param catalog_photos Path to folder with catalog ID photos. The default follows the instructions for the `catRlog` system setup.
#' @param match_sessions Path to folder where matching sessions will be saved. The default follows the instructions for the `catRlog` system setup.
#' @param reviewed_matches Path to folder where final reconciled/reviewed matching decisions should be saved. The default follows the instructions for the `catRlog` system setup.

#' @return Shiny app. See the [vignette](https://ericmkeen.github.io/catRlog/) for a detailed user guide.
#' @export
#' @import shiny
#' @import DT
#' @import shinyjs
#' @import dplyr
#' @import tidyselect
#'
review_matches <- function(catalog_photos = 'catalog/catalog/',
                           match_sessions = "matches/match sessions/",
                           reviewed_matches = 'matches/reviewed matches/'
                           ){

  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values
    rv <- reactiveValues()
    rv$refdir <- catalog_photos
    rv$reflfull <- NULL
    rv$dfilter <- data.frame()
    rv$reflf <- NULL
    rv$refi <- 1
    rv$worklfull <- NULL
    rv$worklf <- NULL
    rv$worki <- 1
    rv$matchdata <- data.frame()
    rv$matches <- data.frame()
    rv$guesses <- data.frame()
    rv$cants <- data.frame()
    rv$news <- data.frame()
    rv$discrepancies <- data.frame()

    rv$discwork <- data.frame()
    rv$discids <- c()
    rv$disci <- 1


    #########################################################
    # Get list of match sessions

    output$matchfiles <- renderUI({
      workdir <- match_sessions ; workdir
      dirops <- list.files(workdir)
      dirops <- paste0(workdir,dirops)
      if(length(dirops)>0){
        selectInput("matchfiles",label=h4("Select match sessions to add to review docket:"),
                    choices=dirops,selected=1,multiple=TRUE,width="90%")
      }else{ "No match sessions found! Look in catRlog > 3 matches > match sessions" }
    })

    #########################################################
    # Import match sessions

    output$import <- renderUI({
      if(!is.null(input$matchfiles) && length(input$matchfiles)>0){
        actionButton("import",h4("Import match sessions"),width="40%")
      }
    })

    observeEvent(input$import,{
      #lf <- list.files(workdir) ; lf
      #lf <- paste0(workdir,lf) ; lf
      lf <- input$matchfiles
      ms <- data.frame()
      for(i in 1:length(lf)){
        msi <- read.csv(lf[i],stringsAsFactors=FALSE,header=FALSE)
        ms <- rbind(ms,msi)
      }
      print(ms)
      names(ms) <- c("match.date","analyst","collection","decision","photo","ID","photo.path","id.path")
      ms
      rv$matchdata <- ms
    })

    #########################################################
    # Review Tab

    output$matchdata <- renderDataTable(rv$matchdata)
    output$matches <- renderDataTable(rv$matches)
    output$news <- renderDataTable(rv$news)
    output$guesses <- renderDataTable(rv$guesses)
    output$cants <- renderDataTable(rv$cants)
    output$discrepancies <- renderDataTable(rv$discrepancies)
    output$discwork <- renderDataTable(rv$discwork)

    #########################################################
    # Parse matches

    output$sort <- renderUI({
      if(!is.null(rv$matchdata) && nrow(rv$matchdata)>0){
        actionButton("sort",h4("Parse matches"),width="40%")
      }
    })

    observeEvent(input$sort,{
      discrepancies <- data.frame()
      matches <- data.frame()
      guesses <- data.frame()
      cants <- data.frame()
      news <- data.frame()

      ms <- rv$matchdata
      head(ms)
      paths <- unique(ms$photo.path) ; paths
      i=1
      for(i in 1:length(paths)){
        pathi <- paths[i] ; pathi
        msi <- ms[ms$photo.path==pathi,] ; msi
        matchi <- msi$ID ; matchi
        decisions <- msi$decision ; decisions
        udecide <- unique(decisions)
        umatch <- unique(matchi) ; umatch
        dfi <- data.frame(path=pathi,
                          n=nrow(msi),
                          analysts=paste(msi$analyst,collapse="-"),
                          decision=paste(msi$decision,collapse="-"),
                          id=paste(msi$ID,collapse="-"))
        dfi
        if(length(umatch)>1 | length(udecide)>1){
          discrepancies <- rbind(discrepancies,dfi)
        }else{
          if(!is.na(umatch)){
            if(udecide=="MATCH"){matches <- rbind(matches,dfi)}
            if(udecide=="GUESS"){guesses <- rbind(guesses,dfi)}
          }else{
            if(udecide=="CANT"){cants <- rbind(cants,dfi)}
            if(udecide=="NEW"){news <- rbind(news,dfi)}
          }
        }
      }
      rv$matches <- matches
      rv$cants <- cants
      rv$guesses <- guesses
      rv$news <- news
      rv$discrepancies <- discrepancies

      print(rv$news)
    })

    #########################################################
    # Status reports

    output$status1 <- renderUI({
      HTML(paste("Decisions = ",nrow(rv$matchdata),"<br/>",
                 "Analysts = ",paste(unique(rv$matchdata$analyst),collapse=", "),"<br/>",
                 "Photos processed = ",length(unique(rv$matchdata$photo.path)),"<br/>"
      ))
    })

    output$status2 <- renderUI({
      HTML(paste("Unanimous matches = ",nrow(rv$matches),"<br/>",
                 "Unanimous new whales = ",nrow(rv$news),"<br/>",
                 "Unanimous guesses = ",nrow(rv$guesses),"<br/>",
                 "Unanimous give-ups = ",nrow(rv$cants),"<br/>",
                 "Discrepancies = ",nrow(rv$discrepancies),"<br/>"
      ))
    })

    output$discrepalert <- renderUI({
      if(nrow(rv$discrepancies)>0){
        "There appear to be discrepancies with the decisions for at least one photo. Use the DISCREPANCIES tab to reconcile."
      }else{
        if(nrow(rv$matchdata)>0 && nrow(rv$matches)>0)
          actionButton("store",h4("Consolidate & store reviewed matches"))
      }
    })


    #########################################################
    #########################################################
    #########################################################
    #########################################################
    # DISCREPANCIES

    #########################################################
    #########################################################
    # DISPUTED PHOTOS

    #########################################################
    # Stage photos under dispute

    observe({
      if(nrow(rv$discrepancies)>0){
        lf <- rv$discrepancies$path #; print(lf)
        rv$worklf <- lf
      }
    })

    #########################################################
    # Photo under dispute

    output$workid <- renderText({ if(nrow(rv$discrepancies)>0){as.character(rv$worklf)[rv$worki]} })
    output$workstatus <- renderText({ if(nrow(rv$discrepancies)>0){paste0("Image ",rv$worki," out of ",length(rv$worklf))} })
    output$workpic <- renderImage({filename <- as.character(rv$worklf)[rv$worki]
    list(src = filename,width="100%")}, deleteFile = FALSE)

    #########################################################
    # Navigation

    output$worknext <- renderUI({
      if(length(rv$worklf)>0){ actionButton("worknext",h4("Next")) }
    })

    output$workback <- renderUI({
      if(length(rv$worklf)>0){ actionButton("workback",h4("Back")) }
    })

    observeEvent(input$worknext, {
      newi <- rv$worki + 1
      if(newi > length(rv$worklf)){newi <- 1}
      rv$worki <- newi
    })

    observeEvent(input$workback, {
      newi <- rv$worki - 1
      if(newi < 1){newi <- length(rv$worklf)}
      rv$worki <- newi
    })

    #########################################################
    #########################################################
    # DECISION OPTIONS

    observe({
      if(nrow(rv$discrepancies)>0){
        discpath <- rv$worklf[rv$worki]
        discwork <- rv$discrepancies[rv$discrepancies$path == discpath,] ; print(discwork)
        rv$discwork <- discwork
        ids <- strsplit(as.character(discwork$id),"-")[[1]]
        ids <- ids[!is.na(ids)]; print(ids)
        rv$discids <- ids
      }
    })

    #########################################################
    # Candidate IDs under dispute

    output$discid <- renderText({ if(length(rv$discids)>0){as.character(rv$discids)[rv$disci]} })
    output$discstatus <- renderText({ if(length(rv$discids)>0){paste0("Option ",rv$disci," out of ",length(rv$discids))} })
    output$discpic <- renderImage({filename <- paste0("../4 catalog/catalog/",as.character(rv$discids)[rv$disci])
    list(src = filename,width="100%")}, deleteFile = FALSE)

    #########################################################
    # Navigate decision options

    output$discnext <- renderUI({
      if(length(rv$discids)>0){ actionButton("discnext",h4("Next")) }
    })

    output$discback <- renderUI({
      if(length(rv$discids)>0){ actionButton("discback",h4("Back")) }
    })

    observeEvent(input$discnext, {
      newi <- rv$disci + 1
      if(newi > length(rv$discids)){newi <- 1}
      rv$disci <- newi
    })

    observeEvent(input$discback, {
      newi <- rv$disci - 1
      if(newi < 1){newi <- length(rv$discids)}
      rv$disci <- newi
    })

    #########################################################
    #########################################################
    # Final call

    output$final <- renderUI({if(nrow(rv$discwork)>0){
      radioButtons("final",label=h3("Final decision:"),
                   choices=list("No decision"="NA",
                                "Successful match"="match",
                                "No match - new whale!"="new",
                                "Cannot ID"="cant"),
                   selected="NA",inline=TRUE,width="95%")
    }
    })

    output$matchops <- renderUI({
      if(input$final=="match"){
        choices <- c("NA",rv$discids)
        radioButtons("matchops",label="Match options:",
                     choices=choices,
                     selected="NA",inline=TRUE,width="95%")
      }
    })

    output$discsave <- renderUI({
      if(input$final != "NA"){
        if(input$final=="match"){
          if(input$matchops != "NA"){
            actionButton("discsave",h4("Save final decision"))
          }
        }else{
          actionButton("discsave",h4("Save final decision"))
        }
      }
    })

    observeEvent(input$discsave,{
      worki <- rv$worki
      discrow <- rv$discwork

      if(input$final=="cant"){
        discrow$decision <- "CANT"
        discrow$id <- NA
        rv$cants <- rbind(rv$cants,discrow)
      }

      if(input$final=="new"){
        discrow$decision <- "NEW"
        discrow$id <- NA
        rv$news <- rbind(rv$news,discrow)
      }

      if(input$final=="match"){
        discrow$decision <- "MATCH"
        discrow$id <- input$matchops
        rv$matches <- rbind(rv$matches,discrow)
      }

      rv$discrepancies <- rv$discrepancies[-worki,]

      print(discrow)
    })

    #########################################################
    #########################################################
    #########################################################
    # Store reviewed match session

    observeEvent(input$store,{
      mr <- data.frame()
      if(nrow(rv$matches)>0){mr <- rbind(mr,data.frame(rv$matches,action="MATCH"))}
      if(nrow(rv$news)>0){mr <- rbind(mr,data.frame(rv$news,action="NEW"))}
      if(nrow(rv$guesses)>0){mr <- rbind(mr,data.frame(rv$guesses,action="GUESS"))}
      if(nrow(rv$cants)>0){mr <- rbind(mr,data.frame(rv$cants,action="CANT"))}
      if(nrow(rv$discrepancies)>0){mr <- rbind(mr,rv$discrepancies)}

      fn <- paste0(reviewed_matches, "Reviewed Matches ",gsub(":","",gsub("-","",as.character(Sys.time()))),".csv") ; fn
      write.csv(mr,file=fn,quote=FALSE,row.names=FALSE)
      print("Reviewed matches stored!")
      print(fn)
    })

  }


  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Match Review",
                           #########################################################
                           tabPanel("set up",
                                    fluidRow(column(12,
                                                    br(),uiOutput("matchfiles"),
                                                    br())),
                                    fluidRow(column(12,uiOutput("import"))),
                                    br(),
                                    fluidRow(column(12,htmlOutput("status1"))),
                                    br(),
                                    fluidRow(column(12,uiOutput("sort"))),
                                    br(),
                                    fluidRow(column(12,htmlOutput("status2"))),
                                    br(),
                                    fluidRow(column(12,uiOutput("discrepalert"))),

                                    br()),

                           #########################################################
                           tabPanel("DISCREPANCY!",
                                    br(),
                                    fluidRow(column(4,h4("Disputed match:")),
                                             column(8,textOutput("workid"))),
                                    fluidRow(column(10,imageOutput("workpic")),
                                             column(2,
                                                    uiOutput("workback",inline=FALSE),br(),
                                                    uiOutput("worknext",inline=FALSE),br(),
                                                    textOutput("workstatus",inline=FALSE))),
                                    hr(),
                                    fluidRow(column(4,h4("Decisions on record:")),
                                             column(8,textOutput("discid"))),
                                    fluidRow(column(10,imageOutput("discpic")),
                                             column(2,
                                                    uiOutput("discback",inline=FALSE),br(),
                                                    uiOutput("discnext",inline=FALSE),br(),
                                                    textOutput("discstatus",inline=FALSE))),
                                    fluidRow(column(12,dataTableOutput("discwork"))),
                                    hr(),
                                    fluidRow(column(12,uiOutput("final")),
                                             column(12,uiOutput("matchops"))),
                                    br(),
                                    fluidRow(column(12,uiOutput("discsave"))),
                                    br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                    br(),br(),br(),br(),br(),br(),br(),br(),br()
                           ),
                           #########################################################
                           tabPanel("review",
                                    tabsetPanel(
                                      tabPanel("All", br(), fluidRow(column(12,dataTableOutput("matchdata")))),
                                      tabPanel("Matched", br(), fluidRow(column(12,dataTableOutput("matches")))),
                                      tabPanel("New", br(), fluidRow(column(12,dataTableOutput("news")))),
                                      tabPanel("Guessed", br(), fluidRow(column(12,dataTableOutput("guesses")))),
                                      tabPanel("Can't", br(), fluidRow(column(12,dataTableOutput("cants")))),
                                      tabPanel("Discrepancies", br(), fluidRow(column(12,dataTableOutput("discrepancies"))))
                                    ))
  ))

  #########################################################
  #########################################################
  #########################################################

  shinyApp(ui = ui, server = server)

  #########################################################
  #########################################################

}
