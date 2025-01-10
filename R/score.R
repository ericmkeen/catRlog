#' Assign quality scores to ID photos
#'
#' @param collections_to_score Path to folder containing subfolders of photo collections that you wish to score. The default follows the instructions for the `catRlog` system setup.
#' @param score_sessions_path Path to folder where your scores will be saved. The default follows the instructions for the `catRlog` system setup.
#' @return Shiny app. See the [vignette](https://ericmkeen.github.io/catRlog/) for a detailed user guide.
#' @export
#' @import shiny
#' @import DT
#' @import shinyjs
#' @import dplyr
#' @import tidyselect
#'
score <- function(collections_to_score = 'photos/photos/',
                  score_sessions_path = 'scores/score sessions/'){

  #########################################################
  #########################################################

  server <- function(session, input, output) {

    #########################################################
    #########################################################
    # Setup reactive values
    rv <- reactiveValues()
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
    #########################################################
    # Functions

    subset.data <- function(matches,worklfull){
      #matches <- rv$matchdata
      #matches <- matchdata
      dones <- matches[,5] ; dones
      dones <- which(! worklfull %in% dones) ; dones
      #dones <- which(! rv$worklfull %in% dones) ; dones
      worklf <- worklfull[dones]
      return(worklf)
    }

    #########################################################
    #########################################################
    # Stage filepaths and lists of files

    # Scoring session record
    observe({
      if(!is.null(input$session)){
        if(input$session == "Start new session"){
          matchdir <- score_sessions_path
          #matchfile <- paste0(matchdir,list.files(matchdir)[6]) ; matchfile
          #matchdata <- read.csv(matchfile,stringsAsFactors=FALSE,header=FALSE)
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
        #worklfull <- paste0("../0 photos/photos/2015 Elemiah/",list.files("../0 photos/photos/2015 Elemiah")) ; worklfull
        lf <- paste0(input$workdir,lf)
        rv$worklfull <- lf
        rv$worklf <- lf
      }
    })

    #########################################################
    #########################################################
    # IMAGE

    # Photo to match
    output$workid <- renderText({ if(!is.null(input$workdir)){gsub(input$workdir,"",rv$worklf[rv$worki])} })
    output$workstatus <- renderText({ if(!is.null(input$workdir)){paste0("Image ",rv$worki," out of ",length(rv$worklf))} })
    output$workpic <- renderImage({filename <- rv$worklf[rv$worki]
    list(src = filename,width="100%")}, deleteFile = FALSE)

    #########################################################
    #########################################################
    # Data table

    output$matchdata <- renderDataTable(rv$matchdata)

    #########################################################
    #########################################################
    # SETUP

    # Get filename for scoring session
    output$session <- renderUI({
      if(input$analyst != ""){
        matchdir <- score_sessions_path
        lf <- list.files(matchdir) ; lf
        lf <- paste0(matchdir,lf)
        sessops <- c("Start new session",lf)
        selectInput("session","Start new matching session, or add to existing session?",selected=sessops[1],choices=sessops,width="50%")
      }
    })

    # Setup dropdown menu of photo collection options
    output$workdir <- renderUI({
      workdir <- collections_to_score; workdir
      dirops <- list.files(workdir)
      dirops <- paste0(workdir,dirops,"/")
      if(length(dirops)>0){
        selectInput("workdir",label=h4("Select photo collection to work on:"),choices=dirops,selected=1)
      }else{ "No photo collections found! Look in catRlog > photos > photos" }
    })

    # Filter box to subset to unscored photos
    observe({
      input$notdone
      if(input$notdone){
        if(nrow(rv$matchdata)>0){
          rv$worklf <- subset.data(matches=rv$matchdata,worklfull=rv$worklfull)
        }}
    })

    #########################################################
    #########################################################
    # NAVIGATION

    # Setup button to step forward
    output$worknext <- renderUI({
      if(length(rv$worklf)>0){ actionButton("worknext",h4("Next")) }
    })

    # Setup button to step backward
    output$workback <- renderUI({
      if(length(rv$worklf)>0){ actionButton("workback",h4("Back")) }
    })

    # What to do when Step Forward button is clicked
    observeEvent(input$worknext, {
      newi <- rv$worki + 1
      if(newi > length(rv$worklf)){
        showModal(modalDialog(title="You have reached the end of the scoring set!",
                              "Bringing you back to the first file...",
                              size="m",easyClose=TRUE))
        newi <- 1
      }
      rv$worki <- newi
      isolate({updateSelectInput(session, "angle",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "focus",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "exposure",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "visible",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "distinct",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "calf",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "parasite",label=NULL,choices=NULL,selected="N/A")})
    })

    # What to do when Step Backward button is clicked
    observeEvent(input$workback, {
      newi <- rv$worki - 1
      if(newi < 1){newi <- length(rv$worklf)}
      rv$worki <- newi
      isolate({updateSelectInput(session, "angle",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "focus",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "exposure",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "visible",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "distinct",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "calf",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "parasite",label=NULL,choices=NULL,selected="N/A")})
    })

    #########################################################
    #########################################################
    # SCORES

    output$angle <- renderUI({if(!is.null(input$session) && nchar(input$session)>2){
      selectInput("angle","Angle to whale",choices=c(list("N/A"="N/A","60 - 90 deg: Perfect" =1,"30 - 60 deg"=2,"<30: Poor"=3)),selected="N/A",size=4,selectize=FALSE)
    }})

    output$exposure <- renderUI({if(!is.null(input$session) && nchar(input$session)>2){
      selectInput("exposure","Image exposure",choices=c(list("N/A"="N/A","Well-lit, good contrast" =1,"Slight issue, some marks obscure"=2,"Poor"=3)),selected="N/A",size=4,selectize=FALSE)
    }})

    output$focus <- renderUI({if(!is.null(input$session) && nchar(input$session)>2){
      selectInput("focus","Image focus",choices=c(list("N/A"="N/A","Crisp, good detail" =1,"Bit blurred, some marks obscure"=2,"Poor"=3)),selected="N/A",size=4,selectize=FALSE)
    }})

    output$visible <- renderUI({if(!is.null(input$session) && nchar(input$session)>2){
      selectInput("visible","Proportion visible",choices=c(list("N/A"="N/A","High arch, perfect" =1,"Low marks obscured"=2,"Dorsal fin only"=3)),selected="N/A",size=4,selectize=FALSE)
    }})

    output$distinct <- renderUI({if(!is.null(input$session) && nchar(input$session)>2){
      selectInput("distinct","Feature distinctiveness",choices=c(list("N/A"="N/A","Very distinct, even with poor photo" =1,"Moderate"=2,"Photo must be perfect"=3)),selected="N/A",size=4,selectize=FALSE)
    }})

    output$calf <- renderUI({if(!is.null(input$session) && nchar(input$session)>2){
      selectInput("calf","Calf / juvenile",choices=c(list("N/A"="N/A","Well-marked adult" =1,"Subadult, few marks"=2,"Apparent calf"=3)),selected="N/A",size=4,selectize=FALSE)
    }})

    output$parasites <- renderUI({if(!is.null(input$session) && nchar(input$session)>2){
      selectInput("parasites","External parasites",choices=c(list("N/A"="N/A","None" =1,"Few"=2,"Many"=3)),selected="N/A",size=4,selectize=FALSE)
    }})

    #########################################################
    #########################################################
    # SAVE

    # Setup Save button to appear after scores are entered
    output$save <- renderUI({
      decisions <- c("","","","","","","")
      if(!is.null(input$angle)){decisions[1] <- input$angle}else{decisions[1] <- ""}
      if(!is.null(input$focus)){decisions[2] <- input$focus}else{decisions[2] <- ""}
      if(!is.null(input$exposure)){decisions[3] <- input$exposure}else{decisions[3] <- ""}
      if(!is.null(input$visible)){decisions[4] <- input$visible}else{decisions[4] <- ""}
      if(!is.null(input$calf)){decisions[5] <- input$calf}else{decisions[5] <- ""}
      if(!is.null(input$parasites)){decisions[6] <- input$parasites}else{decisions[6] <- ""}
      if(!is.null(input$distinct)){decisions[7] <- input$distinct}else{decisions[7] <- ""}
      #print(decisions)
      empties <- which(decisions %in% c("N/A"))
      if(length(empties)==0  && input$session!=""){
        actionButton("save",label=h4("Save & Next"),width="95%")
      }
    })

    # What to do when Save button is clicked
    observeEvent(input$save,{
      new.line <- paste0(as.character(Sys.time()),",",input$analyst,",",
                         gsub(collections_to_score,"",input$workdir),",",
                         gsub(input$workdir,"",rv$worklf[rv$worki]),",",
                         rv$worklf[rv$worki],",",
                         input$angle,",",
                         input$focus,",",
                         input$exposure,",",
                         input$visible,",",
                         input$distinct,",",
                         input$calf,",",
                         input$parasites,"\n") ; new.line
      cat(new.line,file=rv$matchfile,append=TRUE)

      if(file.exists(rv$matchfile)){rv$matchdata <- read.csv(rv$matchfile,header=FALSE)}

      if(input$notdone){
        rv$worklf <- subset.data(matches=rv$matchdata,worklfull=rv$worklfull)
        rv$worki <- 1
      }else{
        newi <- rv$worki + 1
        if(newi > length(rv$worklf) & length(rv$worklf)>1){
          showModal(modalDialog(title="You have reached the end of the scoring set!",
                                "Bringing you back to the first file...",
                                size="m",easyClose=TRUE))
          newi <- 1
        }
        rv$worki <- newi
      }

      isolate({updateSelectInput(session, "angle",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "focus",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "exposure",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "visible",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "distinct",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "calf",label=NULL,choices=NULL,selected="N/A")})
      isolate({updateSelectInput(session, "parasite",label=NULL,choices=NULL,selected="N/A")})
    })
  }

  #########################################################
  #########################################################
  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Photo Scoring",
                           #########################################################
                           tabPanel("set up",
                                    fluidRow(column(12,h3("Analyst details"),
                                                    br(),textInput("analyst","Enter your name:",value=""),
                                                    br(),textInput("note","Session description:",value=""),
                                                    br(),
                                                    uiOutput("session"))),br(),
                                    fluidRow(column(12,h3("Setup photos to score"),
                                                    br(),uiOutput("workdir"),
                                                    br(),
                                                    checkboxInput("notdone","Filter to as-yet unprocessed photos?",value=FALSE))),

                                    br()),

                           #########################################################
                           tabPanel("GO SCORE",
                                    fluidRow(column(10,imageOutput("workpic")),
                                             column(2,textOutput("workid"),br(),
                                                    uiOutput("workback",inline=FALSE),br(),
                                                    uiOutput("worknext",inline=FALSE),br(),
                                                    textOutput("workstatus",inline=FALSE))),
                                    hr(),
                                    fluidRow(column(3,uiOutput("angle")),
                                             column(3,uiOutput("exposure")),
                                             column(3,uiOutput("focus")),
                                             column(3,uiOutput("visible"))),
                                    fluidRow(column(3,uiOutput("distinct")),
                                             column(3,uiOutput("calf")),
                                             column(3,uiOutput("parasites")),
                                             column(3,br(),uiOutput("save")))
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
