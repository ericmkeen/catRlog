#' Update photo in reference catalog
#'
#' @param catalog_photos Path to folder with reference catalog ID photos. The default follows the instructions for the `catRlog` system setup.
#' @param events Path to folder with events spreadsheets. The default follows the instructions for the `catRlog` system setup.
#' @param photo_collections Path to folder containing subfolders of photo collections. The default follows the instructions for the `catRlog` system setup.

#' @return Shiny app. See the [vignette](https://ericmkeen.github.io/catRlog/) for a detailed user guide.
#' @export
#' @import shiny
#' @import DT
#' @import shinyjs
#' @import dplyr
#' @import tidyselect
#'
update_catalog <- function(catalog_photos = 'catalog/catalog/',
                           events = 'events/',
                           photo_collections = 'photos/photos/'){

  # Load photo key
  # (Inventory the IDs you can get photos of based on event logs)
  LOG <- setup_photo_key(events = events,
                         photo_collections = photo_collections)

  #########################################################
  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values

    rv <- reactiveValues()
    rv$log <- LOG
    rv$refdir <- catalog_photos
    rv$reflf <- c()
    rv$refi <- 1
    rv$refid <- ""
    rv$id <- ""
    rv$worklf <- c()
    rv$worki <- 1
    rv$maybes <- c()

    #########################################################
    # Stage reference catalog (show only those IDs present in event files & photo collections)

    observe({rv$refdir
      lf <- list.files(rv$refdir)
      keeps <- c()
      ids <- unique(as.character(rv$log$id))
      for(i in 1:length(ids)){
        idi <- ids[i] ; idi
        keepi <- grep(idi,lf)
        keeps <- c(keeps,keepi)
      }
      lf <- lf[keeps]
      lf <- paste0(rv$refdir,lf)
      rv$reflf <- lf
    })

    #########################################################
    # Stage ID info for currently selected individual

    observe({
      rv$refid <- gsub(rv$refdir,"",rv$reflf[rv$refi])
      refid <- rv$refid
      idext <- paste0(".",tools::file_ext(refid)) ; idext
      id <- gsub(idext,"",refid)
      rv$id <- substr(id,1,(nchar(id)-1))
    })


    #########################################################
    # Stage photos to consider for replacing catalogue

    observe({
      if(length(rv$reflf)>0 && nchar(rv$id)>1){
        matchi <- which(as.character(rv$log$id)==as.character(rv$id))
        if(length(matchi)>0){
          rv$worklf <- as.character(rv$log$imgpath[matchi])
        }else{
          rv$worklf <- c()
        }
      }
      print(rv$id)
      print(rv$reflf)
      print(rv$worklf)
    })

    #########################################################
    # Display images and associated data

    # Reference picture
    output$refid <- renderText(rv$refid)
    output$refstatus <- renderText({paste0("Image ",rv$refi," out of ",length(rv$reflf)) })
    output$refpic <- renderImage({filename <- as.character(rv$reflf[rv$refi])
    list(src = filename,width="90%")}, deleteFile = FALSE)

    # Photo to match
    output$workid <- renderText({ rv$worklf[rv$worki] })
    output$workstatus <- renderText({ paste0("Image ",rv$worki," out of ",length(rv$worklf)) })
    output$workpic <- renderImage({filename <- as.character(rv$worklf[rv$worki])
    list(src = filename,width="90%")}, deleteFile = FALSE)

    #########################################################
    # Setup reactive input objects

    output$worknext <- renderUI({
      if(length(rv$worklf)>0){ actionButton("worknext",h4("Next")) }
    })

    output$workback <- renderUI({
      if(length(rv$worklf)>0){ actionButton("workback",h4("Back")) }
    })

    output$refnext <- renderUI({ actionButton("refnext",h4("Next")) })

    output$refback <- renderUI({ actionButton("refback",h4("Back")) })

    #########################################################
    # Setup action buttons

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

    observeEvent(input$refnext, {
      newi <- rv$refi + 1
      if(newi > length(rv$reflf)){newi <- 1}
      rv$refi <- newi
      rv$worki <- 1
    })

    observeEvent(input$refback, {
      newi <- rv$refi - 1
      if(newi < 1){newi <- length(rv$reflf)}
      rv$refi <- newi
      rv$worki <- 1
    })

    #########################################################
    # Button: REPLACE PHOTO

    observeEvent(input$replace, {
      # Gather info on catalog photo to be replaced
      catid <- rv$refid
      catpath <- as.character(rv$reflf[rv$refi])
      print(catid)
      print(catpath)

      # Gather info on photo that will replace it
      newpath <- as.character(rv$worklf[rv$worki])
      print(newpath)

      # Create replacement file path
      repath <- catpath
      repath <- gsub("/catalog/","/replaced/",catpath)
      fext <- paste0(".",tools::file_ext(repath))
      repath <- gsub(fext,"",repath)
      repath <- paste0(repath,"_",gsub("-","",as.character(Sys.Date())),fext)
      print(repath)

      # Copy catalog photo to replaced folder
      file.copy(from=catpath,to=repath)

      # Replace catalog photo with new photo
      file.copy(from=newpath,to=catpath,overwrite=TRUE)

      # Update app's log
      rv$log <- setup.photo.key()

      # Update refi
      rv$refi <- rv$refi + 1
      rv$refi <- rv$refi - 1
    })

    #########################################################
    # Button: ADD NEW FEATURE

    observeEvent(input$newfeature, {
      # Gather info on photo that will replace it
      newpath <- as.character(rv$worklf[rv$worki])
      fext <- paste0(".",tools::file_ext(newpath)) ; fext
      print(newpath)

      # Gather info on catalog photo to be replaced
      catid <- as.character(rv$id)
      catpath <- paste0(catalog_photos,catid,"X",fext)
      print(catid)
      print(catpath)

      # Replace catalog photo with new photo
      file.copy(from=newpath,to=catpath,overwrite=TRUE)

      # Update app's log
      rv$log <- setup.photo.key()

      # Alert user
      showNotification("This image has been added to the catalog,
                     with a feature placeholder X.
                     Go to folder and replace with the correct feature letter.")

    })

  }

  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Compare IDs to catalog image",

                           #########################################################
                           tabPanel("COMPARE",
                                    fluidRow(column(9,imageOutput("refpic")),
                                             column(3,h3("Reference Catalog"),
                                                    textOutput("refid"),br(),
                                                    uiOutput("refback",inline=TRUE),
                                                    uiOutput("refnext",inline=TRUE),
                                                    textOutput("refstatus",inline=TRUE),
                                                    br(),br())),
                                    fluidRow(column(9,imageOutput("workpic")),
                                             column(3,h3("Sightings of this whale"),
                                                    textOutput("workid"),br(),
                                                    uiOutput("workback",inline=TRUE),
                                                    uiOutput("worknext",inline=TRUE),
                                                    textOutput("workstatus",inline=FALSE),
                                                    br(),br(),
                                                    actionButton("replace",h4("Replace catalog image!")),
                                                    br(),br(),
                                                    actionButton("newfeature",h4("Add as new feature")),
                                                    br(),br()))
                           )


  ))

  #########################################################
  #########################################################
  #########################################################

  shinyApp(ui = ui, server = server)

  #########################################################
  #########################################################

}
