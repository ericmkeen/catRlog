#' Stage events
#'
#' @return desc
#' @export
#'
stage_events <- function(){

  install_exiftool()

  #########################################################
  #########################################################

  server <- function(input, output) {

    #########################################################
    # Setup reactive values

    rv <- reactiveValues()
    rv$worklfull <- NULL
    rv$worklf <- NULL
    rv$exif <- data.frame()
    rv$event <- data.frame()

    #########################################################
    # Get list of photo collections

    output$photofolders <- renderUI({
      workdir <- "../0 photos/photos/" ; workdir
      dirops <- list.files(workdir) ; dirops
      dirops <- paste0(workdir,dirops) ; dirops
      if(length(dirops)>0){
        selectInput("photofolders",label=h4("Select photo folder to work with:"),
                    choices=dirops,selected=1,multiple=FALSE,width="90%")
      }else{ "No photo folders found! Look in catRlog > 0 photos > photos" }
    })

    #########################################################
    # Get image metadata once photo collection is chosen

    observe({input$photofolders
      if(!is.null(input$photofolders) && nchar(input$photofolders)>2){
        #imgdir <- dirops[5] ; imgdir
        imgdir <- input$photofolders
        imglf <- list.files(imgdir) ; imglf
        imgpath <- paste0(imgdir,"/",imglf) ; imgpath
        ixf <- exif_read(imgpath) ; ixf
        names(ixf)
        lfexif <- data.frame(file=imglf,path=imgpath,ixf)
        rv$exif <- lfexif

        dt <- lfexif$CreateDate ; head(dt)
        ds <- substr(dt,1,10) ; ds
        ds <- gsub(":","-",ds) ; ds
        ts <- substr(dt,12,21) ; ts
        dt <- paste(ds,ts) ; dt
        dt <- time.turner(dt)
        if("GPSLatitude" %in% names(lfexif)){lat <- lfexif$GPSLatitude}else{lat <- ""}
        if("GPSLongitude" %in% names(lfexif)){lon <- lfexif$GPSLongitude}else{lon <- ""}

        event <- data.frame(year=dt$yyyy,month=dt$mm,day=dt$dd,time=ts,
                            group="",julian=dt$j,
                            file=imglf,filepath=imgpath,
                            lat,lon)
        head(event)
        rv$event <- event
      }
    })

    #########################################################
    # Button to create and save event table

    output$create <- renderUI({
      if(!is.null(input$photofolders) && length(input$photofolders)>0){
        actionButton("create",h4("Create & save Events spreadsheet"),width="40%")
      }
    })

    observeEvent(input$create,{
      if(!is.null(input$photofolders) && nchar(input$photofolders)>2){
        #imgdir <- dirops[4] ; imgdir
        imgdir <- input$photofolders
        imgcore <- strsplit(imgdir,"/")[[1]] ; imgcore
        imgcore <- imgcore[length(imgcore)] ; imgcore
        eventfn <- paste0("../0 photos/staged events/",imgcore,".csv") ; eventfn
        write.csv(rv$event,file=eventfn,quote=FALSE,row.names=FALSE)

        showNotification("Event spreadsheet has been added to > 0 photos > staged events !")

      }
    })

    #########################################################
    # Data tables

    output$event <- renderDataTable(rv$event, options=list(pageLength = 5))
    output$exif <- renderDataTable(rv$exif)

  }


  #########################################################
  #########################################################
  #########################################################

  ui <- shinyUI(navbarPage("Stage Events spreadsheet based on Photos folder",
                           #########################################################
                           tabPanel("process",
                                    fluidRow(column(12,
                                                    br(),uiOutput("photofolders"),
                                                    br())),
                                    fluidRow(column(12,h4("Preview of event spreadsheet:"))),
                                    fluidRow(column(12,dataTableOutput("event"))),
                                    br(),br(),
                                    fluidRow(column(12,uiOutput("create"))),
                                    br(),br(),br(),br()),

                           #########################################################
                           tabPanel("review",
                                    tabsetPanel(
                                      tabPanel("EXIF data", br(), fluidRow(column(12,dataTableOutput("exif"))))
                                    ))
  ))

  #########################################################
  #########################################################
  #########################################################

  shinyApp(ui = ui, server = server)

  #########################################################
  #########################################################


}
