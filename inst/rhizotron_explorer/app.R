######################
# User interface (UI)
######################

ui<-fluidPage(

  #Add busy spinner
  shinybusy::add_busy_spinner(spin = "fading-circle", onstart = FALSE),

  #App title
  titlePanel("(Mini)Rhizotron Data Explorer"),

  #Sidebar panel for inputs
  sidebarLayout(

    sidebarPanel(

      HTML('<center><img src="logos.png" height=60></center>'),

      br(),

      tabsetPanel(selected="Image processing",

        tabPanel("Copy files", fluid=TRUE,

                 br(),

                 p(strong("Copy new (mini)rhizotron images")),

                 textInput(inputId="path_from",
                           label="From:",
                           placeholder="Provide path...",
                           value="E:/Back-up minirhizotron images/images/POEM2021_images"),

                 textInput(inputId="path_to",
                           label="To:",
                           placeholder="Provide path...",
                           value="Z:/Poem/Minirhizotron images/POEM2021_images"),

                 br(),

                 actionButton(inputId="copy_images",
                              label="Copy new images",
                              icon=shiny::icon(name="copy",
                                               lib="font-awesome"),
                              class = "btn btn-primary")
        ),

        tabPanel("Image processing", fluid=TRUE,

                 br(),

                 textInput(inputId="path",
                           label="Locate folder containing segmented images to analyse",
                           placeholder="Provide path..."),

                 checkboxInput(inputId = "POEM_images",
                               label="These are POEM minirhizotron images",
                               value=TRUE),
                 
                 checkboxInput(inputId = "binary_images",
                               label="These are converted segmentations for RhizoVision Explorer",
                               value=TRUE),

                p(strong("Noise removal")),

                 fluidRow(
                   column(8, checkboxInput(inputId = "median_filter",
                                           label="Do median filtering (noise removal)",
                                           value=FALSE,
                                           width='100%')),
                   column(4, sliderInput(inputId = "kernel_size",
                                         label="Kernel size",
                                         value=3,
                                         min=2,
                                         max=10,
                                         step=1,
                                         ticks=FALSE,
                                         width='90%'))),

                 fluidRow(
                   column(6, numericInput(inputId = "resolution",
                                          label="Image resolution (dpi)",
                                          value=2500,
                                          min=1,
                                          width='100%')),
                   column(6, selectInput(inputId="feature_units",
                                         label="Desired units",
                                         choices = list("px", "cm", "mm"),
                                         selected="mm",
                                         multiple = FALSE,
                                         width='100%'))),

                 p(strong("Root distribution")),

                 checkboxInput(inputId = "root_distribution",
                               label="Calculate root distribution indices",
                               value=FALSE,
                               width='100%'),

                 fluidRow(
                   column(6, sliderInput(inputId="d",
                                         label="% - D value",
                                         value=90,
                                         min=1,
                                         max=99,
                                         step=1,
                                         ticks=FALSE,
                                         width='100%')),
                   column(6, sliderInput(inputId = "drf",
                                         label="% - Deep root fraction",
                                         value=30,
                                         min=1,
                                         max=99,
                                         step=1,
                                         ticks=FALSE,
                                         width='100%'))),

                 br(),

                 actionButton(inputId="process_images",
                              label="Process images",
                              icon=shiny::icon(name="rocket",
                                        lib="font-awesome"),
                              class = "btn btn-primary")
        )#,

        # tabPanel("Variables", fluid=TRUE,
        #
        #          br(),
        #
        #          p(strong("Load existing csv file")),
        #
        #          fluidRow(
        #            column(8, fileInput(inputId="load_existing_file",
        #                                label=NULL,
        #                                multiple=FALSE,
        #                                buttonLabel = "Choose file",
        #                                accept=".csv",
        #                                width='100%')),
        #            column(4, actionButton(inputId="update_csv",
        #                                   label="Load data",
        #                                   width='100%',
        #                                   icon=shiny::icon(name="file-csv",
        #                                                    lib="font-awesome")))),
        #
        #          selectInput(inputId="experiment",
        #                      label="Select POEM experiment(s)",
        #                      choices = list("POEM 2021" = "POEM2021",
        #                                     "POEM 2023" = "POEM2023",
        #                                     "All experiments" = "All"),
        #                      selected="All",
        #                      multiple = FALSE),
        #
        #          fluidRow(
        #            column(6, selectInput(inputId="y",
        #                                  label="Y variable",
        #                                  choices = list("Mean rooting depth" = "mrd",
        #                                                 "Maximum rooting depth" = "maxrd",
        #                                                 "Root surface area" = "rsa",
        #                                                 "Relative root surface area" = "rrsa"),
        #                                  selected="rrsa",
        #                                  multiple = FALSE,
        #                                  width='100%')),
        #            column(6, selectInput(inputId="x",
        #                                  label="X variable",
        #                                  choices = list("PFG order of arrival" = "Arrival",
        #                                                 "Days after sowing" = "Days",
        #                                                 "Depth" = "Depth_cm"),
        #                                  selected="Days",
        #                                  multiple = FALSE,
        #                                  width='100%')))
        # ),

        # tabPanel("Options", fluid=TRUE,
        #
        #          br(),
        #
        #          checkboxInput(inputId="majorgrid",
        #                        label="Remove major grid lines?",
        #                        value=FALSE),
        #
        #          checkboxInput(inputId="minorgrid",
        #                        label="Remove minor grid lines?",
        #                        value=FALSE),
        #
        #          sliderInput(inputId="sizeTitle",
        #                      label="Size of the Y axis title(s)",
        #                      value=16,
        #                      min=1,
        #                      max=25,
        #                      step=1),
        #
        #          sliderInput(inputId="sizeText",
        #                      label="Size of the axis text",
        #                      value=14,
        #                      min=1,
        #                      max=25,
        #                      step=1),
        #
        #          sliderInput(inputId="lineSize",
        #                      label="Line width",
        #                      value=0.5,
        #                      min=0.1,
        #                      max=5,
        #                      step=0.1),
        #
        #          colourpicker::colourInput(inputId="lineColor1",
        #                                    label="Line colour for main variable",
        #                                    value="black",
        #                                    allowTransparent = TRUE),
        #
        #          colourpicker::colourInput(inputId="lineColor2",
        #                                    label="Line colour for second variable",
        #                                    value=viridis::viridis_pal()(4)[3],
        #                                    allowTransparent = TRUE)
        # )

        ), width=4),

    #Main panel for displaying outputs
    mainPanel(

      tabsetPanel(selected="Extracted features",


        tabPanel("File management", fluid=TRUE,

                 br(),
                 textOutput(outputId="n_copied_file"),
                 br(),
                 textOutput(outputId="n_total_file")

        ),

        tabPanel("Extracted features", fluid=TRUE,

                 dataTableOutput(outputId="table_results"),
                 downloadButton(outputId = "downloadData", label="Download data as csv file")


        ),

        # tabPanel("Plot POEM results", fluid=TRUE,
        #
        #          plotOutput(outputId = "plot_results")
        #
        # ),

        tabPanel("About", fluid=TRUE,

                 br(),

                 p("Developer: Benjamin Delory"),
                 p("Email: Benjamin.Delory@leuphana.de"),
                 p("Phone: +49-4131-677.2185"),

                 br(),

                 HTML('<center><img src="Logo_POEM.png" width=150></center>')

        )

    ))))

#########
# Server
#########

server <- function(input, output){

  reticulate::source_python(paste(system.file("python", package = "POEMdemo"), "py_functions.py", sep="/"))

  observeEvent(input$copy_images, {

    n<-copy_new_images(path_from=input$path_from, path_to=input$path_to) #Run python code

    output$n_copied_file<-renderText({

      paste(n, " new file(s) were added to the destination folder\n",
            sep="")})

    output$n_total_file<-renderText({

      paste(length(list.files(input$path_to)), " file(s) are now present in the destination folder",
            sep="")})

  })

  my_df<-eventReactive(input$process_images, {

          #List all png files with path
          files<-sort(list.files(input$path, full.names = TRUE, pattern="\\.png$"))

          #Create empty data frame to store results

          if (input$root_distribution==TRUE){

              data<-data.frame(image=rep(NA, length(files)),
                               height=rep(NA, length(files)),
                               width=rep(NA, length(files)),
                               mrd=rep(NA, length(files)),
                               maxrd=rep(NA, length(files)),
                               d=rep(NA, length(files)),
                               drf=rep(NA, length(files)),
                               rsa=rep(NA, length(files)),
                               rrsa=rep(NA, length(files)))}

          else {

            data<-data.frame(image=rep(NA, length(files)),
                             height=rep(NA, length(files)),
                             width=rep(NA, length(files)),
                             rsa=rep(NA, length(files)),
                             rrsa=rep(NA, length(files)))
          }

          #Unit conversion + rename columns
          if (input$feature_units=="px") {
            factor<-1
            if (input$root_distribution==TRUE) {colnames(data)[c(2:6,8)]<-c("height_px", "width_px", "mrd_px", "maxrd_px", paste("d", input$d, "_px", sep=""), "rsa_px2")}
            else {colnames(data)[2:4]<-c("height_px", "width_px", "rsa_px2")}}

          if (input$feature_units=="cm") {
            factor<-cm(1)/input$resolution
            if (input$root_distribution==TRUE) {colnames(data)[c(2:6,8)]<-c("height_cm", "width_cm", "mrd_cm", "maxrd_cm", paste("d", input$d, "_cm", sep=""), "rsa_cm2")}
            else {colnames(data)[2:4]<-c("height_cm", "width_cm", "rsa_cm2")}}

          if (input$feature_units=="mm") {
            factor<-10*cm(1)/input$resolution
            if (input$root_distribution==TRUE) {colnames(data)[c(2:6,8)]<-c("height_mm", "width_mm", "mrd_mm", "maxrd_mm", paste("d", input$d, "_mm", sep=""), "rsa_mm2")}
            else {colnames(data)[2:4]<-c("height_mm", "width_mm", "rsa_mm2")}}

          withProgress(message="Processing images", value=0, {

          #Start counter
          index<-0

          for (file in files){ #Process each image in folder

            index<-index+1

            incProgress(1/length(files), detail=paste("Image ", index, " of ", length(files), sep=""))

            features<-process_image(path=file,
                                    binary=input$binary_images,
                                    median_filter=input$median_filter,
                                    kernel=as.integer(input$kernel_size),
                                    root_distribution=input$root_distribution,
                                    D=input$d,
                                    DRF=input$drf) #Run python code

            if (input$root_distribution==TRUE){

              data[index,]<-c(as.character(features$image),
                              as.numeric(features$height)*factor,
                              as.numeric(features$width)*factor,
                              as.numeric(features$mrd)*factor,
                              as.numeric(features$maxrd)*factor,
                              as.numeric(features$d)*factor,
                              as.numeric(features$drf),
                              as.numeric(features$rsa)*factor^2,
                              as.numeric(features$rrsa))}

            else {

              data[index,]<-c(as.character(features$image),
                              as.numeric(features$height)*factor,
                              as.numeric(features$width)*factor,
                              as.numeric(features$rsa)*factor^2,
                              as.numeric(features$rrsa))

            }}})

          if (input$POEM_images==TRUE){

            design_POEM<-data.frame(Tube=1:40,
                                    Plot=rep(c(201:207, 210:211, 213:219, 221:222, 224:225), each=2),
                                    Arrival=rep(c("G","S","F","S","L","F","G","S","F","L","L","S","L","F","L","G","G","F","S","G"), each=2),
                                    Replicate=rep(c(1,1,1,2,1,2,2,3,3,2,3,4,4,4,5,3,4,5,5,5), each=2),
                                    Start=rep("2021.04.13", 40))

            design_POEM$Start<-as.POSIXct(strptime(design_POEM$Start, format="%Y.%m.%d"))

            data <-separate(data, "image", c("Name_experiment", "Tube", "Location", "Date", "Time", "Session", "User"), sep="_")
            data$Tube<-as.numeric(str_remove(data$Tube, "T"))
            data$Location<-as.numeric(str_remove(data$Location, "L"))
            data$Time<-as.POSIXct(strptime(paste(data$Date, data$Time, sep=" "), format="%Y.%m.%d %H%M%S"))
            data$Date<-as.POSIXct(strptime(data$Date, format="%Y.%m.%d"))
            data$Session<-as.numeric(data$Session)
            data$User<-str_remove(data$User, ".png")
            data<-data %>%
              add_column(Depth_cm=1.4142*data$Location-9.8995, .after="Location") %>%
              add_column(Plot=design_POEM$Plot[match(data$Tube, design_POEM$Tube)], .after="Name_experiment") %>%
              add_column(Arrival=design_POEM$Arrival[match(data$Tube, design_POEM$Tube)], .after="Plot") %>%
              add_column(Replicate=design_POEM$Replicate[match(data$Tube, design_POEM$Tube)], .after="Arrival") %>%
              add_column(Days=round(as.numeric(difftime(data$Date-design_POEM$Start[match(data$Tube, design_POEM$Tube)], units="days"))), .after="Date")
            data$Days[data$Days<=3 & data$Experiment=="POEM2021"]<-0
            data$Arrival<-factor(data$Arrival, levels=c("S", "F", "G", "L"))}

          return(data)

        })

  # my_new_df<-eventReactive(input$update_csv, {
  #
  #   ext <- tools::file_ext(input$load_existing_file$name)
  #   validate(need(ext == "csv", "Please upload a csv file"))
  #   return(read.csv(input$load_existing_file$datapath, header = TRUE))
  #
  #   })

  # output$table_results<-renderDataTable(
  #
  #   {if (is.null(input$load_existing_file)==TRUE) {my_df()}
  #
  #   else {my_new_df()}}, options=list(pageLength=10))

  output$table_results<-renderDataTable(my_df(), options=list(pageLength=10))

  output$downloadData <- downloadHandler(
    filename=function(){paste("Features", "_", Sys.Date(), ".csv", sep="")},
    content=function(file){write.csv(my_df(), file, row.names=FALSE)})

}

shinyApp(ui, server)
