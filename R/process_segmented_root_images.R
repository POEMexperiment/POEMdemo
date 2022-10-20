############################################
# Function to process segmented root images
############################################

# This function returns a data frame that is ready to use for data exploration and analysis in R.

process_segmented_root_images<-function(path, root.distribution=TRUE, res=2500, units="mm", POEM=TRUE,
                                        median.filter=TRUE, kernel.size=3, D=90, DRF=30){

  #List all png files with path
  files<-sort(list.files(path, full.names = TRUE, pattern="\\.png$"))

  #Source python code
  reticulate::source_python(paste(system.file("python", package = "POEM"),"py_functions.py", sep="/"))

  #Create empty data frame to store results

  if (root.distribution==TRUE){

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
  if (units=="px") {
    factor<-1
    if (root.distribution==TRUE) {colnames(data)[c(2:6,8)]<-c("height_px", "width_px", "mrd_px", "maxrd_px", paste("d", D, "_px", sep=""), "rsa_px2")}
    else {colnames(data)[2:4]<-c("height_px", "width_px", "rsa_px2")}}

  if (units=="cm") {
    factor<-cm(1)/res
    if (root.distribution==TRUE) {colnames(data)[c(2:6,8)]<-c("height_cm", "width_cm", "mrd_cm", "maxrd_cm", paste("d", D, "_cm", sep=""), "rsa_cm2")}
    else {colnames(data)[2:4]<-c("height_cm", "width_cm", "rsa_cm2")}}

  if (units=="mm") {
    factor<-10*cm(1)/res
    if (root.distribution==TRUE) {colnames(data)[c(2:6,8)]<-c("height_mm", "width_mm", "mrd_mm", "maxrd_mm", paste("d", D, "_mm", sep=""), "rsa_mm2")}
    else {colnames(data)[2:4]<-c("height_mm", "width_mm", "rsa_mm2")}}

    #Start counter
    index<-0

    #Create progress bar
    pb <- tcltk::tkProgressBar(title = "Image processing in progress", min = 0,
                        max = length(files), width = 300)

    for (file in files){ #Process each image in folder

      index<-index+1

      features<-process_image(path=file,
                              median_filter=median.filter,
                              kernel=as.integer(kernel.size),
                              root_distribution=root.distribution,
                              D=D,
                              DRF=DRF) #Run python code

      tcltk::setTkProgressBar(pb, index, label=paste(round(index/length(files)*100, 0), "% done"))

      if (root.distribution==TRUE){

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

      }}

    close(pb)

  if (POEM==TRUE){

    design_POEM<-data.frame(Tube=1:40,
                            Plot=rep(c(201:207, 210:211, 213:219, 221:222, 224:225), each=2),
                            Arrival=c("G","S","F","S","L","F","G","S","F","L","L","S","L","F","L","G","G","F","S","G"),
                            Replicate=c(1,1,1,2,1,2,2,3,3,2,3,4,4,4,5,3,4,5,5,5),
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
      add_column(Days=as.numeric(data$Date-design_POEM$Start[match(data$Tube, design_POEM$Tube)]), .after="Date")
    data$Days[data$Days<=3 & data$Experiment=="POEM2021"]<-0
    data$Arrival<-factor(data$Arrival, levels=c("S", "F", "G", "L","B"))}

  return(data)
}
