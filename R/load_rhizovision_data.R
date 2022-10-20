load_rhizovision_data<-function(file, POEM=TRUE, ...){

  #file is a character string for the path to the RhizoVision csv file
  #POEM is a Boolean variable (are these POEM minirhizotron data?)

  #Read RVE data
  data <- read.csv(file, ...)

  #If POEM images
  if (POEM==TRUE){

    design_POEM<-data.frame(Tube=1:40,
                            Plot=rep(c(201:207, 210:211, 213:219, 221:222, 224:225), each=2),
                            Arrival=rep(c("G","S","F","S","L","F","G","S","F","L","L","S","L","F","L","G","G","F","S","G"), each=2),
                            Replicate=rep(c(1,1,1,2,1,2,2,3,3,2,3,4,4,4,5,3,4,5,5,5), each=2),
                            Start=rep("2021.04.13", 40))

    design_POEM$Start<-as.POSIXct(strptime(design_POEM$Start, format="%Y.%m.%d"))

    data <-separate(data, "File.Name", c("Name_experiment", "Tube", "Location", "Date", "Time", "Session", "User"), sep="_")
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
      add_column(Days=round(as.numeric(data$Date-design_POEM$Start[match(data$Tube, design_POEM$Tube)])/(24*60*60)), .after="Date")
    data$Days[data$Days<=3 & data$Experiment=="POEM2021"]<-0
    data$Arrival<-factor(data$Arrival, levels=c("S", "F", "G", "L"))}

  return(data)

}
