setwd("C:/Users/davin/Documents/PhD/Writing/Heterogeneity/Supp_Data/Tool_promoter_platecurver/300623_rep5/")
library(readxl)
library(growthcurver)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(plater)
library(directlabels)
library(gtools)

#plater import
plated <- read_plate(
  file = "300623_layout.csv",
  well_ids_column = "Wells")

#take away OD column from the plater tibble and transpose
platerbind <- plated[,1:2] %>% t()

#####functionise plotting#####
#save formatting as a theme
mytheme<-theme(axis.text.x =element_text(size=12),
               axis.text.y =element_text(size=12),
               axis.title =element_text(size=16),
               axis.line = element_line(size=1.2),
               axis.ticks = element_line(size=1.2),
               legend.position = "none")

ggplotify.bc<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=4) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 28), breaks = seq(0, 28, by = 4)) +
    scale_y_continuous(limits = c(0,1.2), breaks = seq(0, 1.2, by = 0.4))}

ggplotify.bcypdx<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=4) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 28), breaks = seq(0, 28, by = 4)) +
    scale_y_continuous(limits = c(0,1.2), breaks = seq(0, 1.2, by = 0.4))}

##GGLPLOT FUNCTON FOR DIFFERENT SM AND YEPG SCALE
ggplotify.bcsm<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=4) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 28), breaks = seq(0, 28, by = 4)) +
    scale_y_continuous(limits = c(0,0.6), breaks = seq(0, 0.6, by = 0.1))}

ggplotify.bcyepg<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=4) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 56), breaks = seq(0, 56, by = 8)) +
    scale_y_continuous(limits = c(0,0.6), breaks = seq(0, 0.6, by = 0.1))}


#plotting function after model prediction in growthcurver
ggplotify.curves <- function(g){ggplot(g, aes(x=Time, y=OD)) +
    geom_point(aes(y=value), alpha=0.5, color="blue") +
    geom_line(aes(y=pred.od), color="red") +
    facet_wrap(~variable, ncol=4) +
    scale_x_continuous(limits = c(0, 54), breaks = seq(0, 54, by = 8)) +
    theme_bw()}

#####Extract OD function#####
extractify.OD <- function (f){
  csv <- read.csv(f)
  ods <- csv[c(1:8),c(2:13)]
  transv <- as.vector(t(ods))
  return(transv)
}


#####SORTFILES#####
#folder for files containing OD
sortfiles<-mixedsort(list.files(pattern = '300623_t'))
sortfiles


#tidy up function - REMEMBER TO CHANGE TIME POINTS
tidy.up<-function(newseries){
  Time<-c("0","2","3","5","6","7","8","9","10","11","11.5",
          "24.5","24.75","25","26","27","29","30","51","52","53","54")
  tns<-t(newseries)
  nice<-rbind(platerbind[2,], tns)
  samples<-as.data.frame(nice[c(1,3:nrow(nice)),], col.names=TRUE, stringsAsFactors=FALSE)
  colnames(samples)<-samples[1,]
  samples_clean<-samples[2:nrow(samples),]
  merged<-cbind(Time,samples_clean, stringsAsFactors=FALSE) 
  fornum <- merged[!is.na(names(merged))]
  num<- fornum %>% mutate_if(sapply(fornum, is.character), as.numeric)
  write.csv(num, "tidy_new.csv", row.names=FALSE) #######CHANGE FILE NAME
}

#check extractify
##length(sortfiles)
##extractify.OD(sortfiles[[22]])


#series dataframe for loop
series <- data.frame(well=c("A01","A02","A03","A04","A05","A06","A07","A08","A09","A10","A11","A12",
                            "B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12",
                            "C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12",
                            "D01","D02","D03","D04","D05","D06","D07","D08","D09","D10","D11","D12",
                            "E01","E02","E03","E04","E05","E06","E07","E08","E09","E10","E11","E12",
                            "F01","F02","F03","F04","F05","F06","F07","F08","F09","F10","F11","F12",
                            "G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12",
                            "H01","H02","H03","H04","H05","H06","H07","H08","H09","H10","H11","H12"))

#loop
for(f in sortfiles){series[[f]]<-cbind(extractify.OD(f))}
tidy.up(series)
tidied<-read.csv("tidy_new.csv", header = T, stringsAsFactors = F)
##View(tidied)

####Extraction, blank-correction and plotting#####---------------------------------

#####extraction#####
#extract ypd1
ypd1<- select(tidied,1,contains("ypd1"))
names(ypd1)[names(ypd1) == "blank_ypd1"] <- "blank"

#extract ypd2
ypd2<- select(tidied,1,contains("ypd2"))
names(ypd2)[names(ypd2) == "blank_ypd2"] <- "blank"


#extract ypdx1
ypdx1<- select(tidied,1,contains("ypdx1"))
names(ypdx1)[names(ypdx1) == "blank_ypdx1"] <- "blank"

#extract ypdx2
ypdx2<- select(tidied,1,contains("ypdx2"))
names(ypdx2)[names(ypdx2) == "blank_ypdx2"] <- "blank"


#extract sm1
sm1<- select(tidied,1,contains("sm1"))
names(sm1)[names(sm1) == "blank_sm1"] <- "blank"

#extract sm2
sm2<- select(tidied,1,contains("sm2"))
names(sm2)[names(sm2) == "blank_sm2"] <- "blank"


#extract yepg1
yepg1<- select(tidied,1,contains("yepg1"))
names(yepg1)[names(yepg1) == "blank_yepg1"] <- "blank"

#extract yepg2
yepg2<- select(tidied,1,contains("yepg2"))
names(yepg2)[names(yepg2) == "blank_yepg2"] <- "blank"


#####blank-correction#####
#note: [,3:ncol(df)-1] means 2nd column to 2nd to last column. [,3:(ncol(df)-1)] means 3rd column to 2nd to last column
ypd1_bc<-ypd1
ypd1_bc[,2:(ncol(ypd1_bc)-1)] = ypd1_bc[,2:(ncol(ypd1_bc)-1)] - ypd1_bc[,ncol(ypd1_bc)]

ypd2_bc<-ypd2
ypd2_bc[,2:(ncol(ypd2_bc)-1)] = ypd2_bc[,2:(ncol(ypd2_bc)-1)] - ypd2_bc[,ncol(ypd2_bc)]

ypdx1_bc<-ypdx1
ypdx1_bc[,2:(ncol(ypdx1_bc)-1)] = ypdx1_bc[,2:(ncol(ypdx1_bc)-1)] - ypdx1_bc[,ncol(ypdx1_bc)]

ypdx2_bc<-ypdx2
ypdx2_bc[,2:(ncol(ypdx2_bc)-1)] = ypdx2_bc[,2:(ncol(ypdx2_bc)-1)] - ypdx2_bc[,ncol(ypdx2_bc)]

sm1_bc<-sm1
sm1_bc[,2:(ncol(sm1_bc)-1)] = sm1_bc[,2:(ncol(sm1_bc)-1)] - sm1_bc[,ncol(sm1_bc)]

sm2_bc<-sm2
sm2_bc[,2:(ncol(sm2_bc)-1)] = sm2_bc[,2:(ncol(sm2_bc)-1)] - sm2_bc[,ncol(sm2_bc)]

yepg1_bc<-yepg1
yepg1_bc[,2:(ncol(yepg1_bc)-1)] = yepg1_bc[,2:(ncol(yepg1_bc)-1)] - yepg1_bc[,ncol(yepg1_bc)]

yepg2_bc<-yepg2
yepg2_bc[,2:(ncol(yepg2_bc)-1)] = yepg2_bc[,2:(ncol(yepg2_bc)-1)] - yepg2_bc[,ncol(yepg2_bc)]



#melting
ypd1_melt <- melt(ypd1_bc, id.vars = "Time")
ypd2_melt <- melt(ypd2_bc, id.vars = "Time")
ypdx1_melt <- melt(ypdx1_bc, id.vars = "Time")
ypdx2_melt <- melt(ypdx2_bc, id.vars = "Time")
sm1_melt <- melt(sm1_bc, id.vars = "Time")
sm2_melt <- melt(sm2_bc, id.vars = "Time")
yepg1_melt <- melt(yepg1_bc, id.vars = "Time")
yepg2_melt <- melt(yepg2_bc, id.vars = "Time")

####plotting####
#plotting
ypd1_plot<-ggplotify.bc(ypd1_melt)
ypd2_plot<-ggplotify.bc(ypd2_melt)

ypdx1_plot<-ggplotify.bcypdx(ypdx1_melt)
ypdx2_plot<-ggplotify.bcypdx(ypdx2_melt)

sm1_plot<-ggplotify.bcsm(sm1_melt)
sm2_plot<-ggplotify.bcsm(sm2_melt)

yepg1_plot<-ggplotify.bcyepg(yepg1_melt)
yepg2_plot<-ggplotify.bcyepg(yepg2_melt)

ypd1_plot

ypdx1_plot

sm1_plot

yepg1_plot






##--------------------------------REAL-TIME SCRIPT END------------------------------##


#####GROWTHCURVER#### ================================================================================
plate_ypd1 <- SummarizeGrowthByPlate(ypd1, bg_correct = "blank", plot_fit = TRUE, plot_file = "plate_ypd1.pdf") 
write.xlsx(plate_ypd1, 'plate_gc_ypd1.xlsx')
##plate_ypd2 <- SummarizeGrowthByPlate(ypd2, bg_correct = "blank", plot_fit = TRUE,plot_file = "plate_ypd2.pdf") 
##write.xlsx(plate_ypd2, 'plate_gc_ypd2.xlsx')


plate_ypdx1 <- SummarizeGrowthByPlate(ypdx1, bg_correct = "blank", plot_fit = TRUE, plot_file = "plate_ypdx1.pdf") 
write.xlsx(plate_ypdx1, 'plate_gc_ypdx1.xlsx')
##plate_ypdx2 <- SummarizeGrowthByPlate(ypdx2, bg_correct = "blank", plot_fit = TRUE, plot_file = "plate_ypdx2.pdf") 
##write.xlsx(plate_ypdx2, 'plate_gc_ypdx2.xlsx')


plate_sm1 <- SummarizeGrowthByPlate(sm1, bg_correct = "blank", plot_fit = TRUE, plot_file = "plate_sm1.pdf") 
write.xlsx(plate_sm1, 'plate_gc_sm1.xlsx')
##plate_sm2 <- SummarizeGrowthByPlate(sm2, bg_correct = "blank", plot_fit = TRUE, plot_file = "plate_sm2.pdf") 
##write.xlsx(plate_sm2, 'plate_gc_sm2.xlsx')


plate_yepg1 <- SummarizeGrowthByPlate(yepg1, bg_correct = "blank", plot_fit = TRUE, plot_file = "plate_yepg1.pdf") 
write.xlsx(plate_yepg1, 'plate_gc_yepg1.xlsx')
##plate_yepg2 <- SummarizeGrowthByPlate(yepg2, bg_correct = "blank", plot_fit = TRUE, plot_file = "plate_yepg2.pdf") 
##write.xlsx(plate_yepg2, 'plate_gc_yepg2.xlsx')



#####make models#####
models.ypd1 <- lapply(ypd1[2:ncol(ypd1)], function(x) SummarizeGrowth(ypd1$Time,x))
##models.ypd2 <- lapply(ypd2[2:ncol(ypd2)], function(x) SummarizeGrowth(ypd2$Time,x))

models.ypdx1 <- lapply(ypdx1[2:ncol(ypdx1)], function(x) SummarizeGrowth(ypdx1$Time,x))
##models.ypdx2 <- lapply(ypdx2[2:ncol(ypdx2)], function(x) SummarizeGrowth(ypdx2$Time,x))

models.sm1 <- lapply(sm1[2:ncol(sm1)], function(x) SummarizeGrowth(sm1$Time,x))
##models.sm2 <- lapply(sm2[2:ncol(sm2)], function(x) SummarizeGrowth(sm2$Time,x))

models.yepg1 <- lapply(yepg1[2:ncol(yepg1)], function(x) SummarizeGrowth(yepg1$Time,x))
##models.yepg2 <- lapply(yepg2[2:ncol(yepg2)], function(x) SummarizeGrowth(yepg2$Time,x))


#loop to get predictions
predicted.ypd1 <-data.frame(time=ypd1$Time)
for (i in names(ypd1[2:ncol(ypd1)])){
  predicted.ypd1[[i]] <- predict(models.ypd1[[i]]$model)}
##predicted.ypd2 <-data.frame(time=ypd2$Time)
##for (i in names(ypd2[2:ncol(ypd2)])){
##  predicted.ypd2[[i]] <- predict(models.ypd2[[i]]$model)}

predicted.ypdx1 <-data.frame(time=ypdx1$Time)
for (i in names(ypdx1[2:ncol(ypdx1)])){
  predicted.ypdx1[[i]] <- predict(models.ypdx1[[i]]$model)}
##predicted.ypdx2 <-data.frame(time=ypdx2$Time)
##for (i in names(ypdx2[2:ncol(ypdx2)])){
##  predicted.ypdx2[[i]] <- predict(models.ypdx2[[i]]$model)}

predicted.sm1 <-data.frame(time=sm1$Time)
for (i in names(sm1[2:ncol(sm1)])){
  predicted.sm1[[i]] <- predict(models.sm1[[i]]$model)}
##predicted.sm2 <-data.frame(time=sm2$Time)
##for (i in names(sm2[2:ncol(sm2)])){
##  predicted.sm2[[i]] <- predict(models.sm2[[i]]$model)}

predicted.yepg1 <-data.frame(time=yepg1$Time)
for (i in names(yepg1[2:ncol(yepg1)])){
  predicted.yepg1[[i]] <- predict(models.yepg1[[i]]$model)}
##predicted.yepg2 <-data.frame(time=yepg2$Time)
##for (i in names(yepg2[2:ncol(yepg2)])){
##  predicted.yepg2[[i]] <- predict(models.yepg2[[i]]$model)}



#melt predicted dataframe and add to observed table
mpred.ypd1 <- melt(predicted.ypd1, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_ypd1 <- left_join(ypd1_melt, mpred.ypd1, by=c("Time"="time", "variable"="Sample"))

##mpred.ypd2 <- melt(predicted.ypd2, id.vars="time", variable.name="Sample", value.name="pred.od")
##obspred_ypd2 <- left_join(ypd2_melt, mpred.ypd2, by=c("Time"="time", "variable"="Sample"))


mpred.ypdx1 <- melt(predicted.ypdx1, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_ypdx1 <- left_join(ypdx1_melt, mpred.ypdx1, by=c("Time"="time", "variable"="Sample"))

##mpred.ypdx2 <- melt(predicted.ypdx2, id.vars="time", variable.name="Sample", value.name="pred.od")
##obspred_ypdx2 <- left_join(ypdx2_melt, mpred.ypdx2, by=c("Time"="time", "variable"="Sample"))


mpred.sm1 <- melt(predicted.sm1, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_sm1 <- left_join(sm1_melt, mpred.sm1, by=c("Time"="time", "variable"="Sample"))

##mpred.sm2 <- melt(predicted.sm2, id.vars="time", variable.name="Sample", value.name="pred.od")
##obspred_sm2 <- left_join(sm2_melt, mpred.sm2, by=c("Time"="time", "variable"="Sample"))


mpred.yepg1 <- melt(predicted.yepg1, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_yepg1 <- left_join(yepg1_melt, mpred.yepg1, by=c("Time"="time", "variable"="Sample"))

##mpred.yepg2 <- melt(predicted.yepg2, id.vars="time", variable.name="Sample", value.name="pred.od")
##obspred_yepg2 <- left_join(yepg2_melt, mpred.yepg2, by=c("Time"="time", "variable"="Sample"))




#plotting - make sure plot is exported when done

ggplotify.curves(obspred_ypd1)
##ggplotify.curves(obspred_ypd2)

ggplotify.curves(obspred_ypdx1)
##ggplotify.curves(obspred_ypdx2)

ggplotify.curves(obspred_sm1)
##ggplotify.curves(obspred_sm2)

ggplotify.curves(obspred_yepg1)
##ggplotify.curves(obspred_yepg2)



##--------------------------------------###SCRIPT END###------------------------------------------

