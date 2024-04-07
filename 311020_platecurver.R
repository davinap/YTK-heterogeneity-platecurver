setwd("C:/Users/davin/Documents/PhD/Writing/Heterogeneity/Supp_Data/YTK_platecurver/311020_platecurver/")
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
  file = "311020_layout.csv",
  well_ids_column = "Wells")

#take away OD column from the plater tibble and transpose
platerbind <- plated[,1:2] %>% t()

#####SORTFILES#####
#folder for files containing OD
sortfiles<-mixedsort(list.files(pattern = '.xlsx'))
sortfiles

#####FUNCTIONS#####
#extract OD function
extractify.OD <- function(f){
  excel <-read_excel(f, col_names=FALSE, 
                     range = "C27:N34", col_types = c("numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric"))
  transv <- as.vector((t(excel)))
  return(transv)}

#tidy up function - REMEMBER TO CHANGE TIME POINTS
tidy.up<-function(newseries){
  Time<-c("0","1", "2","3","4","5","6","7","8","8.5","10","11","11.25","12","13","23.25","24","25",
          "25.5","26","27","48","49","50","51","52","53","54","55","55.5","56")
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
View(tidied)

####Extraction, blank-correction and plotting#####---------------------------------

#####extraction#####
#extract ypd1
ypd1<- select(tidied,1,contains("ypd1"))
names(ypd1)[names(ypd1) == "blank_ypd1"] <- "blank"

#extract ypdx1
ypdx1<- select(tidied,1,contains("ypdx1"))
names(ypdx1)[names(ypdx1) == "blank_ypdx1"] <- "blank"
#view(ypdx1)

#extract sm1
sm1<- select(tidied,1,contains("sm1"))
names(sm1)[names(sm1) == "blank_sm1"] <- "blank"
#view(sm1)

#extract yepg1
yepg1<- select(tidied,1,contains("yepg1"))
names(yepg1)[names(yepg1) == "blank_yepg1"] <- "blank"


#####blank-correction#####
#note: [,3:ncol(df)-1] means 2nd column to 2nd to last column. [,3:(ncol(df)-1)] means 3rd column to 2nd to last column
ypd1_bc<-ypd1
ypd1_bc[,2:(ncol(ypd1_bc)-1)] = ypd1_bc[,2:(ncol(ypd1_bc)-1)] - ypd1_bc[,ncol(ypd1_bc)]
head(ypd1_bc)

ypdx1_bc<-ypdx1
ypdx1_bc[,2:(ncol(ypdx1_bc)-1)] = ypdx1_bc[,2:(ncol(ypdx1_bc)-1)] - ypdx1_bc[,ncol(ypdx1_bc)]

sm1_bc<-sm1
sm1_bc[,2:(ncol(sm1_bc)-1)] = sm1_bc[,2:(ncol(sm1_bc)-1)] - sm1_bc[,ncol(sm1_bc)]

yepg1_bc<-yepg1
yepg1_bc[,2:(ncol(yepg1_bc)-1)] = yepg1_bc[,2:(ncol(yepg1_bc)-1)] - yepg1_bc[,ncol(yepg1_bc)]



#melting
ypd1_melt <- melt(ypd1_bc, id.vars = "Time")
head(ypd1_melt)

ypdx1_melt <- melt(ypdx1_bc, id.vars = "Time")

sm1_melt <- melt(sm1_bc, id.vars = "Time")

yepg1_melt <- melt(yepg1_bc, id.vars = "Time")



####ggplotting####
#save formatting as a theme
mytheme<-theme(axis.text.x =element_text(size=12),
               axis.text.y =element_text(size=12),
               axis.title =element_text(size=16),
               axis.line = element_line(size=1.2),
               axis.ticks = element_line(size=1.2),
               legend.position = "none")

#####functionise plotting#####
ggplotify.bc<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=4) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 56), breaks = seq(0, 56, by = 8)) +
    scale_y_continuous(limits = c(0,1.2), breaks = seq(0, 1.2, by = 0.4))}

ggplotify.bcypdx<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=4) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 56), breaks = seq(0, 56, by = 8)) +
    scale_y_continuous(limits = c(0,1.6), breaks = seq(0, 1.6, by = 0.4))}


#plotting
ypd1_plot<-ggplotify.bc(ypd1_melt)
ypd1_plot

ypdx1_plot<-ggplotify.bcypdx(ypdx1_melt)
ypdx1_plot
##View(ypdx1_bc)


##GGLPLOT FUNCTON FOR DIFFERENT SM AND YEPG SCALE
ggplotify.bcsm<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=4) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 56), breaks = seq(0, 56, by = 8)) +
    scale_y_continuous(limits = c(0,1.0), breaks = seq(0, 1.0, by = 0.2))}

ggplotify.bcyepg<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point() + 
    geom_line() +
    xlab("Hours") + 
    ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=4) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 56), breaks = seq(0, 56, by = 8)) +
    scale_y_continuous(limits = c(0,0.8), breaks = seq(0, 0.8, by = 0.2))}


sm1_plot<-ggplotify.bcsm(sm1_melt)
sm1_plot
##View(sm1_bc)

yepg1_plot<-ggplotify.bcyepg(yepg1_melt)
yepg1_plot


#####taking out #896_yepg1 to avoid broken loop#####
View(yepg1_bc)
yepgbc_minus<-yepg1_bc %>% select(1,3:ncol(yepg1_bc))
yepgm_melt <- melt(yepgbc_minus, id.vars = "Time") #melt

View(yepgbc_minus)




#####GROWTHCURVER#### ================================================================================
plate_ypd1 <- SummarizeGrowthByPlate(ypd1, bg_correct = "blank", plot_fit = TRUE,
                                     plot_file = "plate_ypd1.pdf") 
write.xlsx(plate_ypd1, 'plate_gc_ypd1.xlsx')



plate_ypdx1 <- SummarizeGrowthByPlate(ypdx1, bg_correct = "blank", plot_fit = TRUE,
                                      plot_file = "plate_ypdx1.pdf") 
write.xlsx(plate_ypdx1, 'plate_gc_ypdx1.xlsx')



plate_sm1 <- SummarizeGrowthByPlate(sm1, bg_correct = "blank", plot_fit = TRUE,
                                    plot_file = "plate_sm1.pdf") 
write.xlsx(plate_sm1, 'plate_gc_sm1.xlsx')



plate_yepg1 <- SummarizeGrowthByPlate(yepg1, bg_correct = "blank", plot_fit = TRUE,
                                      plot_file = "plate_yepg1.pdf") 
write.xlsx(plate_yepg1, 'plate_gc_yepg1.xlsx')


#for yepg minus 896
plate_yepgmin <- SummarizeGrowthByPlate(yepgbc_minus, bg_correct = "blank", plot_fit = TRUE,
                                        plot_file = "plate_yepgminus.pdf") 
write.xlsx(plate_yepgmin, 'plate_gc_yepgmin.xlsx')



#####make models#####
models.ypd1 <- lapply(ypd1[2:ncol(ypd1)], function(x) SummarizeGrowth(ypd1$Time,x))

models.ypdx1 <- lapply(ypdx1[2:ncol(ypdx1)], function(x) SummarizeGrowth(ypdx1$Time,x))

models.sm1 <- lapply(sm1[2:ncol(sm1)], function(x) SummarizeGrowth(sm1$Time,x))

models.yepg1 <- lapply(yepg1[2:ncol(yepg1)], function(x) SummarizeGrowth(yepg1$Time,x))

#yepgminus
models.yepgm <- lapply(yepgbc_minus[2:ncol(yepgbc_minus)], function(x) SummarizeGrowth(yepgbc_minus$Time,x))

#loop to get predictions

predicted.ypd1 <-data.frame(time=ypd1$Time)
for (i in names(ypd1[2:ncol(ypd1)])){
  predicted.ypd1[[i]] <- predict(models.ypd1[[i]]$model)}

predicted.ypdx1 <-data.frame(time=ypdx1$Time)
for (i in names(ypdx1[2:ncol(ypdx1)])){
  predicted.ypdx1[[i]] <- predict(models.ypdx1[[i]]$model)}

predicted.sm1 <-data.frame(time=sm1$Time)
for (i in names(sm1[2:ncol(sm1)])){
  predicted.sm1[[i]] <- predict(models.sm1[[i]]$model)}

predicted.yepg1 <-data.frame(time=yepg1$Time)
for (i in names(yepg1[2:ncol(yepg1)])){
  predicted.yepg1[[i]] <- predict(models.yepg1[[i]]$model)}

#yepg minus
predicted.yepgm <-data.frame(time=yepgbc_minus$Time)
for (i in names(yepgbc_minus[2:ncol(yepgbc_minus)])){
  predicted.yepgm[[i]] <- predict(models.yepgm[[i]]$model)}


#melt predicted dataframe and add to observed table
mpred.ypd1 <- melt(predicted.ypd1, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_ypd1 <- left_join(ypd1_melt, mpred.ypd1, by=c("Time"="time", "variable"="Sample"))
head(obspred_ypd1)

mpred.ypdx1 <- melt(predicted.ypdx1, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_ypdx1 <- left_join(ypdx1_melt, mpred.ypdx1, by=c("Time"="time", "variable"="Sample"))

mpred.sm1 <- melt(predicted.sm1, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_sm1 <- left_join(sm1_melt, mpred.sm1, by=c("Time"="time", "variable"="Sample"))

mpred.yepg1 <- melt(predicted.yepg1, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_yepg1 <- left_join(yepg1_melt, mpred.yepg1, by=c("Time"="time", "variable"="Sample"))


#yepgminus
mpred.yepgm <- melt(predicted.yepgm, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_yepgm <- left_join(yepgm_melt, mpred.yepgm, by=c("Time"="time", "variable"="Sample"))


####ggplot growthcurver results####
ggplotify.curves <- function(g)
{ggplot(g, aes(x=Time, y=OD)) +
    geom_point(aes(y=value), alpha=0.5, color="blue") +
    geom_line(aes(y=pred.od), color="red") +
    facet_wrap(~variable, ncol=4) +
    theme_bw()}


#plotting - make sure plot is exported when done

ggplotify.curves(obspred_ypd1)

ggplotify.curves(obspred_ypdx1)

ggplotify.curves(obspred_sm1)

ggplotify.curves(obspred_yepg1)

#yepg minus 896
ggplotify.curves(obspred_yepgm)

View(ypd1)

