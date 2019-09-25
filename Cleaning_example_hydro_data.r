
## 14 Feb 2019
## Laura Allen 
## Starting cleaning some hydrology data for power analysis

##///////////////////////////////////

## clean environment

rm(list=ls())

## Packages ----
library("tidyr")
library("dplyr")

## Goatfell -----
## Import data ----

## Goatfell 01
goat1.1 <- read.csv("Z:/PA_hydrology/Goatfell/Goatfell 01/GOATFELL01 0001 20141121.csv",header = F) #import data
goat1.2 <- read.csv("Z:/PA_hydrology/Goatfell/Goatfell 01/GOATFELL01 0002 20141121.csv",header = F) 
goat1.3 <- read.csv("Z:/PA_hydrology/Goatfell/Goatfell 01/GOATFELL01 0003 20141121.csv",header = F)
head(goat1.1) ## look at first row
str(goat1.1) ## look at data structure

#combine all goatfell1 files
goat1 <- rbind(goat1.1,goat1.2,goat1.3)

# Goatfell 02
goat2.1 <- read.csv("Z:/PA_hydrology/Goatfell/Goatfell 02/GOATFELL02 0001 20141121.csv",header = F) 
goat2.2 <- read.csv("Z:/PA_hydrology/Goatfell/Goatfell 02/GOATFELL02 0002 20141121.csv",header = F) 
goat2.3 <- read.csv("Z:/PA_hydrology/Goatfell/Goatfell 02/GOATFELL02 0003 20141121.csv",header = F)
goat2 <- rbind(goat2.1,goat2.2,goat2.3) #combine goatfell 2 files


## clean data ----

# split the data into separate columns
#goatfell1
clean_goat1 <- separate(goat1, col=1, into=c("Date","Time","Water_level"), sep = ";", remove = TRUE,
         convert = FALSE, extra = "warn", fill = "warn") ## split the data into separate columns


#save the clean file - to processed data folder
write.csv(clean_goat1,"Z:/PA_PowerAnalysis/Processed_data/GOATFELL01 20141121 combined-clean.csv") 

#goatfell2
clean_goat2 <- separate(goat2, col=1, into=c("Date","Time","Water_level"), sep = ";", remove = TRUE,
                        convert = FALSE, extra = "warn", fill = "warn") ## split the data into separate columns

write.csv(clean_goat2,"Z:/PA_PowerAnalysis/Processed_data/GOATFELL02 20141121 combined-clean.csv") #


## Add site data columns ----

head(clean_goat1) ## look at first rows of the data
clean_goat1$Site <- c(rep("Goatfell",length(clean_goat1[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
clean_goat1$Logger_ID <- c(rep("1",length(clean_goat1[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)

clean_goat2$Site <- c(rep("Goatfell",length(clean_goat2[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
clean_goat2$Logger_ID <- c(rep("2",length(clean_goat2[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)
head(clean_goat2) ## have a look and see if it worked

## now these could be combined to a single goatfell file
goats <- rbind(clean_goat1,clean_goat2)
head(goats)
goats$Water_level <- as.numeric(goats$Water_level) ## need to change wter level to numeric to caluclate mean

## replace '---' with 'NA' - not needed now because automatic when changed to numeric
# na_goats <- goats %>% mutate(Water_level = replace(Water_level, which(Water_level == "---"), NA))  %>% 

## Calculate mean daily values for each site ----
## calculate the mean for each logger on each day, and rename the 'mean' column with a better name

mean_goats <- group_by(goats,Date,Logger_ID,Site) %>%
  summarise(mean(Water_level,na.rm=TRUE)) %>% rename("Mean daily water level"="mean(Water_level, na.rm = TRUE)") 

str(mean_goats)
# to match rest of data - replace '.' in data with '/' - requires fixed = TRUE else it reads "." as 'any character'
mean_goats$Date <- gsub(".", "/",mean_goats$Date, fixed = TRUE)

## save the file with the means for each logger on each date
write.csv(mean_goats,"Z:/PA_PowerAnalysis/Processed_data/Goatfell_mean_waterlevels.csv") 



##//////////////////////////////////////

####### Ben Lawers ----
rm(list=ls())

## Import data 
## site 1
site1.1 <- read.csv("Z:/PA_hydrology/Ben Lawers/Site 1/BENLAWERS1 0001 20150612.csv",header = F) #import data
site1.2 <- read.csv("Z:/PA_hydrology/Ben Lawers/Site 1/BENLAWERS1 0002 20150612.csv",header = F)  
site1.3 <- read.csv("Z:/PA_hydrology/Ben Lawers/Site 1/BENLAWERS1 0003 20150612.csv",header = F) 
site1.4 <- read.csv("Z:/PA_hydrology/Ben Lawers/Site 1/BENLAWERS1 0007 20150612.csv",header = F) 
head(site1.1) ## look at first row
str(site1.1) ## look at data structure
#combine all site 1 files
site1 <- rbind(site1.1,site1.2,site1.3,site1.4)

# Site 2
site2.1 <- read.csv("Z:/PA_hydrology/Ben Lawers/Site 2/BENLAWERS2 0001 20150612.csv",header = F) #import data
site2.2 <- read.csv("Z:/PA_hydrology/Ben Lawers/Site 2/BENLAWERS2 0002 20150612.csv",header = F)  
site2.3 <- read.csv("Z:/PA_hydrology/Ben Lawers/Site 2/BENLAWERS2 0003 20150612.csv",header = F) 
site2.4 <- read.csv("Z:/PA_hydrology/Ben Lawers/Site 2/BENLAWERS2 0007 20150612.csv",header = F) 
site2 <- rbind(site2.1,site2.2,site2.3,site2.4)

## clean data 

# split the data into separate columns
#site1
clean_site1<- separate(site1, col=1, into=c("Date","Time","Water_level"), sep = ";", remove = TRUE,
                        convert = FALSE, extra = "warn", fill = "warn") ## split the data into separate columns

#site2
clean_site2 <- separate(site2, col=1, into=c("Date","Time","Water_level"), sep = ";", remove = TRUE,
                        convert = FALSE, extra = "warn", fill = "warn") ## split the data into separate columns

## Add site data columns 

head(clean_site1) ## look at first rows of the data
clean_site1$Site <- c(rep("Ben Lawers",length(clean_site1[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
clean_site1$Logger_ID <- c(rep("1",length(clean_site1[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)

clean_site2$Site <- c(rep("Ben Lawers",length(clean_site2[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
clean_site2$Logger_ID <- c(rep("2",length(clean_site2[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)
head(clean_site2) ## have a look and see if it worked

## now these could be combined to a single  file
allsites <- rbind(clean_site1,clean_site2)
head(allsites)
allsites$Water_level <- as.numeric(allsites$Water_level) ## need to change wter level to numeric to caluclate mean

## Calculate mean daily values for each site 
## calculate the mean for each logger on each day, and rename the 'mean' column with a better name

mean_water<- group_by(allsites,Date,Logger_ID,Site) %>%
  summarise(mean(Water_level,na.rm=TRUE)) %>% rename("Mean daily water level"="mean(Water_level, na.rm = TRUE)") 

str(mean_water)
# to match rest of data - replace '.' in data with '/' - requires fixed = TRUE else it reads "." as 'any character'
mean_water$Date <- gsub(".", "/",mean_water$Date, fixed = TRUE)

## save the file with the means for each logger on each date
write.csv(mean_water,"Z:/PA_PowerAnalysis/Processed_data/Ben_Lawers_mean_waterlevels.csv")



## ///////////////

## Slamannan bog ----

# Import data
site1.1 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 1/SLAMBOGM01 0001 20141031.csv",header = F) #import data
site1.2 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 1/SLAMBOGM01 0002 20141031.csv",header = F)  
site1.3 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 1/SLAMBOGM01 0003 20141031.csv",header = F) 
site1.4 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 1/SLAMBOGM01 0007 20141031.csv",header = F) 
head(site1.1) ## look at first row
str(site1.1) ## look at data structure
#combine all site 1 files
clean_site1 <- rbind(site1.1,site1.2,site1.3,site1.4)

# Site 2
site2.1 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 2/SLAMBOGM02 0001 20141031.csv",header = F) #import data
site2.2 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 2/SLAMBOGM02 0002 20141031.csv",header = F)  
site2.3 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 2/SLAMBOGM02 0003 20141031.csv",header = F) 
site2.4 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 2/SLAMBOGM02 0007 20141031.csv",header = F) 
clean_site2 <- rbind(site2.1,site2.2,site2.3,site2.4)

# Site 2
site3.1 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 3/SLAMBOGM03 0001 20141031.csv",header = F) #import data
site3.2 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 3/SLAMBOGM03 0002 20141031.csv",header = F)  
site3.3 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 3/SLAMBOGM03 0003 20141031.csv",header = F) 
site3.4 <- read.csv("Z:/PA_hydrology/Slamannan Bog/Site 3/SLAMBOGM03 0007 20141031.csv",header = F) 
clean_site3 <- rbind(site3.1,site3.2,site3.3,site3.4) ## these are already in split columns so don't need the 'separate' bit
head(clean_site3)

clean_site1$Site <- c(rep("Slamannan Bog",length(clean_site1[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
clean_site1$Logger_ID <- c(rep("1",length(clean_site1[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)

clean_site2$Site <- c(rep("Slamannan Bog",length(clean_site2[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
clean_site2$Logger_ID <- c(rep("2",length(clean_site2[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)

clean_site3$Site <- c(rep("Slamannan Bog",length(clean_site3[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
clean_site3$Logger_ID <- c(rep("3",length(clean_site3[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)


## now these could be combined to a single  file
allsites <- rbind(clean_site1,clean_site2,clean_site3)
names(allsites) <- c("Date","Time","Water_level","Site","Logger_ID") ## change the column headings
allsites$Water_level <- as.numeric(as.character(allsites$Water_level)) ## need to change wter level to numeric to caluclate mean

str(allsites)
head(allsites)


## Calculate mean daily values for each site 
mean_water<- group_by(allsites,Date,Logger_ID,Site) %>%
  summarise(mean(Water_level,na.rm=TRUE)) %>% rename("Mean daily water level"="mean(Water_level, na.rm = TRUE)") 

str(mean_water)
# to match rest of data - replace '.' in data with '/' - requires fixed = TRUE else it reads "." as 'any character'
mean_water$Date <- gsub(".", "/",mean_water$Date, fixed = TRUE)

## save the file with the means for each logger on each date
write.csv(mean_water,"Z:/PA_PowerAnalysis/Processed_data/Slamannan Bog_mean_waterlevels.csv")


## Edinglassie ----
site1 <- read.csv("Z:/PA_hydrology/Edinglassie/Site 1/EDINGLASS1 0001 20150317.csv",header = F) #import data
site2 <- read.csv("Z:/PA_hydrology/Edinglassie/Site 2/EDINGLASS2 0001 20150318.csv",header = F) #import data
site3 <- read.csv("Z:/PA_hydrology/Edinglassie/Site 3/EDINGLASS3 0001 20150318.csv",header = F) #import data


site1$Site <- c(rep("Edinglassie",length(site1[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
site1$Logger_ID <- c(rep("1",length(site1[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)

site2$Site <- c(rep("Edinglassie",length(site2[,1]))) 
site2$Logger_ID <- c(rep("2",length(site2[,1]))) 

site3$Site <- c(rep("Edinglassie",length(site3[,1]))) 
site3$Logger_ID <- c(rep("3",length(site3[,1]))) 

head(site3)

## need to separate data in first column
site3 <-  separate(site3, col=1, into=c("V1","V2","V3"), sep = ";", remove = TRUE,
                      convert = FALSE, extra = "warn", fill = "warn") ## split the data into separate columns

## now these could be combined to a single  file
allsites <- rbind(site1,site2,site3)

names(allsites) <- c("Date", "Time", "Water_level", "Site", "Logger_ID")



# to match rest of data - replace '.' in date with '/' - requires fixed = TRUE else it reads "." as 'any character'
allsites$Date <- gsub(".", "/",allsites$Date, fixed = TRUE)
allsites$Date <- as.Date(allsites$Date, "%d/%m/%Y") ## need to change wter level to numeric to caluclate mean
allsites$Water_level <- as.numeric(as.character(allsites$Water_level))
allsites$Site <- as.factor(allsites$Site)
allsites$Logger_ID <- as.factor(allsites$Logger_ID)
str(allsites)
head(allsites)

## Calculate mean daily values for each site 
mean_water<- group_by(allsites,Date,Logger_ID,Site) %>%
  summarise(mean(Water_level,na.rm=TRUE)) %>% rename("Mean daily water level"="mean(Water_level, na.rm = TRUE)") 

str(mean_water)
head(mean_water)

## save the file with the means for each logger on each date
write.csv(mean_water,"Z:/PA_PowerAnalysis/Processed_data/Edinglassie_mean_waterlevels_vers2.csv")


## Portmoak----

# Import data
site1.1 <- read.csv("Z:/PA_hydrology/Portmoak/Hydrology Data - Portmoak - 00_0001 - hourly - 20160321 to 20160709.csv",header = F) #import data
site1.2 <- read.csv("Z:/PA_hydrology/Portmoak/Hydrology Data - Portmoak - 00_0001 - hourly - 20160709 to 20161125.csv",header = F) #import data
site1.3 <- read.csv("Z:/PA_hydrology/Portmoak/Hydrology Data - Portmoak - 00_0001 - hourly - 20161124 to 20170603.csv",header = F) #import data
head(site1.1) ## look at first row
str(site1.1) ## look at data structure
#combine all site 1 files
clean_site1 <- rbind(site1.1,site1.2,site1.3)

clean_site1$Site <- c(rep("Portmoak",length(clean_site1[,1]))) ## add  a new column, filling it with a vector repeating 'Goatfell' for however long the column of existign data is.
clean_site1$Logger_ID <- c(rep("1",length(clean_site1[,1]))) ## same, but for logger_ID (I assume that's what 1 and 2 are)



## need to separate data in first column
allsites <-  separate(clean_site1, col=1, into=c("Date","Time","Water_level"), sep = ";", remove = TRUE,
         convert = FALSE, extra = "warn", fill = "warn") ## split the data into separate columns

# get into correct formats
# to match rest of data - replace '.' in date with '/' - requires fixed = TRUE else it reads "." as 'any character'
allsites$Date <- gsub(".", "/",allsites$Date, fixed = TRUE)
allsites$Date <- as.Date(allsites$Date, "%d/%m/%Y") ## need to change wter level to numeric to caluclate mean
allsites$Water_level <- as.numeric(allsites$Water_level)
allsites$Site <- as.factor(allsites$Site)
allsites$Logger_ID <- as.factor(allsites$Logger_ID)
str(allsites)
head(allsites)


## Calculate mean daily values for each site 
mean_water<- group_by(allsites,Date,Logger_ID,Site) %>%
  summarise(mean(Water_level,na.rm=TRUE)) %>% rename("Mean daily water level"="mean(Water_level, na.rm = TRUE)") 

str(mean_water)

## save the file with the means for each logger on each date
write.csv(mean_water,"Z:/PA_PowerAnalysis/Processed_data/Portmoak_mean_waterlevels.csv")
