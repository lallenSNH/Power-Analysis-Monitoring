
## Simulation using sample hydrology data

## Laura Allen

## 17 April 2019

## -------------------------------------

## Take the existing data from a couple of sites, which will represent 'control' data from a set
# of loggers. We want to look at the change in water depth over a year, and see how many loggers are needed in order to have enough power
# to detect a change of x% between restored and control. The restored data can be simulated by using the control data and adding a 
# set % change over the course of a year (so you know the actual amount of difference between the datasets that you are
# aiming to detect.)

# clear environment
rm(list=ls())

## Packages ----
library(dplyr)
library(lme4)

## 1: import data ----
Benl <- read.csv("Z:/PA_PowerAnalysis/Processed_data/BenLawers_mean_waterlevels_vers2.csv")
Carr <- read.csv("Z:/PA_PowerAnalysis/Processed_data/Carrifran_mean_waterlevels_vers2.csv")
Corr <- read.csv("Z:/PA_PowerAnalysis/Processed_data/Corrour_mean_waterlevels_vers2.csv")
Edin <- read.csv("Z:/PA_PowerAnalysis/Processed_data/Edinglassie_mean_waterlevels_vers2.csv")
Glen <- read.csv("Z:/PA_PowerAnalysis/Processed_data/Glenmullie_mean_waterlevels_vers2.csv")
Goat <- read.csv("Z:/PA_PowerAnalysis/Processed_data/Goatfell_mean_waterlevels_vers2.csv")
Nair <- read.csv("Z:/PA_PowerAnalysis/Processed_data/Nairnside_mean_waterlevels_vers2.csv")
Slam  <- read.csv("Z:/PA_PowerAnalysis/Processed_data/Slamannan Bog_mean_waterlevels_vers2.csv")
Port <- read.csv("Z:/PA_PowerAnalysis/Processed_data/Portmoak_mean_waterlevels.csv")

## 2: combine into one dataset----
# identifying individual loggers
names(Benl) #check column names
names(Corr)


## fix dates to format yyyy-mm-dd
Carr$Date <- as.Date(Carr$Date, "%d/%m/%Y")
Corr$Date <- as.Date(Corr$Date, "%d/%m/%Y")
Glen$Date <- as.Date(Glen$Date, "%d/%m/%Y")
Nair$Date <- as.Date(Nair$Date, "%d/%m/%Y")
Slam$Date <- as.Date(Slam$Date, "%d/%m/%Y")
Benl$Date <- as.Date(Benl$Date, "%d/%m/%Y")
Edin$Date <- as.Date(Edin$Date)
Goat$Date <- as.Date(Goat$Date, "%d/%m/%Y")
Port$Date <- as.Date(Port$Date)

# select only the key columns(the matching ones)
Benl <-  select(Benl,Date,Logger_ID,Site,Mean.daily.water.level) #
Carr <-  select(Carr,Date,Logger_ID,Site,Mean.daily.water.level)
Corr <-  select(Corr,Date,Logger_ID,Site,Mean.daily.water.level)
Edin <-  select(Edin,Date,Logger_ID,Site,Mean.daily.water.level)
Glen <-  select(Glen,Date,Logger_ID,Site,Mean.daily.water.level)
Goat <-  select(Goat,Date,Logger_ID,Site,Mean.daily.water.level)
Nair <-  select(Nair,Date,Logger_ID,Site,Mean.daily.water.level)
Slam <-  select(Slam,Date,Logger_ID,Site,Mean.daily.water.level)
Port <-  select(Port,Date,Logger_ID,Site,Mean.daily.water.level)

head(Benl) # check the formats match
head(Carr)
head(Corr)
head(Edin)
head(Glen)
head(Goat)
head(Nair)
head(Slam)
head(Port)


control <- rbind(Benl,Carr,Corr,Edin,Glen,Goat,Nair,Slam,Port) %>% # join datasets into one
  droplevels() ## need to 'drop levels' because after filtering, it sometimes saves the empty levels of a factor that you deleted.
control$Log_siteID <- paste(control$Site,"log",control$Logger_ID) # create a new column combining logger number and site name (so each logger has a unique ID once sites combined)

# get rid of the negative water table values - first need to convert the factor to a character to edit the text
control$Mean.daily.water.level <- as.character(control$Mean.daily.water.level) 
control$Mean.daily.water.level <- gsub("-", "",control$Mean.daily.water.level) # then you can replace the '-' symbol

# convert each variable to the appropriate type (e.g. date, factor, number)
control$Date <- as.Date(control$Date)
control$Logger_ID <- as.factor(control$Logger_ID)
control$Site <- as.factor(control$Site)
control$Log_siteID <- as.factor(control$Log_siteID)
control$Mean.daily.water.level <- as.numeric(control$Mean.daily.water.level)
control <-  rename(control,waterlevel = Mean.daily.water.level) 
control$colours <- control$Site ## add a column to set the colour to be used for plotting each site
levels(control$colours) <- c("red","blue4","brown","deeppink","chartreuse4","darkorange","green","black","deepskyblue1","darkviolet","goldenrod")
## create a columns of colours matched to the sites to use for plotting (there's a chart you can choose from if you google R colours)

summary(control) # see a summary of the data
str(control)
head(control)

control <- control[-c(which(is.na(control$waterlevel))),] ## remove any rows where waterlevel was recorded as NA

levels(control$Site)
# plot the water levels over time ----

plot(control$waterlevel~control$Date,col="white")
head(control)
l <- 2
for(l in 1:length(levels(control$Log_siteID))){ ## for each factor level of loggersiteID
rs <- which(control$Log_siteID==levels(control$Log_siteID)[l]) ## identify which rows contain that logger ID
con <- control[rs,c(1,4,5,6)] # select the rows with the right logger
con_ord <- con[order(con$Date),] # then sort them in order of date
col1 <- as.character(con_ord$colours[1])
lines(con_ord$waterlevel~con_ord$Date,col=col1,lwd=2)
} ## then plot the line of data for that logger
legend("topright",legend=c(as.character(levels(control$Site))),fill=c(as.character(levels(control$colours))))#add a legend


## 3: create 'restored' data set - add x% change over the year ----

restored <- control # base it on the control dataset
rest <- restored[order(restored$Date),] # order it by date
rest$multiplier <- as.numeric(rest$Date) # convert dates to numbers, so we can use them to gradually increase the amount of slope over time
rest$multiplier <- rest$multiplier-16373 # use date as a number, but start from near 1, or it produces too large numbers
rest$waterlevel <- rest$waterlevel+(rest$waterlevel*(-0.001*rest$multiplier)) ## multiply the data by an amount of change over time (you can adjust this to try different scenarios)

#then plot the 'restored' data to compare
plot(rest$waterlevel~rest$Date,col="white")
for(l in 1:length(levels(rest$Log_siteID))){ ## for each factor level of loggersiteID
  rs <- which(rest$Log_siteID==levels(rest$Log_siteID)[l]) ## identify which rows contain that logger ID
  con <- rest[rs,c(1,4,5,6)] # select the rows with the right logger
  con_ord <- con[order(con$Date),] # then sort them in order of date
  col1 <- as.character(con_ord$colours[1])
  lines(con_ord$waterlevel~con_ord$Date,col=col1,lwd=2)
} ## then plot the line of data for that logger
legend("topright",legend=c(as.character(levels(control$Site))),fill=c(as.character(levels(control$colours))))#add a legend


# combine the restored and control into a dataframe ----
head(rest) # check the columns match
head(control)

rest <- rest[,1:6] # exclude the 'multiplier' column now we are done with it (so that rest and control have the same columns for joining)

rest$status <- rep("restored",length(rest$waterlevel)) # fill int he full length of the column witht he word 'restored'
control$status <- rep("control",length(rest$waterlevel))

restcont <- rbind(rest,control) #join by rows
head(restcont)
str(restcont) #check it looks ok

## 4: test model to see if you can detect the difference ----
# between the restored and unrestored over time.
# water level ~ time + as.factor(restored or control) (maybe + (1|Site)?)

# this is our mixed effect model
summary(restcont)
m1 <- lmer(waterlevel~scale(Date)*as.factor(status)+(1|Site),data=restcont,REML=F)
summary(m1) # summary output, shows size of effect of the restoration status*
## probably need an interaction between date and restoration status - effect of restoration increases over time
# rescaled date because model warned me that the scales of the predictor variables were too different (although this was confusing because there was only one continuous predictor variable)

## to see if it can detect a significant difference, used a likelihood ratio test,
# which compares if the model including restoration status is significantly betetr at explaining the pattern 
# of the data than the model without restoration status
m2 <- lmer(waterlevel~Date+(1|Site),data=restcont,REML=F) 
anova(m1,m2) ## gives a p-value of whether the more complex model is significantly better at explaining the waterlevel
#(in this case, with all the data, yes, but m1 still wasn't very good at capturing the effect size*) 
# *(although I'm not totally sure how to interpret the effect size with date and with rescaling applied...)

