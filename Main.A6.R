# Assign 6  - Dynamic P
# 
# change for your setting.  You can copy/paste this line and then
# source("Main.PredicR.R")
# to run 
#setwd("/Users/rout/Desktop/Dropbox/2019m6-46892-MSBA-fin/RCode/Assign6")  # Set this as you like
# Send graphics/plots to ....
Gdir <-"./Graphics"   #  Graphics printed to local dir
Aratio <- 1.5 # For my plots (1=square)


# usuals
require("ggplot2")
require("grid")
require("dplyr")
require("reshape2")
require('RColorBrewer')

# Libraries - specific to this task
require("MASS")     # for robust regression http://r-statistics.co/Robust-Regression-With-R.html
require("glmnet")   # for LASSO / ELASTIC NET

require("glmnet") 
library("broom")

# a few libs for this project
source("misc_lib.R")
source("/Users/rout/Desktop/Dropbox/R_lib/bryan_lib.R")
source("forecast_lib.R")

Verbose<-FALSE # controls some printing


# I try not to use just "red" and "blue"
some.color<-"Set1"
if (Verbose){
	display.brewer.pal(8, some.color)
}
Pallet <- brewer.pal(8, some.color)

## Load Data

# Load the data
eR.File <- "Industry49_data.csv"
eR <- read.table(eR.File,
        header=TRUE,
        stringsAsFactors = FALSE,  # make strings real strings
        sep=",",
        #nrows=400  # to load part of file
)
R<-data.frame(eR)   

P.File<- "PredictData.Larger.csv"
P <- read.table(P.File,
        header=TRUE,
        stringsAsFactors = FALSE,  # make strings real strings
        sep=",",
        #nrows=400  # to load part of file
)

P<-data.frame(P) 
P$DPratio <- (1/P$PriceDividendRatio)
P$CONSTANT <- 0   # needed when you use just one thingy

# lots of ways to join data in R; this works.  The "by" list is long so I dont get dups of "T"
Data<-full_join(eR, P, by = c("year","month","T"))
# "T" turns out to be a bad name for a variable (means TRUE in R)
colnames(Data)[colnames(Data)=="T"]<-"Time"

# There are a few NA's that are bugging me.  Swap to MEANS for now
Data <- Data %>% filter(Time<2019)
Data <- Data %>% mutate_all(list(~ifelse(is.na(.), mean(.,na.rm=TRUE), .)))

if (Verbose){
	print(head(Data[,c("Time","eR_Market","DPratio","PriceDividendRatio")]))
	print(tail(Data[,c("Time","eR_Market","DPratio","PriceDividendRatio")]))
}
# PREDICT SOMTHING
horizon <- 12 # 
A<- cumsum(Data[,grepl("eR_",colnames(Data))])  # note excess returns

R.h<-apply(A,2,function(x){(lead(x,n=horizon) - x )/horizon})
colnames(R.h)<-gsub("eR_","R.tplush.",colnames(R.h))
colnames(R.h)<-gsub("I_","",colnames(R.h))
if (Verbose){
	print(apply(R.h,2,mean,na.rm=TRUE)*12)
	print(apply(R.h,2,sd,na.rm=TRUE)*12)
}
Data<-cbind(Data,R.h)

# Forecast ahead 12 month VOL
ER.h<- apply(R.h,2,mean,na.rm=TRUE)
ER.h<- data.frame(matrix(rep(ER.h,each=nrow(Data)),nrow=nrow(Data))) # I HATE R!!
Vol<-data.frame(cumsum(as.data.frame(Data[,grepl("eR_",colnames(Data))]^2)))
Vol.h<-data.frame(apply(Vol,2,function(x){(lead(x,n=horizon) - x )/horizon}))
Vol.h<- Vol.h - ER.h^2
Vol.h<-data.frame(sqrt(Vol.h))
colnames(Vol.h)<-gsub("eR_","Vol.tplush.",colnames(Vol.h))
Vol.h.ANNUAL <- Vol.h * sqrt(12)  # VARIANCE linear in time.
Data<-cbind(Data,Vol.h)

# DO I have sharpie here? ER/Vol ought to do it

Sharpe.h<-R.h/Vol.h
colnames(Sharpe.h)<-colnames(R.h)
colnames(Sharpe.h)<-gsub("R.tplush.","Sharpe.tplush.",colnames(Sharpe.h))
Data<-cbind(Data,Sharpe.h)



if (Verbose){
	SUM<-data.frame(
	cbind(mean=t(t(colMeans(R.h,na.rm=TRUE)*12)),
		std=t(t(colMeans(Vol.h,na.rm=TRUE)*sqrt(12))),
		sharpe=t(t(colMeans(Sharpe.h,na.rm=TRUE)*sqrt(12)))
		)
	)
	colnames(SUM)<-c("mean","std","sharpe")
	print(SUM)
}


#x<-cumsum((Data[,grepl("eR_Market",colnames(Data))])^2)
#v<-(lead(x,n=horizon) - x )/horizon  -mean(Data[,grepl("eR_Market",colnames(Data))])

Use.Industries = c(
	"Fin", 
	"Telcm",  
	"Rtail", 
	"Util", 
	"Oil", 
	"Agric",
	"Autos", 
	"Whlsl",
	"Beer",
	"Toys",
	"Banks"
	 )
print(Use.Industries)


