#

Forecast <- function(
	D,       # The data must be passed.....
	use = c("CONSTANT","DPratio"),   # names
	what = c("Market"),  # names Fin, Market,... 
	LHS = "R",  # R=Returns V=Vol
	train=2007, # all up to this year
	a=1,  # 1=lasso ; 0=ridge
	Plot=TRUE,
	Interesting=c(.01,.1,.25,.5,1),
	Verbose=FALSE
){
	TRAIN <- which(D$Time<=train)
	x<-as.matrix(D[TRAIN,colnames(D)%in%c(use)])
	x.all<-as.matrix(D[,colnames(D)%in%c(use)]) 
	
	ER.predict <- data.frame()
	for (i in what){
		if (LHS == "R"){
			y<-D[TRAIN,	grepl(paste(c("R.tplush.*",i),collapse=""),colnames(D),perl=TRUE)]	
		}
		if (LHS == "V"){
			y<-D[TRAIN,	grepl(paste(c("Vol.tplush.*",i),collapse=""),colnames(D),perl=TRUE)]	
		}
					
		print(paste(c(LHS,":",i),collapse=""))
		if(Verbose){	
			print(head(x))
			print(head(y))
		}

		fit <- cv.glmnet(x=(x),y=y, family = "gaussian", alpha=a)	
		results <- tidy(fit) 
		Lambda <- results$lambda 
		hat<-predict(fit,newx=(x.all),s=Lambda)
		sigmas <- apply(hat,2,sd,na.rm=TRUE)
		if (Verbose) {
				print(sigmas*12)
		}
	
	## FILTER (could do r2 on out of sample?)
	
	keep<-sapply(quantile(sigmas,probs=Interesting,rm.na=T),function(x){which.min(abs(sigmas - x))})
	hat<-data.frame(hat[,keep])
	
	# Melt this into one long list

	colnames(hat)<-sprintf(paste("L=",'%.3e',sep=""), Lambda[keep])
	hat<-cbind(Time=D$Time,hat)	
	hat<-data.frame(hat)
	hat <- melt(
				data=hat,
				,id="Time"
				)  %>% arrange(Time)
					
	colnames(hat)<-gsub("variable","Lambda",colnames(hat))
	hat$what<-i
	hat$a<-a
	ER.predict<-rbind(ER.predict,hat)
	if(Verbose){
		print(head(ER.predict))
		print(tail(ER.predict))
	}					
	## 
	}	
	return(ER.predict)
}

Plot.vol.data<-function(
	data,
	what = c("Market"),
	highlight = c("Market"),
	h=12,
	Verbose=FALSE
){
	#HAT A DF wide
	
	data <- melt(
				data=data[,grepl("(Time|Vol.tplush)",colnames(data))],
				,id="Time"
				)  %>% arrange(Time)
	data$variable<-gsub("Vol.tplush.(I_)*","",(data$variable))	
	if(Verbose){
		print(what)
		print(unique(data$variable))		
	}	
	if(length(what)+length(highlight)>0){				
		data<- data %>% filter(variable %in% c(what,highlight))
	}		
	#	
	p<-ggplot(data=data)
	p<-p+geom_line(aes(x=Time, y=as.numeric(value)*sqrt(h),color=variable))
	if(length(highlight)>0){					
		p<-p+geom_line(
			data= data %>% arrange(Time) %>% filter(variable %in% highlight),
		aes(	
			x=Time, y=as.numeric(value)*sqrt(h),
					color=variable,
					),
					size=1.5,
					color="black"
					) 				
	}
	#p<-p+scale_color_manual(values=Pallet)
	p<-p+labs(
		color="sigma[eR_]",
		x="Time",	
		y="sigma(r) (annaul)",
		title=""
	)
	if (Verbose){	
		print(p)
	}
	return(p)
}


Plot.hat<-function(
	hat,
	h=12
){
	#HAT A DF wide
	p<-ggplot(data=
			melt(
				data=hat,
				,id="Time"
				)  %>% arrange(Time)
	)
	p<-p+geom_line(aes(x=Time, y=as.numeric(value)*h,color=variable)) 
	
	#p<-p+scale_color_manual(values=Pallet)
	p<-p+labs(
		color="E[eR_]",
		x="Time",	
		y="r (annaul)",
		title="market"
	)	
	print(p)
}

Plot.Forecast<-function(
	Ex,
	highlight=c("Market"),
	clip=c(-0.1,0.2),      # annual
	h=12,                  # scale set to sqrt(12) if plot sd
	target = "E[R| date t]",  #
	title = ""
){
	#HAT A DF long format
	
	clip <- clip * 1/h
	if (!any(is.na(clip))){
		Ex$value[which(Ex$value<clip[1])]<-clip[1]
		Ex$value[which(Ex$value>clip[2])]<-clip[2]
	}
	
	p<-ggplot(data= Ex %>% arrange(Time))

	p<-p+geom_line(aes(x=Time, y=as.numeric(value)*h,
					color=what,
					linetype=Lambda
					)) 
				
				
	if(length(highlight)>0){					
		p<-p+geom_line(
		data= Ex %>% arrange(Time) %>% filter(what %in% highlight),
		aes(	
			x=Time, y=as.numeric(value)*h,
					color=what,
					linetype=Lambda,
					),
					size=1.5,
					color="black"
					) 				
	}				
	#p<-p+scale_color_manual(values=Pallet)
	p<-p+labs(
		color="Portfolio",
		x="Time",	
		y= target,
		title=title
	)	
	p<-p+scale_linetype_manual(values=rep("solid",times=1000))
	p<-p +guides( linetype = FALSE , size = FALSE)
	if(Verbose){
		print(p)
	}
	return(p)
}


