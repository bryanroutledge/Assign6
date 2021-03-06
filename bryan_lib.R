# Handy lib that I use all the time
#
#
library(stringr)

# how to pause.  Copy or use this 
pawse <- function(
	t=NULL,
	message=NULL
){
	if(!is.null(t)){
		if(!is.null(message)){print(message)}
		Sys.sleep(t)
	}else{
		if(is.null(message)){message="Press [enter] to continue"}
		readline(prompt=message)
	}
	return()
}

clean <- function(
	x)
{
	x<-str_trim(x) # left and right
	x<-str_to_lower(x)
	x<-gsub("[^a-z0-9]","",x,perl=TRUE)
	x<-gsub("\\s+","",x,perl=TRUE)	
	return(x)
}

clean.dash <- function(
	x)
{
	x<-str_trim(x) # left and right
	x<-gsub("[^A-Za-z0-9]","-",x,perl=TRUE)
	x<-gsub("\\s+","",x,perl=TRUE)	
	x<-gsub("-+","-",x,perl=TRUE)	
	x<-gsub("^-|-$","",x,perl=TRUE)			
	return(x)
}

f.pdf<-function(
        file,
        dir="Graphics"
        )
        #  Gets you back FRED.PDF even if you gave it FRED.PDF or FRED
{
	# Graphics can be NULL or "" to get you no leading / just local
   f<-paste(c(file,".pdf"),collapse="")	
   if (!is.null(dir) ){
   	    if (dir==""){
   			# print("dir-->NULL")
   			dir<-NULL
   		}
   	}
   	if (!is.null(dir)){	
   		f<-paste(c(dir,"/",file,".pdf"),collapse="")
   } else{
   		f<-paste(c(file,".pdf"),collapse="")
   }
   f<-sub("(pdf.)+pdf", "pdf", f, ignore.case = TRUE, perl = TRUE)
   print(paste(c("TO FILE:",f),collapse=""))
   return(f)
}

print.plot<-function(
	p,
	file,
    dir="Graphics",
    height=NA,
    width=NA,
    aspect.ratio=NA, # instead of height width
    eps=FALSE
    )
{
      f<-f.pdf(file,dir=dir)
      #  https://stackoverflow.com/questions/20103212/how-to-set-the-whole-chart-to-become-wider-in-ggplot
      

	if(!is.na(aspect.ratio)){
		height <- 6
		width  <- height * aspect.ratio
	}

	if(!is.na(height)&!is.na(width)){
		pdf(file=f, height=height, width=width) 
	}
	else {
      pdf(f) 
    }  
      print(p)
      dev.off()

      
      if (eps){
      	  f<-sub("(pdf.)*pdf", "eps", f, ignore.case = TRUE, perl = TRUE)
	      postscript(f, horizontal = FALSE)
	      print(p)
	      dev.off()
	      print("EPS...")
	      print(f)
	   }

      return(f)      
}
        
	
	