# some libs for this

Remove.NA <- function(
	D
){
	# drop rows with NA
	return(
		D %>% filter(rowSums(is.na(D)) == 0)	
	)
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
        
