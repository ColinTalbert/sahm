read.dat<-function(input.file,hl=NULL,include=NULL,response.col){
#A small function to read in a csv with three header lines and assign everythinig
#to the parent environment

#Written by Marian Talbert 6/8/2012
          if(file.access(input.file,mode=0)!=0) stop(paste("input file supplied", input.file, "does not exist",sep=" "))
          
          dat<-try(read.csv(input.file,skip=3,header=FALSE))
          if(class(dat)=="try-error") stop("Error reading MDS")
          
          if(is.null(hl)){
            hl<-readLines(input.file,1)
            hl=strsplit(hl,',')
          }
          colnames(dat) = hl[[1]]
            assign("hl",hl,envir=parent.frame())
            assign("dat",dat,envir=parent.frame())
          tif.info<-readLines(input.file,3)
          tif.info<-strsplit(tif.info,',')
             assign("tif.info",tif.info,envir=parent.frame())
          options(warn=-1)
             if(is.null(include)) {
                  include<-as.numeric(tif.info[[2]])
                  include[include!=1]<-0 
             assign("include",include,envir=parent.frame())
             }
          options(warn=1)
          response<-dat[,match(tolower(response.col),tolower(names(dat)))]
            assign("response",response,envir=parent.frame())
 return()
}
