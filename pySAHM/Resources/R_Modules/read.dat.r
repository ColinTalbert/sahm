read.dat<-function(input.file,hl=NULL,include=NULL,response.col,is.inspect=FALSE,pres=TRUE,absn=TRUE,bgd=TRUE,model="null"){
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
          #maxent uses for R evaluation metrics only if -9998
          if(model=="maxent") dat<-dat[-c(which(dat[,match(tolower(response.col),tolower(names(dat)))]==-9999),arr.ind=TRUE),] 
          response<-dat[,match(tolower(response.col),tolower(names(dat)))]
          dat<-dat[order(response),]
          response<-response[order(response)]
           dat[dat==-9999]<-NA
             
          if(is.inspect){ #for predictor inspector and pairs explore take some additional steps
                 #remove testing split ROWS
                 if(!is.na(match("EvalSplit",names(dat)))) {
                      response<-response[-c(which(dat$EvalSplit=="test"),arr.ind=TRUE)]
                      dat<-dat[-c(which(dat$EvalSplit=="test"),arr.ind=TRUE),]  
                  }
                 if(!is.na(match("Split",names(dat)))){
                     response<-response[-c(which(dat$Split=="test"),arr.ind=TRUE)] 
                     dat<-dat[-c(which(dat$Split=="test"),arr.ind=TRUE),]
                 }
                   #for the purpose of the pairs plot, taking all counts greater than 1 and setting them equal to presence
                   #this is never exported the true responses are also used so we have to distinguish
                  if(response.col=="responseCount") {
                        TrueResponse<-response
                        response[response>=1]<-1
                        famly="poisson"
                  } else {famly="binomial"
                          TrueResponse<-response
                          }
                          TrueResponse[TrueResponse%in%c(-9999,-9998)]<-0
                      assign("famly",famly,envir=parent.frame())
                  if(any(response==-9998)) {
                       response[response==-9998]<-0
                         assign("abs.lab","Avail",envir=parent.frame())
                         assign("pres.lab","Used",envir=parent.frame())
                       } else {
                          assign("abs.lab","Abs",envir=parent.frame())
                          assign("pres.lab","Pres",envir=parent.frame())
                       }
                 
                 #remove any of pres absn or bgd that aren't desired
                 temp<-c(0,1,-9999)
                 temp<-temp[c(absn,pres,bgd)]
                 dat<-dat[response%in%temp,]
                 TrueResponse<-TrueResponse[response%in%temp]
                 response<-response[response%in%temp]
                 
                
                 assign("TrueResponse",TrueResponse,envir=parent.frame())     
           }
            assign("response",response,envir=parent.frame())
            assign("dat",dat,envir=parent.frame())
 return()
}
