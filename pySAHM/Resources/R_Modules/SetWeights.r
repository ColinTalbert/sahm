SetWeights<-function(input.file,output.file,response.col="ResponseBinary",method="Density"){

#Description:
#This function sets weights as a potential remedial measure when autocorrelation is found in the residuals of
#the model fit based on the number of points in an area or weights can be set so that the total weight of absence points
#is equal to the weight of presence.  The Density options should never be used on presence only data with randomly selected
#background points.  The problem with this functions is that there is no way that I can think of to optimize weights based on the Density
#I don't know how much near by points should be downweighted or how close constitutes near by as this would seem to depend on the species being
#modeled and it's environment

#Written by Marian Talbert 12/7/2011

   #Read input data and remove any columns to be excluded
          dat.in<-read.csv(input.file,header=FALSE,as.is=TRUE)
          dat<-as.data.frame(dat.in[4:dim(dat.in)[1],])
           names(dat)<-dat.in[1,]

        response<-dat[,match(tolower(response.col),tolower(names(dat)))]
        
        bg.dat<-dat[response==-9999,]

          if(dim(bg.dat)[1]!=0){
            dat<-dat[-c(which(response==-9999,arr.ind=TRUE)),]
            dat.in<-dat.in[-c(which(response==-9999,arr.ind=TRUE)+3),]
            response<-response[-c(which(response==-9999,arr.ind=TRUE))]
            bg.dat$site.weight=""
            }

        if(method=="Density") {
            if(is.na(match("spatstat",installed.packages()[,1]))) {
             install.packages("spatstat",repos="http://lib.stat.cmu.edu/R/CRAN")
            }

            library(spatstat)
               win<-ripras(x=as.numeric(dat$X),y=as.numeric(dat$Y))
               study.area<-ppp(x=as.numeric(dat$X),y=as.numeric(dat$Y),window=win)

              # using just density causes problems for points in low density areas
              # I've taken the completely arbitrary step of setting weights equal to 1/sqrt(den+1)
              den<-density.ppp(study.area,at="points",leaveoneout=FALSE)

            dat$site.weight<-1/sqrt(den+1)

                     }

            if(method=="PresAbs") {
            response<-as.numeric(response)
                    PresAbsTab<-table(response)
                    if(length(PresAbsTab)!=2) stop("PresAbs option is only available for binary data")
                     #sum of presence weights equals sum of absence weights and total sum of weights is the same
                     #as if weights were equal to 1
                     weight<-.5*sum(PresAbsTab)/PresAbsTab
                      match(response,names(weight))
                      dat$site.weight<-as.vector(weight[match(response,names(weight))])
            }


         #inserting data must be done in 3 steps because dat.in isn't a proper dataframe in that
         #not all elements in a column are of the same type

           dat.in[,(dim(dat.in)[2]+1)]<-NA
          dat.in[4:(dim(dat.in)[1]),(dim(dat.in)[2])]<-dat$site.weight
          dat.in[c(1,3),(dim(dat.in)[2])]<-c("site.weight","")
          dat.in[2,(dim(dat.in)[2])]<-1

              if(dim(bg.dat)[1]!=0) {
                names(bg.dat)<-names(dat.in)
                dat.in<-rbind(dat.in,bg.dat)}

              #write output files for R modules
             write.table(dat.in,file=output.file,row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)


    }

  #assign default values
  responseCol="ResponseBinary"
  method="Density"


 #Reading in command line arguments
 Args <- commandArgs(T)
    print(Args)

    #replace the defaults with passed values
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="met") method <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output.file <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="i") infil <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
    }

	#Run SetWeights with these parameters
	SetWeights(input.file=infil,output.file,response.col=responseCol,method=method)

