SetWeights<-function(input.file,output.file,response.col="ResponseBinary",method="KDE"){

#Description:
#This function sets weights as a potential remedial measure when autocorrelation is found in the residuals of
#the model fit based on the number of points in an area or weights can be set so that the total weight of absence points
#is equal to the weight of presence.  The KDE options should never be used on presence only data with randomly selected
#background points

#Written by Marian Talbert 12/7/2011



   #Read input data and remove any columns to be excluded
          dat.in<-read.csv(input.file,header=FALSE,as.is=TRUE)
          dat<-as.data.frame(dat.in[4:dim(dat.in)[1],])


        response<-dat[,match(tolower(response.col),tolower(names(dat)))]
        if(method="KDE") x

      #Ignoring background data that might be present in the mds

          bg.dat<-dat[response==-9999,]

          if(dim(bg.dat)[1]!=0){
            dat<-dat[-c(which(response==-9999,arr.ind=TRUE)),]
            dat.in<-dat.in[-c(which(response==-9999,arr.ind=TRUE)+3),]
            response<-response[-c(which(response==-9999,arr.ind=TRUE))]
            bg.dat$Split=""
            }
            
            if(method="EquPresAbs"){
               }
            #this splits the training set
             split.mask<-dat[,match(tolower("evalsplit"),tolower(names(dat)))]=="train"
             index<-seq(1:nrow(dat))[split.mask]
             if(stratify==TRUE){
               dat[,ncol(dat)+1]<-NA
                for(i in 1:length(names(table(response)))){
                  index.i<-index[response[split.mask]==names(table(response))[i]]
                  index.i<-index.i[order(runif(length(index.i)))]
                  dat[index.i,ncol(dat)]<-c(rep(seq(1:n.folds),each=floor(length(index.i)/n.folds)),sample(seq(1:n.folds),size=length(index.i)%%n.folds,replace=FALSE))
                }
             } else{
                index<-index[order(runif(length(index)))]
                dat[index,ncol(dat)+1]<-c(rep(seq(1:n.folds),each=floor(length(index)/n.folds)),sample(seq(1:n.folds),size=length(index)%%n.folds,replace=FALSE))
             }
             names(dat)[ncol(dat)]<-"Split"
         #inserting data must be done in 3 steps because dat.in isn't a proper dataframe in that
         #not all elements in a column are of the same type

          dat.in<-dat.in[c(1:3,rownames(dat)),] #removing rows that weren't selected for the test train split
          dat.in[4:(dim(dat.in)[1]),(dim(dat.in)[2]+1)]<-dat$Split
          dat.in[c(1,3),(dim(dat.in)[2])]<-c("Split","")
          dat.in[2,(dim(dat.in)[2])]<-1

              if(dim(bg.dat)[1]!=0) {
                names(bg.dat)<-names(dat.in)
                dat.in<-rbind(dat.in,bg.dat)}

              #write output files for R modules
             write.table(dat.in,file=output.file,row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)


    }

  #assign default values
  responseCol="ResponseBinary"
  n.folds=10
  stratify=TRUE

 #Reading in command line arguments
 Args <- commandArgs(T)
    print(Args)

    #replace the defaults with passed values
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="nf") n.folds <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="stra") stratify <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output.file <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="i") infil <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
    }
 stratify<-as.logical(stratify)
 n.folds<-as.numeric(n.folds)
	#Run the Test training split with these parameters
	CrossValidationSplit(input.file=infil,output.file=output.file,response.col=responseCol,
  n.folds=n.folds,stratify=stratify)
