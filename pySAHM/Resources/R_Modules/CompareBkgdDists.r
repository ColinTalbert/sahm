CompareBkgdDists<-function(file.list,num.reps,num.pts){
    #Written by Marian Talbert 4/11/2012
    #This function takes list of MDS files generated using different background point generation surfaces and splits them randomply into smaller samples
    #overlays histograms of the distributions for each predictor and produces boxplots of the distribution of the means of each set and their variances
    #so that the user can determine when the distribution for each predictor has stabilized and specify the proper number of background points and 
    #compare the distribution of different predictors for different point generations surfaces.  The output is a very long pdf. 
    #num.reps should be the number of times to subsample the data in order to produce the histograms
    #num.pts is the number of points that should be subsampled for each rep in a vector format ex. 
    #num.pts<-c(500,1000,2000,4000,8000,16000,32000)
    #Command prompt junk is just junk right now.  I'll have to experiment with how to pass a numeric vector
    #and a list of mds files if it's ever decided that anyone wants to use this function

        last.dir<-strsplit(file.list[1],split="\\\\")
         parent<-sub(paste("\\\\",last.dir[[1]][length(last.dir[[1]])],sep=""),"",file.list[1])
         parent<-sub(paste("\\\\",last.dir[[1]][length(last.dir[[1]])-1],sep=""),"",parent)
          parent<-sub(paste("\\\\",last.dir[[1]][length(last.dir[[1]])-2],sep=""),"",parent)
        
        Hist.list<-Var.list<-Mean.list<-mins<-maxes<-max.counts<-Hist.List<-Var.List<-Mean.List<-Min.List<-Max.List<-Max.Counts<-list()
        
        #defining several helper functions
        range.f<-function(a,b) range(a[,b])
        extract<-function(a) do.call("rbind",a)
        box.p<-function(a,b) boxplot(a[,b],add=TRUE)
        boxplot.fct<-function(lst,j){
            a<-lapply(lst,extract)
            rng<-range(unlist(lapply(a,range.f,j)))
            plot(c(0,length(lst)+1),rng,type="n",xaxt="n",cex.axis=4,xlab="",ylab="",las=2)
            pmean<-pvar<-vector()
            for(m in 1:length(num.pts)) {boxplot(a[[m]][,j],add=TRUE,at=m,col="grey87",xaxt="n",yaxt="n",border="grey37")
               pmean<-c(pmean,mean(a[[m]][,j]))
               pvar<-c(pvar,sd(a[[m]][,j]))  
            }
               lines(loess(pmean~seq(1:7)),col="black",lwd=2)
                lines(loess(pmean+pvar/sqrt(length(num.pts))~seq(1:7)),col="red",lwd=3)
                 lines(loess(pmean-pvar/sqrt(length(num.pts))~seq(1:7)),col="red",lwd=3)
            points(seq(1:7),pmean,cex=2,bg="black",col="yellow",pch=21)
            points(seq(1:7),pmean+pvar/sqrt(length(num.pts)),cex=2,bg="red",pch=21)
            points(seq(1:7),pmean-pvar/sqrt(length(num.pts)),cex=2,bg="red",pch=21)    
          }
        f<-function(a){max(a$density)}                                 
        f2<-function(l,j){list(Breaks=l[j][[1]]$breaks,Counts=l[j][[1]]$density)}
         f3<-function(l,j){
            nB<-length(l$Breaks)
            rect(l$Breaks[-nB], 0, l$Breaks[-1L],l$Counts,col=l$Color,lwd=.01)
            }                                                                                       
        ##################################################################
        ### Now compiling some metrics from all point patterns, background defs and for all variables
        for(k in 1:length(file.list)){
                #get the names of csvs in the file.lists and set the path
            MDS.data<-read.csv(file.list[k],skip=2)
            hl<-strsplit(readLines(file.list[k],1),",")
            colnames(MDS.data)<-hl[[1]]   
            for(i in 1:length(num.pts)){
                   print(i)
                    
                    #reading each CSV and storing metrics so that I can later plot the infor
                    #for all so that I don't end up reading in and storing 10 massive files at
                    #the same time
                    hist.list<-var.list<-mean.list<-min.list<-max.list<-max.counts<-list()
                    #take the sample order randomly and then partion it 
                    samp<-seq(1:nrow(MDS.data))
                    samp<-samp[order(runif(length(samp)))]
                    for(j in 1:num.reps){
                        hist.list[[j]]<-apply(MDS.data[samp[((j-1)*num.pts[i]+1):(j*num.pts[i])],],2,hist,plot=FALSE,breaks=40)
                        var.list[[j]]<-apply(MDS.data[samp[((j-1)*num.pts[i]+1):(j*num.pts[i])],],2,var,na.rm=TRUE)
                        mean.list[[j]]<-apply(MDS.data[samp[((j-1)*num.pts[i]+1):(j*num.pts[i])],],2,mean,na.rm=TRUE)
                        min.list[[j]]<-apply(MDS.data[samp[((j-1)*num.pts[i]+1):(j*num.pts[i])],],2,min,na.rm=TRUE)
                        max.list[[j]]<-apply(MDS.data[samp[((j-1)*num.pts[i]+1):(j*num.pts[i])],],2,max,na.rm=TRUE)
                        max.counts[[j]]<-unlist(lapply(hist.list[[j]],f))
                     }
                     Hist.list[[i]]<-hist.list
                     Var.list[[i]]<-var.list
                     Mean.list[[i]]<-mean.list
                     mins[[i]]<-apply(do.call("rbind",min.list),2,min)
                     maxes[[i]]<-apply(do.call("rbind",max.list),2,max)
                     max.counts[[i]]<-apply(do.call("rbind",max.counts),2,max)
            }
           Hist.List[[k]]<-Hist.list
           Var.List[[k]]<-Var.list
           Mean.List[[k]]<-Mean.list
           Min.List[[k]]<-mins
           Max.List[[k]]<-maxes
           Max.Counts[[k]]<-max.counts
        }
        
        color.ramp<-rainbow(length(num.pts),alpha=.6,start=0,end=.75)
        xlable.list<-extract(strsplit(file.list,split="\\\\"))[,ncol(extract(strsplit(file.list,split="\\\\")))-1]
        ylabs<-num.pts
        pdf(paste(parent,"\\","Distributions3",".pdf",sep=""),pointsize=1,onefile=TRUE,colormodel="cmyk",width=30,height=20)
          par(mfrow=c(length(file.list),9),mar=c(0,0,3,0),oma=c(17,13,17,13))
              #now go through each variable
               ff<-function(l) l$Counts
        
        #for histograms we want the x axis consistent across plots
        Mins<-extract(lapply(Min.List,extract))
        Maxes<-extract(lapply(Max.List,extract))
        
        for(j in 4:ncol(MDS.data)){
            for(m in 1:length(file.list)){
                    par(mar=c(0,0,3,0))
                    for(i in 1:length(num.pts)){
                          Break<-lapply(Hist.List[[m]][[i]],f2,j=j)
                          temp<-lapply(Break,FUN=ff)
                          plot(c(min(Mins[,j]),max(Maxes[,j])),c(0,max(unlist(lapply(temp,max)))),type="n",main=names(MDS.data)[[i]][j],xlab="",ylab="",xaxt="n",yaxt="n")
                          if(i==1) mtext(xlable.list[m],side=2,cex=5,line=2)
                          #for the x reps get the histogram breaks and counts
        
                          for(k in 1:length(Break)) Break[[k]]$Color<-color.ramp[k]
                          lapply(Break,f3)
                          if(m==length(file.list)) mtext(paste(ylabs[i],"points",sep=" "),side=1,cex=5,line=5)
                    }
                     par(mar=c(0,10,3,0))
                    boxplot.fct(Var.List[[m]],j)
                    if(m==3) mtext(ylabs,side=1,at=seq(from=1,to=length(num.pts)),cex=3.5,las=2,line=2)
                    if(m==1) mtext("Variance Dist.\nas samp size increases",side=3,cex=3.5)
                    boxplot.fct(Mean.List[[m]],j)
                    if(m==1) mtext("Mean Dist\nas samp size increases",side=3,cex=3.5)
                    if(m==3) mtext(ylabs,side=1,at=seq(from=1,to=length(num.pts)),cex=3.5,las=2,line=2)
            }
            mtext(colnames(MDS.data)[j],side=3,cex=7,outer=TRUE)
        }
            dev.off()
}

# Interpret command line argurments #
# Make Function Call #
Args <- commandArgs(trailingOnly=FALSE)

    for (i in 1:length(Args)){
     if(Args[i]=="-f") ScriptPath<-Args[i+1]
     }
   
    #assign default values
   
    responseCol <- "ResponseBinary"
    pres=TRUE
    absn=TRUE
    bgd=TRUE
    #replace the defaults with passed values
    for (arg in Args) {
    	argSplit <- strsplit(arg, "=")
    	argSplit[[1]][1]
    	argSplit[[1]][2]
    	if(argSplit[[1]][1]=="p") predictor <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="i") infile <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
      if(argSplit[[1]][1]=="pres") pres <- as.logical(argSplit[[1]][2])
      if(argSplit[[1]][1]=="absn") absn <- as.logical(argSplit[[1]][2])
      if(argSplit[[1]][1]=="bgd") bgd <- as.logical(argSplit[[1]][2])
    }

ScriptPath<-dirname(ScriptPath)
source(paste(ScriptPath,"chk.libs.r",sep="\\"))
source(paste(ScriptPath,"my.panel.smooth.binary.r",sep="\\"))

CompareBkgdDists(file.list,num.reps,num.pts)
Predictor.inspection(predictor=predictor,input.file=infile,output.dir=output,response.col=responseCol,pres=TRUE,absn=TRUE,bgd=TRUE)