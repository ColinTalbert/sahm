AppendOut<-function(compile.out,Header,x,out,Parm.Len,parent,split.type){
    Header.Length<-nrow(Header)

################ Writing to the csv  ############################

 input<-read.table(compile.out,fill=TRUE,sep=",")
  if(ncol(input)<=2){ #if very first time through little

          write.table(Header,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")
          write.table(x,file=compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE,sep=",")
          output<-matrix(0,0,0)
  } else { #this else (not the first time through) read current csv first

          output<-cbind(input[,(1:(ncol(input)-1))],c(Header[,2],as.character(x[,2])))
              temp=try(write.table(output,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=","),silent=TRUE)
           while(class(temp)=="try-error"){
                      modalDialog("","Please Close the AppendedOutput.exe\ so that R can write to it then press ok to continue ","")
                      temp<-try(write.table(output,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=","),silent=TRUE)
                      }
          }

  ###################### Making the jpg image ###################
  if(ncol(output)>2){

                  jpeg(file=paste(parent,paste("AcrossModel",
                       switch(out$dat$split.type,"crossValidation"="CrossVal","test"="TestTrain","none"="NoSplit"),
                       switch(out$input$model.family,"binomial"="Binom","bernoulli"="Binom","poisson"="Count"),
                       ".jpg",sep=""),sep="\\"),width=(1000+30*ncol(output)),height=1000,pointsize=13,quality=100)
                  par(mfrow=c(Parm.Len,1),mar=c(.2, 5, .6, 2),cex=1.1,oma=c(5, 0, 3, 0))
               #Getting rid of the header
                            row.nms<-as.character(output[(nrow(Header)+3):((nrow(Header)+2)+Parm.Len),1])
                      Hdr<-unlist(strsplit(readLines(compile.out,1),split=","))
                      output<-output[(nrow(Header)+1):nrow(output),2:ncol(output)]
               #Setting up train as numeric
                      train<-output[3:(Parm.Len+2),]
                      train<-matrix(data=as.numeric(as.character(as.matrix(train))),nrow=nrow(train),ncol=ncol(train))
                       row.names(train)<-row.nms
                       train[grep("Percent",rownames(train),ignore.case=TRUE),]<-train[grep("Percent",rownames(train),ignore.case=TRUE),]/100
                #Setting up test as numeric
                 if(split.type!="none"){
                      test<-output[(Parm.Len+3):nrow(output),]
                      test<-test[-c(seq(from=1,to=nrow(test),by=Parm.Len+2),seq(from=2,to=nrow(test),by=Parm.Len+2)),]
                      test<-as.data.frame(matrix(data=as.numeric(as.character(as.matrix(test))),nrow=nrow(test),ncol=ncol(test)))
                          test[is.na(test)]<-0 #Switching NAs to 0 so that the plot will come up
                      test$split.inx<-rep(seq(from=1,to=nrow(test)/Parm.Len),each=Parm.Len)
                      test.lst<-split(test[,-c(ncol(test))],f=test$split.inx)
                         for(i in 1:length(test.lst)) {row.names(test.lst[[i]])<-row.nms
                          test.lst[[i]][grep("Percent",rownames(test.lst[[i]]),ignore.case=TRUE),]<-test.lst[[i]][grep("Percent",rownames(test.lst[[i]]),ignore.case=TRUE),]/100
                                }
                          } else test.lst<-list()
                     ss<-seq(from=1,to=ncol(train),by=1)
                     x.labs<-sub(" ","\n",rownames(train))
                     x.labs<-sub("Percent","Proportion",x.labs)
                    colors.test=c("chocolate3","gold1","darkolivegreen2","steelblue1","brown3")
                  colors.train=c("chocolate4","gold3","darkolivegreen4","steelblue4","brown4")
        #producing plots
                   for(i in 1:Parm.Len){
                            plot(c(.6,(ncol(train)+2)),c(0,1.1),type="n",xaxt="n",
                                xlab=paste("Corresponding Column in ",ifelse(!is.null(out$dat$ma$ma.test),"AppendedOutputTestTrain.csv","AppendedOutput.csv"),sep=""),
                                ylab=x.labs[i])
                                grid(nx=10)
                                if(split.type!="none") legend(ncol(test),y=.75,legend=c(switch(out$dat$split.type, "test"="Test","crossValidation"="CV"),"Train"),fill=c(colors.test[i],colors.train[i]))
                              if(split.type!="crossValidation") rect(xleft=ss-.4,ybottom=0,xright=ss,ytop=train[i,],col=colors.train[i],lwd=2)
                              if(split.type=="crossValidation") points(ss-.1,train[i,],col=colors.train[i],cex=4,pch=19)
                             #if test split
                             options(warn=-1)
                              if(length(test.lst)==1) rect(xleft=ss,ybottom=0,xright=(ss+.4),ytop=pmax(0,test.lst[[1]][i,]),col=colors.test[i],lwd=2)
                              if(length(test.lst)>1){
                                                      for(k in ss){
                                                      a<-vector()
                                                        for(j in 1:length(test.lst)) a<-c(a,as.numeric(test.lst[[j]][i,k]))
                                                        if(sum(a!=0)!=0) boxplot(a,add=TRUE,at=k+.2,col=colors.test[i])
                                                      }
                              }
                             options(warn=0)
                              text((which(train[i,]==max(train[i,],na.rm=TRUE),arr.ind=TRUE)-.25),
                                  max(train[i,],na.rm=TRUE)+.05,labels=as.character(round(max(train[i,]),digits=2)),cex=.8)
                              if(length(test.lst)==1) text((which(test.lst[[1]][i,]==max(test.lst[[1]][i,],na.rm=TRUE),arr.ind=TRUE)+.25),
                                  max(test.lst[[1]][i,],na.rm=TRUE)+.05,labels=as.character(round(max(test.lst[[1]][i,]),digits=2)),cex=.8)
                            if (i==1) par(mar=c(.2, 5, .6, 2))
                            if(i!=1 & i!=(Parm.Len-1)) par(mar=c(.3, 5, .4, 2))
                            if(i==(Parm.Len-1)) par(mar=c(2, 5, .4, 2))
                        }
                        #Outer margin labels
                             Line<-ifelse(Parm.Len==5,-13,-19)
                            for(i in 1:length(Hdr)) mtext(Hdr[i],line=Line,at=(i-1),las=2)
                          mtext("Evaluation Metrics Performance Across Model Runs",outer=TRUE,side=3,cex=1.3)
                          mtext(paste("sub-folder name where model is found in the folder ",parent
                            ,sep=""),side=1,outer=TRUE,line=4)
                       dev.off()

                    }
               }