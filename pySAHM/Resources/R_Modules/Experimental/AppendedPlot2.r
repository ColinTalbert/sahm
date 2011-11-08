jpeg(file=paste(parent,"AcrossModelPerform.jpg",sep="//"),width=1000,height=1000,pointsize=13)
                                      par(mfrow=c(5,1),mar=c(.2, 5, .6, 2),cex=1.1,oma=c(5, 0, 3, 0))
                                        temp<-Train.x[,2:ncol(Train.x)]
                                      temp<-matrix(data=as.numeric(as.matrix(temp,nrow=Par.Len)),nrow=Parm.Len,ncol=(ncol(Train.x)-1))
                                      temp1<-temp
                                      temp[2,]<-temp[2,]/100
                                      temp[3,]<-temp[3,]/100
                                      temp2<-Test.x[,2:ncol(Test.x)]
                                      temp2<-matrix(data=as.numeric(as.matrix(temp2,nrow=Par.Len)),nrow=Parm.Len,ncol=(ncol(Test.x)-1))
                                      temp2[2,]<-temp2[2,]/100
                                      temp2[3,]<-temp2[3,]/100
                                       ss<-seq(from=1,to=(ncol(Train.x)-1),by=1)
                                       x.labs<-sub(" ","\n",Train.x[,1])
                                       x.labs<-sub("Percent","Proportion",x.labs)
                                      colors.test=c("chocolate3","gold1","darkolivegreen2","steelblue1","brown3")
                                    colors.train=c("chocolate4","gold3","darkolivegreen4","steelblue4","brown4")
                               #Plot 1.
                                    plot(c(0,(ncol(Test.x)+2)),c(0,1.1),type="n",xaxt="n",
                                        xlab=paste("Corresponding Column in ",ifelse(!is.null(out$dat$ma$ma.test),"AppendedOutputTestTrain.csv","AppendedOutput.csv"),sep=""),
                                        ylab=x.labs[1])
                                        grid(nx=10)
                                        legend(ncol(Test.x)+1,y=.75,legend=c("Test","Train"),fill=c(colors.test[1],colors.train[1]))
                                      rect(xleft=ss-.4,ybottom=0,xright=ss,ytop=temp[1,],col=colors.train[1],lwd=2)
                                      rect(xleft=ss,ybottom=0,xright=(ss+.4),ytop=temp2[1,],col=colors.test[1],lwd=2)
                                      text((which(temp1[1,]==max(temp1[1,],na.rm=TRUE),arr.ind=TRUE)-.25),
                                          max(temp1[1,],na.rm=TRUE)+.05,labels=as.character(round(max(temp1[1,]),digits=2)),cex=.8)
                                      text((which(temp2[1,]==max(temp2[1,],na.rm=TRUE),arr.ind=TRUE)+.25),
                                          max(temp2[1,],na.rm=TRUE)+.05,labels=as.character(round(max(temp2[1,]),digits=2)),cex=.8)
                                    par(mar=c(.2, 5, .6, 2))
                             #Plot 2-4.
                                    for(i in 2:4){
                                        plot(c(0,(ncol(Test.x)+2)),c(0,1.1),type="n",xaxt="n",
                                        ylab=x.labs[i])
                                        grid(nx=10)
                                      rect(xleft=ss-.4,ybottom=0,xright=ss,ytop=temp[i,],col=colors.train[i],lwd=2)
                                      rect(xleft=ss,ybottom=0,xright=(ss+.4),ytop=temp2[i,],col=colors.test[i],lwd=2)
                                      legend(ncol(Test.x)+1,y=.75,legend=c("Test","Train"),fill=c(colors.test[i],colors.train[i]))
                                      text((which(temp[i,]==max(temp[i,],na.rm=TRUE),arr.ind=TRUE)-.25),
                                          max(temp[i,],na.rm=TRUE)+.05,labels=as.character(round(max(temp[i,]),digits=2)),cex=.8)
                                      text((which(temp2[i,]==max(temp2[i,],na.rm=TRUE),arr.ind=TRUE)+.25),
                                          max(temp2[i,],na.rm=TRUE)+.05,labels=as.character(round(max(temp2[i,]),digits=2)),cex=.8)
                                      par(mar=c(.3, 5, .4, 2))
                                      }
                                      par(mar=c(2, 5, .4, 2))
                            #Plot 5.
                                   i=5
                                  plot(c(0,(ncol(Test.x)+2)),c(0,1.1),type="n",xaxt="n",
                                        xlab="n",
                                        ylab=x.labs[i])
                                        grid(nx=10)
                                      rect(xleft=ss-.4,ybottom=0,xright=ss,ytop=temp[i,],col=colors.train[i],lwd=2)
                                      rect(xleft=ss,ybottom=0,xright=(ss+.4),ytop=temp2[i,],col=colors.test[i],lwd=2)
                                      legend(ncol(Test.x)+1,y=.75,legend=c("Test","Train"),fill=c(colors.test[i],colors.train[i]))
                                      text((which(temp1[i,]==max(temp1[i,],na.rm=TRUE),arr.ind=TRUE)-.25),
                                          max(temp1[i,],na.rm=TRUE)+.05,labels=as.character(round(max(temp1[i,]),digits=2)),cex=.8)
                                      text((which(temp2[i,]==max(temp2[i,],na.rm=TRUE),arr.ind=TRUE)+.25),
                                          max(temp2[i,],na.rm=TRUE)+.05,labels=as.character(round(max(temp2[i,]),digits=2)),cex=.8)
                         #Outer margin labels
                                        for(i in 2:ncol(Orig.Header)) mtext(Orig.Header[1,i],line=-12,at=(i-1),las=2)
                                      mtext("Evaluation Metrics Performance Across Model Runs",outer=TRUE,side=3,cex=1.3)
                                      mtext(paste("Folder Name where model is found in ",
                                        ifelse(!is.null(out$dat$ma$ma.test),"AppendedOutputTestTrain.csv","AppendedOutput.csv"),sep=""),side=1,outer=TRUE,line=3)
                                   dev.off()
