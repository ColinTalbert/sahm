resid.image<-function(dev.contrib,pred,raw.dat,x,y,model.type,file.name,out,label){
          browser()
          #for some reason dev.contrib is negative for binomial and bernoulli and positive for poisson
   if(label!="eval") z<-sign(pred-raw.dat)*abs(dev.contrib)
      else z<-pred-raw.dat
     MinCol<-min(z)
     MaxCol<-max(z)

              #####################################################
              a<-loess(z~x*y)
               x.lim<-rep(seq(from=min(x),to=max(x),length=100),each=100)
               y.lim<-rep(seq(from=min(y),to=max(y),length=100),times=100)
              z<-predict(a,newdata=cbind("x"=x.lim,"y"=y.lim))
              x.lim<-seq(from=min(x),to=max(x),length=100)
              y.lim<-seq(from=min(y),to=max(y),length=100)
                 z<-matrix(data=z,ncol=100,nrow=100,byrow=TRUE)

                  
                 ########### Plot residual smooth with signed and sized residuals on top
              jpeg(file=paste(out$dat$bname,"resid.plot.jpg",sep="."),width=1000,height=1000,pointsize=13)
                 par(oma=c(3,3,3,3))
                 layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
                  image(z,x=x.lim,y=y.lim,col=beachcolours(heightrange=c(min(z),max(z)),sealevel=0,ncolours=length(table(z))),
                  main=paste("Spatial pattern of", ifelse(label!="eval"," deviance residuals\n(magnitude and sign)"," prediction error")),xlab="X coordinate",ylab="Y coordinate")
                  points(x,y,bg=c("blue","red")[sign(pred-raw.dat)/2+1.5], pch=21,cex=abs(dev.contrib)/max(abs(dev.contrib))*5)
                  par(mar = c(3,2.5,2.5,2))

             colrange<-seq(from=MinCol,to=MaxCol,length=100)
               image(1,colrange,
               matrix(data=colrange, ncol=length(colrange),nrow=1),
              col=beachcolours(heightrange=c(MinCol,MaxCol),sealevel=0,ncolours=length(colrange)),
              xlab="",ylab="",
              xaxt="n")
              graphics.off()
              return(a)
              }
