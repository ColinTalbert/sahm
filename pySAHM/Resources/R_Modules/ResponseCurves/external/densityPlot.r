densityPlot<-function(fitLst){

        #How to check that models and data match
        cols<-c("blue","red")
          color.box<-col2rgb(cols,alpha=TRUE)
                           color.box[4,]<-60
          temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
          cols<-apply(color.box/255,2,temp.fct)
          
        par(mfrow=c(1,ncol(dat)),mar=c(0,0,0,0),oma=c(3,5,3,0),xpd=TRUE)
        for(i in 1:ncol(dat)){
            presDens<-density(dat[resp==1,i])
            absDens<-density(dat[resp==0,i])

            plot(x=range(c(absDens$x,presDens$x)),y=c(0,max(absDens$y,presDens$y)),type="n",
            ylab="Density",xlab=names(dat)[i],yaxt="n")
            polygon(absDens,col=cols[1],border="blue")
            polygon(presDens,col=cols[2],border="red")
         }
}