barplot3d <- function(heights, rows, transp="f0", theta=55, phi=25, bar.size=3, bar.space=3,
    col.lab=NULL, row.lab=NULL, z.lab=NULL, col.bar=c("#44ff58","#5844ff","#ff5844"), grid="white", ...) {
 #I rescued this gem from the R graphics gallery.  I'm not sure who originally wrote it but I have altered it
 #so that it is fit to join my minion army 1/3/2012 Marian K. Talbert

    # Set parameters
    cols    = length(heights)/rows
    calkdl  = (bar.size + bar.space)
    slupki  = matrix(heights, nrow=cols, ncol=rows)
    zakres  = pretty(0:ceiling(max(heights, na.rm=T)*1.1))
    odstep  = bar.space/2 + bar.size/2
    colors=colors=c(rgb(red=.8,blue=.2,green=.2,alpha=.7),rgb(red=1,blue=.3,green=.3,alpha=.8),rgb(red=.3,blue=.6,green=.3,alpha=.7),rgb(red=.1,blue=1,green=.1,alpha=.8))


    # Prepare the grid for bars
    y = x = 0
    for (i1 in (1:rows)-1) y = c(y, bar.space/2+i1*calkdl, bar.space/1.99+i1*calkdl,
        bar.space/2+bar.size+i1*calkdl, bar.space/1.99+bar.size+i1*calkdl,
        bar.space+bar.size+i1*calkdl)
    for (i1 in (1:cols)-1) x = c(x, bar.space/2+i1*calkdl, bar.space/1.99+i1*calkdl,
        bar.space/2+bar.size+i1*calkdl, bar.space/1.99+bar.size+i1*calkdl,
        bar.space+bar.size+i1*calkdl)

    if(length(heights)>4){ #we have to squeeze together train\test or train\cv groups and make the distance between pairs much greater
                          #fortunately we always have the same dimensions
    x[c(4,5)]<-(x[c(4,5)]+x[3])/2
    x[6:7]<-x[5]+c(.1,.2)
    x[c(9,10)]<-(x[c(9,10)]+x[8])/2
    x[c(8,9,10)]<-x[c(8,9,10)]+.1-(x[8]-x[7])

    #now do the same thing for the second half
    x[18:19]<-(x[18:19]+x[20])/2
    x[17:16]<-x[18]-c(.1,.2)
    x[c(15,14)]<-(x[c(15,14)]+x[13])/2
    x[c(13,14,15)]<-x[c(13,14,15)]-.1+(x[16]-x[15])
    x[12]<-x[13]-.1
    x[11:21]<-x[11:21]-.5*(x[12]-x[10])
    
    #and then for y...
    y[c(4,5)]<-(y[c(4,5)]+y[3])*2/3
    y[c(7,8)]<-y[c(7,8)]-(y[c(7,8)]-y[9])*1/3
        }

        # persp(x,y,z,theta=75,ticktype="detailed",col=colors)
    # Prepare the z matrix of bar heights
    z = matrix(nrow=length(x), ncol=length(y))
    for (i1 in (1:cols)-1)  for (i2 in (1:rows)-1) z[c(2:5)+5*i1,c(2:5)+5*i2] = 0
    for (i1 in (1:cols)-1)  for (i2 in (1:rows)-1) z[c(3:4)+5*i1,c(3:4)+5*i2] = slupki[i1+1,i2+1]

    # Prepare colors matrix
    if(length(heights)<=4){
          fill   = matrix(nrow=length(x)-1, ncol=length(y)-1)
           for (i1 in (1:rows)-1) {
        fill[6:10,c(2:5)+5*i1] = colors[i1+1]
        fill[1:5,c(2:5)+5*i1] = colors[ifelse(i1==0,i1+2,i1)]
           }
        }

        if(length(heights)>4){
           fill   = matrix(nrow=length(x)-1, ncol=length(y)-1)
          fill[1:5,1:5]=colors[4]
          fill[12:16,7:10]=colors[4]
           fill[7:10,1:5]=colors[3]
          fill[17:20,7:10]=colors[3]
          
          fill[1:5,7:10]=colors[1]
          fill[12:16,1:5]=colors[1]
           fill[7:10,7:10]=colors[2]
          fill[17:20,1:5]=colors[2]
         }

    # Prepare area for plotting
    rys = persp(x, y, matrix(nrow=length(x), ncol=length(y)), col=fill, scale=F, theta=theta,
        phi=phi, zlim = range(zakres), lphi=44, ltheta=-10, axes=F, ...,main=z.lab,xlab="Predicted",ylab="Observed")
    # Add grid lines & numbers
    grid="black"
    for (i1 in zakres) {
        lines(rbind(trans3d(0,0,i1,rys), trans3d(0, max(y),i1,rys)), lwd=1, col=grid,lty=3)
        lines(rbind(trans3d(0,max(y),i1,rys), trans3d(max(x), max(y),i1,rys)), lwd=1, col=grid,lty=3)
        text(trans3d(-(calkdl*cols)*0.04,0,i1,rys), labels=i1, adj=1, cex=0.9)
        }
    # Add ticks & text
    if(length(heights)>4){
            lines(rbind(trans3d(mean(x[8],x[9]),0,0,rys), trans3d(mean(x[8],x[9]),-(calkdl*rows)*0.06,
                0,rys)))
            lines(rbind(trans3d(mean(x[18],x[19]),0,0,rys), trans3d(mean(x[18],x[19]),-(calkdl*rows)*0.06,
                0,rys)))
             text(trans3d(mean(x[8],x[9]),-(calkdl*rows)*0.1,0,rys),
                "Train Abs.\n\nTest Abs.", adj=.8, cex=1.1,srt=(phi+15))
              text(trans3d(mean(x[18],x[19]),-(calkdl*rows)*0.1,0,rys),
                "Train Pres.\n\nTest Pres.", adj=.8, cex=1.1,srt=(phi+15))
              text(trans3d(mean(x[11:13]),-(calkdl*rows)*.3,0,rys),
                "Predicted", adj=.8, cex=1.6,srt=(phi+10))

            } else{
          for (i1 in (1:cols)-1) {
            lines(rbind(trans3d((odstep+calkdl*i1),0,0,rys), trans3d((odstep+calkdl*i1),
                -(calkdl*rows)*0.05,0,rys)))
            if (!is.null(col.lab)) text(trans3d((odstep+calkdl*i1),-(calkdl*rows)*0.1,0,rys),
                col.lab[i1+1], adj=1, cex=0.9)
            }
             text(trans3d(mean(x[11:13]),-(calkdl*rows)*.3,0,rys),
                "Predicted", adj=.8, cex=1.6,srt=(phi+10))
     }
    for (i1 in (1:rows)-1) {
        lines(rbind(trans3d(max(x),(odstep+calkdl*i1),0,rys), trans3d(max(x)+(calkdl*cols)*0.03,
            (odstep+calkdl*i1),0,rys)))
        if (!is.null(col.lab)) text(trans3d(max(x)+(calkdl*cols)*0.05,(odstep+calkdl*i1),0,rys),
            row.lab[i1+1], adj=0, cex=0.9)
        }

     text(trans3d(max(x)*1.18,y[6],0,rys),
                "Observed", adj=.7, cex=1.6,srt=(phi+25))
                
    # Plot the bars!
    par(new=T)
    persp(x, y, z, col=fill, scale=F, theta=theta, phi=phi, zlim = range(zakres),
        lphi=44, ltheta=-10, shade=0.4, axes=F, ...)

    invisible(rys)
}

