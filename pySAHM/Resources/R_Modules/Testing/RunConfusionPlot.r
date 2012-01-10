par(mfrow=c(2,2))
 barplot3d(100*c(8,7,
                 6,5,
                 4,3,
                 2,1),transp="f9", rows=4, theta = 45, phi = 25, expand=.5,
       bar.size=150,bar.space=60,
    col.lab=c("Absence","Presence"), row.lab=c("Presence Train","Presence Test","Absence Train","Absence Test"), z.lab="Confusion Matrix")


barplot3d(c(Stats$train$Cmx[2,1],Stats$train$Cmx[1,1],Stats$train$Cmx[2,2],Stats$train$Cmx[1,2]),transp="f9", rows=2, theta = 40, phi = 25, expand=.5, bar.size=15,bar.space=6,
    col.lab=c("Absence","Presence"), row.lab=c("Absence","Presence"), z.lab="Confusion Matrix")
    
    barplot3d <- function(heights, rows, transp="f0", theta=25, phi=25, bar.size=3, bar.space=0.5,
    col.lab=NULL, row.lab=NULL, z.lab=NULL, col.bar=c("#44ff58","#5844ff","#ff5844"), grid="white", ...)
    
     colors= rgb(red=c(.1,.7),blue=rep(1,times=3),green=rep(1,times=3), alpha=.5)
     colors= rgb(red=seq(from=0,to=1,length=10),blue=rep(0,times=10),green=rep(0,times=10), alpha=.5)
     colors=c(rgb(red=1,blue=0,green=0,alpha=1),rgb(red=1,blue=0,green=0,alpha=.5)