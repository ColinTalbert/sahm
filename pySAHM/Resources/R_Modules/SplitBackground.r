SplitBackground<-function(out,dat){
#this splits the background points but only if we're using pseudoabs data otherwise the split
#is into one group so the list structure can persist making the downstream coding easier
    
        if(out$input$PsdoAbs & floor(table(dat$response)[1]/table(dat$response)[2])>1){
                num.splits<-floor(sum(dat$response==0)/sum(dat$response==1))
                     #partition the pseudoabsences as evenly as possible to match the number of presence
                 Split<-rep(seq(1,num.splits,by=1),length=out$dat$nPresAbs$train[1])
                     #this randomly permutes the split membership and ensures it is the same length as the number of absences
                 Split<-Split[order(runif(table(dat$response)[1], 1, 100))]
       } else {
                 Split<-rep(1,times=sum(table(dat$response)==0))
                 num.splits<-1
       }
       assign("Split",Split,envir=parent.frame())
       assign("num.splits",num.splits,envir=parent.frame())
}