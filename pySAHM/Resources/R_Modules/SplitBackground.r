SplitBackground<-function(out){
        if(out$input$PsdoAbs & floor(out$dat$nPresAbs$train[1]/out$dat$nPresAbs$train[2])>1){
                num.splits<-floor(sum(out$dat$ma$train$dat$response==0)/sum(out$dat$ma$train$dat$response==1))
                #partition the pseudoabsences as evenly as possible to match the number of presence
                #this will always give more absence in a split than presence maybe use round instead of floor
                 Split<-c(rep(seq(from=1,to=num.splits),each=sum(out$dat$ma$train$dat$response==1)),
                  sample(1:num.splits,size=sum(out$dat$ma$train$dat$response==0)-num.splits*sum(out$dat$ma$train$dat$response==1),replace=TRUE))
                 #this randomly permutes the split membership and ensures it is the same length as the number of absences
                 Split<-sample(Split,size=length(Split),replace=FALSE)[1:min(out$dat$nPresAbs$train[1],length(Split))]
       } else {
                 Split<-rep(1,times=sum(out$dat$ma$train$dat$response==0))
                 num.splits<-1
       }
       assign("Split",Split,envir=parent.frame())
       assign("num.splits",num.splits,envir=parent.frame())
}