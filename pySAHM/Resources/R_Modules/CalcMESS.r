 CalcMESS<-function(tiff.entry,pred.vect){
              f<-sum(pred.vect<tiff.entry)/length(pred.vect)*100
              if(is.na(f)) return(NA)
              if(f==0) return((tiff.entry-min(pred.vect))/(max(pred.vect)-min(pred.vect))*100)
              if(0<f & f<=50) return(2*f)
              if(50<=f & f<100) return(2*(100-f))
              if(f==100) return((max(pred.vect)-tiff.entry)/(max(pred.vect)-min(pred.vect))*100)
              else return(NA)
}

#a<-outer(tiff.entry,pred.vect,FUN=CalcMESS)
#Trying hard to make the Calc Mess faster I've tried a for loop instead of vectorization which is slower
#here I tried making a table of the prediction vector.  This is slower for my current data set but might be faster 
#on a bigger dataset where I don't have to pass all values of the predictor on to the mapply
#pred.tab<-table(pred.vect)
#CalcMESS<-function(tiff.entry,pred.tab,pred.vals){
#              f<-sum((pred.vals<tiff.entry)*pred.tab)/sum(pred.tab)*100
#              if(is.na(f)) return(NA)
#              if(f==0) return((tiff.entry-min(pred.vals))/(max(pred.vals)-min(pred.vals))*100)
#              if(0<f & f<=50) return(2*f)
#              if(50<=f & f<100) return(2*(100-f))
#              if(f==100) return((max(pred.vals)-tiff.entry)/(max(pred.vals)-min(pred.vals))*100)
#              else return(NA)
#}
## new try
#              start.time<-Sys.time()
#             for(k in 1:nvars.final){
#                        pred.tab<-table(train.dat[,match(vnames.final.mod[k],names(train.dat))])
#                        pred.vals<-as.numeric(names(pred.tab))
#                        tiff.entry<-temp[,match(vnames.final.mod[k],names(temp))]
#                        if(nvars.final>1) pred.rng[,k]<-mapply(CalcMESS,tiff.entry=tiff.entry,
#                                          MoreArgs=list(pred.tab=pred.tab,pred.vals=pred.vals))
#                        else pred.rng<-mapply(CalcMESS,tiff.entry=temp,MoreArgs=list(pred.vect=pred.range))
#                         }
#             Sys.time()-start.time  