 CalcMESS<-function(tiff.entry,pred.vect){
              f<-sum(pred.vect<tiff.entry)/length(pred.vect)*100
              if(is.na(f)) return(NA)
              if(f==0) return((tiff.entry-min(pred.vect))/(max(pred.vect)-min(pred.vect))*100)
              if(0<f & f<=50) return(2*f)
              if(50<=f & f<100) return(2*(100-f))
              if(f==100) return((max(pred.vect)-tiff.entry)/(max(pred.vect)-min(pred.vect))*100)
              else return(NA)
}
