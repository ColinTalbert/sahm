
CalcMESS<-function(tiff.entry,train.dat){
              #if anything is out of range, return it before calculating sums                           
              train.length<-nrow(train.dat)
              min.train<-train.dat[1,] #because we sorted
              max.train<-train.dat[train.length,]
              upper<-lower<-0
              if(any(tiff.entry<min.train)) lower<-100*(tiff.entry[tiff.entry<min.train]-min.train)/(max.train-min.train)
              if(any(tiff.entry>max.train)) upper<-100*(max.train-tiff.entry[tiff.entry>max.train])/(max.train-min.train)
              extreme.val<-cbind(upper,lower)[which.min((cbind(upper,lower)))]
              if(extreme.val<0) return(extreme.val)
               f<-100*apply(mapply("<",train.dat,tiff.entry),2,sum)/train.length
               f<-I(f<=50)*2*f+I(f>50)*2*(100-f)
              return(which.min(f))
}
