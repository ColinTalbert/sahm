CalcMESS<-function(rast,train.dat){
              #if anything is out of range, return it before calculating sums                           
              browser()
              min.train<-train.dat[1,] #because we sorted
              max.train<-train.dat[length(train.dat),]
              output<-matrix(data=NA,nrow=nrow(rast),ncol=ncol(rast))
              for(k in 1:length(min.train)) output[,k]<-my.min(rast.val=rast[,k],min.train[,k],max.train[,k])
              mapply(my.min,rast,MoreArgs =list(min.train=train.dat[1,],max.train=train.dat[length(train.dat),]))
                  
              lower<-pmin(rast.val,min.train)
              upper<-lower<-0
              if(any(tiff.entry<min.train)) lower<-100*(tiff.entry[tiff.entry<min.train]-min.train)/(max.train-min.train)
              if(any(tiff.entry>max.train)) upper<-100*(max.train-tiff.entry[tiff.entry>max.train])/(max.train-min.train)
              extreme.val<-cbind(upper,lower)[which.min((cbind(upper,lower)))]
              if(extreme.val<0) return(extreme.val)
               f<-100*apply(mapply("<",train.dat,tiff.entry),2,sum)/train.length
               f<-I(f<=50)*2*f+I(f>50)*2*(100-f)
              return(which.min(f))
}
my.min<-function(rast.val,min.train,max.train){
browser()
pmin((rast.val-min.train),(max.train-rast.val))/(max.train-min.train)*100
}