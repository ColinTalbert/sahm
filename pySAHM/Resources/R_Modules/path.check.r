path.check<-function(paths){
if(sum(file.access(paths),mode=0)!=0){
                         temp<-as.vector(file.access(paths))==-1
                         temp.paths<-paths[temp]
                  stop("the following geotiff(s) are missing:",
                      "\nif these are intentionally left blank, uncheck makeBinMap and makeProbabilityMap options\n",
                        paste(paths[temp],collapse="\n"),sep="")
                          }
          }