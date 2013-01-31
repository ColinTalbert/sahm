path.check<-function(paths,newBasepath=NULL){
  if(!is.null(newBasepath)){
       #this bit of code should switch the paths for the tiffs in case someone has relocated their session folder
       #because people can game the system and create mds files outside of vistrails the tiff paths can't always be
       #determined based on the path to the mds file so after switching these out I check for file existence. If the tiffs 
       #aren't relative to the mds I switch them back to what they were before and hope for the best there is no way to figure out
       #the name of the old session folder from the information in the mds file that I can figure out
  
       new.paths<-file.path(file.path(newBasepath,basename(dirname(paths))),basename(paths))
       names(new.paths)<-names(paths)
       if(any(file.access(paths,mode=0)!=0))
       paths[file.access(paths,mode=0)!=0]<-new.paths[(file.access(paths,mode=0)!=0)]
  }
  if(any(file.access(paths,mode=0)!=0)){
       temp<-as.vector(file.access(paths))==-1
       temp.paths<-paths[temp]
       stop("the following geotiff(s) are missing:",
          "\nif these are intentionally left blank, uncheck makeBinMap and makeProbabilityMap options\n",
      paste(paths[temp],collapse="\n"),sep="")
  }
  return(paths)
          }