 read.ma <- function(out){

          ma.name <- out$input$ma.name

      tif.dir <- out$dat$tif.dir$dname
      out.list <- out$dat$ma
      out.list$status[1] <- file.access(ma.name,mode=0)==0
      if(!is.null(out$input$tif.dir)){
          ma <- try(read.csv(ma.name, header=TRUE),silent=T)}

      if(is.null(out$input$tif.dir)){
          try(ma<-read.csv(ma.name,skip=3),silent=T)
          hl<-readLines(ma.name,1)
          hl=strsplit(hl,',')
          colnames(ma) = hl[[1]]

          tif.info<-readLines(ma.name,3)
          tif.info<-strsplit(tif.info,',')
          include<-(as.numeric(tif.info[[2]]))
          paths<-as.character(tif.info[[3]])
          #paths<-paths[!is.na(include)]
          #include[is.na(include)]<-0

            }
      if(class(ma)=="try-error"){
          out$ec <- out$ec+1
          out$error.mssg[[out$ec]] <- paste("ERROR: model array",ma.name,"is not readable")
          return(out)
          } else {
          out.list$status[2]<-T
          }


          r.name <- out$input$response.col
        # check to make sure that response column exists in the model array #

      r.col <- grep(r.name,names(ma))
      if(length(r.col)==0){
          out$ec <- out$ec+1
          out$error.mssg[[out$ec]] <- paste("ERROR: response column (",r.name,") not found in ",ma.name,sep="")
          return(out)
          }
      if(length(r.col)>1){
          out$ec <- out$ec+1
          out$error.mssg[[out$ec]] <- paste("ERROR: multiple columns in ",ma.name," match:",r.name,sep="")
          return(out)
          }

        # remove background points which aren't used here
         if(length(which(ma[,r.col]==-9999,arr.ind=TRUE))>0) ma<-ma[-c(which(ma[,r.col]==-9999,arr.ind=TRUE)),]

      # remove x and y columns #
      xy.cols <- c(match("x",tolower(names(ma))),match("y",tolower(names(ma))))
      xy.cols <- xy.cols[!is.na(xy.cols)]
      if(length(xy.cols)>0){ ma <- ma[,-xy.cols]
          if(is.null(out$input$tif.dir)){
           include<-include[-xy.cols]
           paths<-paths[-xy.cols]
      }}


       # remove weights column except for Random Forest
       if(out$input$model.source.file!="rf.r"){
       site.weights<-match("site.weights",tolower(names(ma)))
       ifelse(!is.na(site.weights),{
          out.list$train.weights<-ma[,site.weights]
          ma <- ma[,-site.weights]
           if(is.null(out$input$tif.dir)){
           include<-include[-site.weights]
           paths<-paths[-site.weights]
            }
          },
          out.list$train.weights<-rep(1,times=dim(ma)[1]))
            }
            
       #remove test training split column if present
          split.indx<-match("split",tolower(names(ma)))
          if(length(na.omit(split.indx))>0){
            include<-include[-c(split.indx)]
            split.col<-ma[,split.indx]
            ma <- ma[,-split.indx]
            out.list$ma.test<-ma[split.col=="test",]
            ma<-ma[split.col=="train",]
            out.list$test.weights<-out.list$train.weights[split.col=="test"]
            out.list$train.weights<-out.list$train.weights[split.col=="train"]
            }

           r.col <- grep(r.name,names(ma))

      # check that response column contains only 1's and 0's, but not all 1's or all 0's if GLMFamily==binomial
      if(out$input$model.source.file=="rf.r") out$input$model.family="bernoulli"
      if(tolower(out$input$model.family)=="bernoulli" || tolower(out$input$model.family)=="binomial"){
      if(any(ma[,r.col]!=1 & ma[,r.col]!=0) | sum(ma[,r.col]==1)==nrow(ma) | sum(ma[,r.col]==0)==nrow(ma)){
          out$ec <- out$ec+1
          out$error.mssg[[out$ec]] <- paste("ERROR: response column (#",r.col,") in ",ma.name," is not binary 0/1",sep="")
          return(out)
          }
           out$dat$ma$resp.name <- names(ma)[r.col]<-"response"
          out.list$n.pres[1] <- sum(ma[,r.col])
          out.list$n.abs[1] <- nrow(ma)-sum(ma[,r.col])
          out.list$resp.name <- names(ma)[r.col]
          ma.names <- names(ma)
          }



     #check that response column contains at least two unique values for counts

      if(tolower(out$input$model.family)=="poisson"){
      if(length(table(unique(ma[,r.col])))==1){
          out$ec <- out$ec+1
          out$error.mssg[[out$ec]] <- paste("ERROR: response column (#",r.col,") in ",ma.name," does not have at least two unique values",sep="")
          return(out)
          }
          out$dat$ma$resp.name <- names(ma)[r.col]<-"response"
          out.list$n.pres[1] <- sum(ma[,r.col])
          out.list$n.abs[1] <- sum(ma[,r.col]==0)
          out.list$resp.name <- names(ma)[r.col]
          ma.names <- names(ma)
          }

      # identify factors (this will eventually be derived from image metadata) #

      factor.cols <- grep("categorical",names(ma))
      factor.cols <- factor.cols[!is.na(factor.cols)]
      if(length(factor.cols)==0){
          out.list$factor.levels <- NA
          } else {
          names(ma) <- ma.names <-  sub("_categorical","",ma.names)
          factor.names <- ma.names[factor.cols]
          factor.levels <- list()
          for (i in 1:length(factor.cols)){


              f.col <- factor.cols[i]

                  x <- table(ma[,f.col])
                  if(nrow(x)<2){
                        out$dat$bad.factor.cols <- c(out$dat$bad.factor.cols,factor.names[i])
                        }
                  lc.levs <-  as.numeric(row.names(x))[x>0] # make sure there is at least one "available" observation at each level
                  lc.levs <- data.frame(number=lc.levs,class=lc.levs)
                  factor.levels[[i]] <- lc.levs

              ma[,f.col] <- factor(ma[,f.col],levels=lc.levs$number,labels=lc.levs$class)


              }

              names(factor.levels)<-factor.names
              out.list$factor.levels <- factor.levels

          }

      #out.list$ma <- ma[,c(r.col,c(1:ncol(ma))[-r.col])]

      # if producing geotiff output, check to make sure geotiffs are available for each column of the model array #
        if(out$input$make.binary.tif==T | out$input$make.p.tif==T | out$input$save.model==TRUE){
               # test that geotiffs match ma.columns
          if(is.null(out$input$tif.dir)){
              ma.cols <- match(ma.names[-r.col],sub(".tif","",basename(paths[-r.col])))
                if(any(is.na(ma.cols))){
                  out$ec <- out$ec+1
                  out$error.mssg[[out$ec]] <- paste("ERROR: the following geotiff(s) are missing in ",
                        tif.dir,":  ",paste(ma.names[-r.col][is.na(ma.cols)],collapse=" ,"),sep="")
                  return(out)
                }
                 #remove columns that shouldn't be used from tiff based on the indicator
                include<-include[-r.col]
                paths<-paths[-r.col]
                paths<-paths[include==1]
                 #creates a list of predictors from tif.ind and response column
               ma.use <- c(r.col,match(sub(".tif","",basename(paths)),ma.names))
                ma<-ma[,ma.use]
                ma.names<-names(ma)
                #Now check that tiffs to be used exist
              #out$dat$tif.names <- tif.names[ma.cols]

              if(sum(file.access(paths),mode=0)!=0){
                  out$ec <- out$ec+1
                  out$error.mssg[[out$ec]] <- paste("ERROR: the following geotiff(s) are missing : ",
                        paths[(file.access(paths)!=0),][1],sep="")
                return(out)
                }
                out$dat$tif.ind<-paths
                }
          if(!is.null(out$input$tif.dir)){
              tif.names <- out$dat$tif.names
              ma.cols <- match(ma.names[-r.col],sub(".tif","",basename(tif.names)))
              if(any(is.na(ma.cols))){
                  out$ec <- out$ec+1
                  out$error.mssg[[out$ec]] <- paste("ERROR: the following geotiff(s) are missing in ",
                        tif.dir,":  ",paste(ma.names[-r.col][is.na(ma.cols)],collapse=" ,"),sep="")
                return(out)
                }
            out$dat$tif.names <- tif.names[ma.cols]
            }} else out$dat$tif.names <- ma.names[-1]

      out.list$ma <- ma[complete.cases(ma),c(r.col,c(1:ncol(ma))[-r.col])]

      if(!is.null(out.list$ma.test)){
        out.list$ma.test<-out.list$ma.test[complete.cases(out.list$ma.test),c(r.col,c(1:ncol(out.list$ma.test))[-r.col])]
        if(out$input$model.source.file!="rf.r") out.list$test.weights<- out.list$test.weights[complete.cases(out.list$ma.test)]
        out.list$n.pres[4] <- sum(out.list$ma.test[,r.col])
        out.list$n.abs[4] <- nrow(out.list$ma.test)-sum(out.list$ma.test[,r.col])
        }
      if(out$input$model.source.file!="rf.r") out.list$train.weights <- out.list$train.weights[complete.cases(ma)]
      if(!is.null(out$dat$bad.factor.cols)) out.list$ma <- out.list$ma[,-match(out$dat$bad.factor.cols,names(out.list$ma))]


        out.list$dims <- dim(out.list$ma)
        out.list$ratio <- min(sum(out$input$model.fitting.subset)/out.list$dims[1],1)
        out.list$n.pres[2] <- sum(out.list$ma[,1])
        out.list$n.abs[2] <- nrow(out.list$ma)-sum(out.list$ma[,1])
        out.list$used.covs <- names(out.list$ma)[-1]

      if(!is.null(out$input$model.fitting.subset)){
            pres.sample <- sample(c(1:nrow(out.list$ma))[out.list$ma[,1]==1],min(out.list$n.pres[2],out$input$model.fitting.subset[1]))
            abs.sample <- sample(c(1:nrow(out.list$ma))[out.list$ma[,1]==0],min(out.list$n.abs[2],out$input$model.fitting.subset[2]))
            out.list$ma.subset <- out.list$ma[c(pres.sample,abs.sample),]
            if(out$input$model.source.file!="rf.r") out.list$weight.subset<-out.list$train.weights[c(pres.sample,abs.sample)]
            out.list$n.pres[3] <- length(pres.sample)
            out.list$n.abs[3] <- length(abs.sample)
            } else {
            out.list$ma.subset <- NULL
            out.list$weight.subset<-NULL
            out.list$n.pres[3] <- NA
            out.list$n.abs[3] <- NA }

if(tolower(out$input$model.family)=="poisson"){
out.list$ma.subset<-out.list$ma
}


          out$dat$ma <- out.list

      return(out)
      }