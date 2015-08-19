model.fit<-function(dat,out,Model,full.fit=FALSE,pts=NULL,weight=NULL,Fold,...){

#This function was written to separate the steps involved in model fitting from the post processing steps
#needed to produce several of the later outputs
#in the generic model fit so that I can use the same simple function call for cross-validation and generic
#model fit.  Written by Marian Talbert June 30th 2012

    suppressMessages(attach(out$input))
    on.exit(detach(out$input))
#================================================================
#                        GLM
#================================================================= 
   
    if(Model=="glm") {
    
    if(PsdoAbs){
      #for glm sum of absence weights set equal to sum of presence weights
      absWt<-sum(dat$response==1)/sum(dat$response==0)
      weight[dat$response==0]<-absWt
      
     }
    mymodel.glm.step<-list()
            penalty <- if(simp.method=="AIC") 2 else
                         log(nrow(dat))

            if(!squared.terms){
                scope.glm <- list(lower=as.formula(paste("response","~1")),
                upper=as.formula(paste("response","~",paste(out$dat$used.covs,collapse='+'))))
            }else{
                factor.mask<-na.omit(match(names(out$dat$factor.levels),out$dat$used.covs))
                cont.mask<-seq(1:length(out$dat$used.covs))
                if(length(factor.mask)!=0) cont.mask<-cont.mask[-c(factor.mask)]

                 scope.glm <- list(lower=as.formula(paste("response","~1")),
                   upper=as.formula(paste("response","~",paste(c(if(length(factor.mask)>0) paste(out$dat$used.covs[factor.mask],collapse=" + "),
                   paste("(",paste(out$dat$used.covs[cont.mask],collapse=" + "),")^2",sep=""),
                   paste("I(",out$dat$used.covs[cont.mask],"^2)",sep="")),collapse=" + "),sep="")))
             }
             
             if(predSelect){
                mymodel.glm.step[[1]] <- step(glm(as.formula(paste("response","~1")),family=model.family,data=dat,weights=weight,na.action="na.exclude"),
                direction='both',scope=scope.glm,k=penalty,trace=1)
            } else {
             mymodel.glm.step[[1]] <- glm(scope.glm$upper,family=model.family,data=dat,weights=weight,na.action="na.exclude")
            }
            
            out$mods$final.mod<-mymodel.glm.step
            if(full.fit) return(out)
            else return(mymodel.glm.step)
     }
#================================================================
#                        MAXENT
#================================================================= 
     if(Model=="maxent"){
          #this is a bit confusing because the files is something.lambdas and I don't necessarilly know what something is
          if(missing(Fold)) lambdasFile=list.files(out$input$lambdas,full.names=TRUE)[grep("lambdas",list.files(out$input$lambdas))]
          else lambdasFile=list.files(file.path(out$input$lambdas,paste("cvSplit",Fold,sep="")),
               full.names=TRUE)[grep("lambdas",list.files(file.path(out$input$lambdas,paste("cvSplit",Fold,sep=""))))] 
          out$mods$final.mod[[1]]<-read.maxent(lambdasFile)
          if(full.fit) return(out)
          else return(out$mods$final.mod)
          }

#================================================================
#          Habitat Suitability Criterion
#================================================================= 
     if(Model=="udc"){
     
      out$mods$final.mod[[1]]<-read.udc(out$input$udc.file)
           return(out)
          }

    
   SplitBackground(out,dat)
   out$dat$ma$train$Split<-c(Split,rep(0,times=sum(dat$response>0)))
#================================================================
#                        MARS
#================================================================= 
    if(Model=="mars") {
          fit_contribs<-list()
          mars.model<-list()
          for(i in 1:num.splits){
                mars.model[[i]]<-earth(response~.,data=dat[c(Split,rep(i,times=sum(dat$response>0)))==i,],degree=mars.degree,penalty=mars.penalty,glm=list(family=model.family))
                
                 #
                if(full.fit) {out$mods$final.mod[[i]]<-mars.model[[i]]
                              fit_contribs[[i]] <- evimp(mars.model[[i]],trim=TRUE)
                             }
           }
           if(full.fit){
                assign("fit_contribs",fit_contribs,envir=parent.frame())
                return(out)
           } else return(mars.model)
    }
#================================================================
#                        BRT
#=================================================================
 if(Model=="brt"){
           brt.full<-list()
           lr.list<-list()          
           mod.simp<-list()
           
          if(model.family=="binomial")  out$input$model.family<-model.family<-"bernoulli"
            if(!is.null(tc)) out$mods$parms$tc.full<-out$mods$parms$tc.sub<-tc
             
           #going to try to estimate learning rate and predictors to use in final model not just on the subset but by calculating for
           #several of the splits (if the used was split)
           lr.samp<-sample(1:num.splits,size=min(num.splits,5),replace=FALSE)
           for(i in 1:length(lr.samp)){
               if(length(lr.samp)>1) {out$dat$Subset$dat<-dat[c(Split,rep(lr.samp[i],times=sum(dat$response>0)))==lr.samp[i],]
                                      out$dat$Subset$weight<-weight[c(Split,rep(lr.samp[i],times=sum(dat$response>0)))==lr.samp[i]]
                                      if(is.null(out$dat$Subset$weight))  out$dat$Subset$weight<-rep(1,times=nrow(out$dat$Subset$dat))
                                      out$dat$Subset$ratio=.5
                                      }
                lr.list[[i]]<-est.lr(out)
            }
            #now reassembling everything from lr estimation before continuing
                 out$mods$lr.mod$good.cols<-unique(unlist(lapply(lr.list,function(lst){lst$lr.mod$good.cols})))
                 out$mods$parms$tc.sub<-round(mean(unlist(lapply(lr.list,function(lst){lst$parms$tc.sub}))))
                 out$mods$parms$tc.full<-round(mean(unlist(lapply(lr.list,function(lst){lst$parms$tc.full}))))
                 out$mods$lr.mod$lr0<-mean(unlist(lapply(lr.list,function(lst){lst$lr.mod$lr0})))
                 out$mods$lr.mod$lr<-mean(unlist(lapply(lr.list,function(lst){lst$lr.mod$lr})))
               
                cat("\nfinished with learning rate estimation, lr=",out$mods$lr.mod$lr0)
                cat("\nfor final fit, lr=",out$mods$lr.mod$lr,"and tc=",out$mods$parms$tc.full,"\n")

                if(simp.method=="cross-validation"){
                    for(i in 1:length(lr.samp)){
                       if(length(lr.samp)>1) {out$dat$Subset$dat<-dat[c(Split,rep(lr.samp[i],times=sum(dat$response>0)))==lr.samp[i],]
                                          out$dat$Subset$weight<-weight[c(Split,rep(lr.samp[i],times=sum(dat$response>0)))==lr.samp[i]]
                                          out$dat$Subset$ratio=.5
                                          }
                        # remove variables with <1% relative influence and re-fit model
                            if(length(out$mods$lr.mod$good.cols)<=1) stop("BRT must have at least two independent variables")
                            max.trees<-NULL
                            #learning rate estimation removes columns with low contributions to fit for removal 
                            #here we put specify use all if no predictor selection was to occur
                            if(!predSelect) out$mods$lr.mod$good.cols<-seq(from=2,to=ncol(out$dat$Subset$dat))
                             
                           n.trees<-c(300,600,800,1000,1200,1500,1800)
                           if(!is.null(out$input$n.trees)) n.trees=out$input$n.trees
                        m0 <- gbm.step.fast(dat=out$dat$Subset$dat,gbm.x=out$mods$lr.mod$good.cols,gbm.y=1,family=model.family,
                              n.trees = n.trees,step.size=step.size,max.trees=max.trees,
                              tolerance.method=tolerance.method,tolerance=tolerance, n.folds=n.folds,prev.stratify=prev.stratify,
                              tree.complexity=out$mods$parms$tc.sub,learning.rate=out$mods$lr.mod$lr0,bag.fraction=bag.fraction,site.weights=out$dat$Subset$weight,
                              autostop=T,debug.mode=F,silent=!debug.mode,
                              plot.main=F,superfast=F)
                        if(predSelect) mod.simp[[i]] <- gbm.simplify(m0,n.folds=n.folds,plot=F,verbose=F,alpha=alpha) # this step is very slow #
                   
                     }        #if we removed bad predictors the good predictor list otherwise make sure we specify include all again
                              if(predSelect) out$mods$simp.mod$good.cols <- unique(unlist(lapply(mod.simp,function(lst){lst$pred.list[[length(lst$pred.list)]]})))
                              else out$mods$simp.mod$good.cols <- seq(from=2,to=ncol(out$dat$Subset$dat))
                              out$mods$simp.mod$good.vars <- names(dat)[out$mods$simp.mod$good.cols]
                             {cat("\n");cat("50%\n")}
                }
              
            final.mod<-list()
                   
            for(i in 1:num.splits){
                 if(out$mods$lr.mod$lr==0) out$mods$lr.mod$lr<-out$mods$lr.mod$lr0
                 n.trees<-c(300,600,800,1000,1200,1500,1800,2200,2600,3000,3500,4000,4500,5000)
                           if(!is.null(out$input$n.trees)) n.trees=out$input$n.trees
                 final.mod[[i]] <- gbm.step.fast(dat=dat[c(Split,rep(i,times=sum(dat$response>0)))==i,],gbm.x=out$mods$simp.mod$good.cols,gbm.y = 1,family=model.family,
                                n.trees = n.trees,n.folds=n.folds,max.trees,
                                tree.complexity=out$mods$parms$tc.full,learning.rate=out$mods$lr.mod$lr,bag.fraction=bag.fraction,site.weights=rep(1,times=nrow(dat[c(Split,rep(i,times=sum(dat$response>0)))==i,])),
                                autostop=T,debug.mode=F,silent=!debug.mode,plot.main=F,superfast=F)
                  #             
                  y <- gbm.interactions(final.mod[[i]])
                  int <- y$rank.list;
                  int<-int[int$p<.05,]
                  int <- int[order(int$p),]
                  int$p <- round(int$p,4)
                  names(int) <- c("v1","name1","v2","name2","int.size","p-value")
                  row.names(int)<-NULL
                 if(full.fit){
                     if(nrow(int)>0) out$mods$interactions[[i]] <- int else out$mods$interactions[[i]] <- NULL
                     }     
          }
         
          if(full.fit) {
        
          #post processing steps
          out$mods$final.mod<-final.mod
          var.name<-unlist(lapply(final.mod,function(lst){as.character(lst$contributions[,1])}))
          var.contrib<-unlist(lapply(final.mod,function(lst){lst$contributions[,2]}))
          var.final<-unique(var.name)
          #storing number of variables in final model
              out$mods$vnames<- unique(var.name)
          #can't take mean here because we need to account for when the variable didn't show up in the model
          out$mods$summary<-aggregate(var.contrib,list(Var=var.name),FUN=sum)
          out$mods$summary[,2]<-out$mods$summary[,2]/num.splits
          names(out$mods$summary)[2]<-"rel.inf"
          out$mods$summary<-out$mods$summary[order(out$mods$summary$rel.inf,decreasing=TRUE),]
          out$mods$n.vars.final<-length(var.final)
             
          if(!is.null(unlist(lapply(out$mods$interactions,is.null)))){
               interaction.lst<-out$mods$interactions[!unlist(lapply(out$mods$interactions,is.null))]
               interactions<-(do.call("rbind",out$mods$interactions))[,1:4] #can't consider p-value just if they were included at least once
               out$mods$interactions<-interactions[!duplicated(interactions[,1:4],MARGIN=1),]
              } else out$mods$interactions=NULL
              return(out)
          }
          else return(final.mod)
   }
#================================================================
#                        RF
#================================================================= 
  if(Model=="rf"){
      
          psd.abs<-dat[dat$response==0,]
          rf.full<-list()
          mtry.vect<-vector()
             
               for(i in 1:length(table(Split))){
                    # tune the mtry parameter - this controls the number of covariates randomly subset for each split #
                  cat("\ntuning mtry parameter\n")
                  x=rbind(psd.abs[i==Split,-1],dat[dat$response>=1,-1])
                   y=factor(c(psd.abs[i==Split,1],dat[dat$response>=1,1]))
                  #x=rbind(dat[dat$response==1,-1],psd.abs[i==Split,-1])
                  #y=c(dat[dat$response==1,1],psd.abs[i==Split,1])
                  if(is.null(mtry)){
                     mtr <- tuneRF(x=x,y=y,mtryStart=3,importance=TRUE,ntreeTry=100,
                     replace=FALSE, doBest=F, plot=F)
                     mtry.vect[i] <- mtr[mtr[,2]==min(mtr[,2]),1][1]
                     t2 <- unclass(Sys.time())
                  } else mtry.vect[i]<-mtry
                    cat("\nnow fitting full random forest model using mtry=",mtry,"\n")
                       #
                     rf.full[[i]] <- randomForest(x=x,y=y,xtest=xtest,ytest=ytest,importance=TRUE, ntree=n.trees,
                        mtry=mtry.vect[i],replace=samp.replace,sampsize=ifelse(is.null(sampsize),(ifelse(samp.replace,nrow(x),ceiling(.632*nrow(x)))),sampsize),
                        nodesize=ifelse(is.null(nodesize),(if (!is.null(y) && !is.factor(y)) 5 else 1),nodesize),maxnodes=maxnodes,
                        localImp=localImp, nPerm=nPerm, keep.forest=ifelse(is.null(keep.forest),!is.null(y) && is.null(xtest),keep.forest),
                        corr.bias=corr.bias, keep.inbag=keep.inbag)
                  if(i==1)model.summary<-importance(rf.full[[i]])
                      else model.summary<-model.summary+importance(rf.full[[i]])
             }
              n.pres<-sum(dat$response==1)
               out$mods$parms$mtry=mean(unlist(lapply(rf.full,FUN=function(lst){lst$mtry})))
                        #Reduce("combine",rf.full)
               out$mods$final.mod <- rf.full
              
               if(PsdoAbs){
                  votes<-rep(NA,times=nrow(dat))

                  #these should be oob votes for the absence in a fairly random order
                  if(num.splits==1) votes[dat$response==0]<-apply(do.call("rbind",lapply(lapply(rf.full,predict,type="vote"),"[",1:sum(dat$response==0),2)),2,mean)
                  else for(i in 1:num.splits){
                       votes[which(i==Split,arr.ind=TRUE)]<-as.vector(apply(do.call("rbind",
                       lapply(lapply(rf.full[-c(i)],predict,newdata=psd.abs[i==Split,-1],type="vote"),"[",,2)),2,mean))
                   }
                   
                   #putting in pres votes
                   pres.votes<-matrix(nrow=sum(dat$response>=1),ncol=num.splits)
                   for(i in 1:num.splits)
                   pres.votes[,i] <- predict(rf.full[[i]],type="vote")[rf.full[[i]]$y==1,2]
                   votes[dat$response==1]<-apply(pres.votes,1,mean)
                   
                 	votes[votes==1]<-max(votes[votes<1])                      
                  votes[votes==0]<-min(votes[votes>0]) #from the original SAHM these can't be equal to 0 or 1 otherwise deviance can't be caluclated
                  #though I'm not sure deviance makes sense for RF anyway
                  #confusion matrix oob error and class error currently don't show up for used available but I think they should
                  response<-c(0,1)[factor(votes>.5)]
                  confusion.mat<-table(dat$response,response)
                  oob.error<-100*(1-sum(diag(confusion.mat))/sum(confusion.mat))
                  class.error<-c(confusion.mat[1,2],confusion.mat[2,1])/(apply(confusion.mat,1,sum))
                  out$mods$predictions<-votes
              } else out$mods$predictions<-predict(out$mods$final.mod[[1]],type="vote")[,2] 
                 
                  model.summary<-1/num.splits*model.summary[order(model.summary[,3],decreasing=T),]
                  out$mods$summary <- model.summary
              if(full.fit) return(out)
              else return(rf.full)
             }


}