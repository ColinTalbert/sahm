pred.fct<-function(model,x,Model){

  if(Model=="glm"){

          y <- try(as.vector(predict(model,x,type="response")),silent=TRUE)
          if(class(y)=="try-error") stop("Predicting the response for the new values failed.  One probable cause is that you are trying to predict to factor levels that were not present during model fitting.")
          # encode missing values as -1.
          y[is.na(y)]<- NaN
     }

  if(Model=="mars"){
        # retrieve key items from the global environment #
        # make predictionss.
        y <- rep(NA,nrow(x))
        y[complete.cases(x)] <- try(as.vector(mars.predict(model,x[complete.cases(x),])$prediction[,1]),silent=true)

      #which(is.na(y)
        # encode missing values as -1.
        y[is.na(y)]<- NaN
  }
  if(Model=="brt"){
         # retrieve key items from the global environment #
          # make predictions from complete data only #
          #y <- rep(NA,nrow(x))
          #y[complete.cases(x)] <- predict.gbm(model, x[complete.cases(x),],model$target.trees,type="response")

          # make predictions from full data #
          y <- try(predict.gbm(model,x,model$target.trees,type="response"),silent=TRUE)
          # encode missing values as -1.
           a<-complete.cases(x)
          y[!(a)]<- NaN
      }

  if(Model=="rf"){
       # retrieve key items from the global environment #
          # make predictions from complete data only #
          y <- rep(NA,nrow(x))
          y[complete.cases(x)] <- try(as.vector(predict(model,newdata=x[complete.cases(x),],type="prob")[,2]),silent=TRUE)

          # make predictions from full data #

          # encode missing values as -1.
          y[is.na(y)]<- NaN
   }
    if(class(y)=="try-error") stop("Predicting the response for the new values failed.  One probable cause is that you are trying to predict to factor levels that were not present during model fitting.")
return(y)
}