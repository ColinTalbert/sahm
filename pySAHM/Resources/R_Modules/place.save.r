place.save<-function(out,Final.Model){

last.dir<-strsplit(out$input$output.dir,split="\\\\")
                        parent<-sub(paste("\\\\",last.dir[[1]][length(last.dir[[1]])],sep=""),"",out$input$output.dir)
                         compile.out<-paste(parent,
                              paste(ifelse(missing(Final.Model),"AcrossModel","FinalEvaluation"),
                               switch(out$dat$split.type,"crossValidation"="CrossVal","test"="TestTrain","none"="NoSplit"),
                               switch(out$input$model.family,"binomial"="Binom","bernoulli"="Binom","poisson"="Count"),".csv"
                              ,sep=""),sep="/")
                              
 Header<-cbind(c("","Original Field Data","Field Data Template","PARC Output Folder","PARC Template","Covariate Selection Name",""),
                            c(last.dir[[1]][length(last.dir[[1]])],
                            out$dat$input$OrigFieldData,out$dat$input$FieldDataTemp,out$dat$input$ParcOutputFolder,
                            out$dat$input$ParcTemplate,ifelse(length(out$dat$input$CovSelectName)==0,"NONE",out$dat$input$CovSelectName),""))

if(file.access(compile.out,mode=0)==-1){ #if very first time through little
          write.table(Header,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=",")

  } else { #this else (not the first time through) read current csv first
          input<-read.table(compile.out,fill=TRUE,sep=",")
          output<-cbind(input,c(Header[,2],rep("",times=(nrow(input)-nrow(Header)))))
              temp=try(write.table(output,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=","),silent=TRUE)
           while(class(temp)=="try-error"){
                      modalDialog("","Please Close the AppendedOutput.exe\ so that R can write to it then press ok to continue ","")
                      temp<-try(write.table(output,file =compile.out,row.names=FALSE,col.names=FALSE,quote=FALSE,sep=","),silent=TRUE)
                      }
          }

 out$input$Append.Dir<-compile.out
 return(out)
 }