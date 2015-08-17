shinyServer(function(input, output) {
 XYs <- reactiveValues(
    Xlocs = NULL,
    Ylocs = NULL,
    vals= NULL
  )

IntractVals<-reactiveValues(
#start with the means
Vals = vector()
)

observeEvent(input$resetVals,{
     IntractVals$Vals=vector()
})
#==============================================
# Maps 
#==========================
# Handle clicks on the plot
observeEvent(input$plot_click, {
    if (is.null(XYs$Xlocs)) {
      # We don't have a first click, so this is the first click
      XYs$Xlocs <- input$plot_click$x
      XYs$Ylocs <-  input$plot_click$y
    } else {
    XYs$Xlocs<-append(XYs$Xlocs,input$plot_click$x)
    XYs$Ylocs<-append(XYs$Ylocs,input$plot_click$y)
    }
    
      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      XYs$vals<-extract(stk,XYdat)
})
observeEvent(input$resetNVals,{
     XYs$Xlocs=NULL
     XYs$Ylocs=NULL
     XYs$vals=NULL
})
#============================  
#Map Generation
lapply(1:length(modelLst),function(i){
output[[paste("map",i,sep="")]] <- renderPlot({       
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE) 
      plot(mapStk,i,maxpixels=60000,col=Colors,xaxt="n",yaxt="n",bty="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)  
  }
  })
})    
#============================    
#Response Curve Generation for Map 
lapply(1:length(modelLst),function(i){
output[[paste("curves",i,sep="")]] <- renderPlot({        
  #Plot the Curves
    responseCurves(list(f=fitLst[[i]]),list(m=modelLst[[i]]),vals=XYs$vals,varImp=list(varImpLst[[i]]),addImp=input$addMImp)
  })
  })

#==============================================
# Sliders   
#============================
#Response curves for sliders

observeEvent(input$addVals,{
  IntractV<-unlist(lapply(paste(names(dat),"aa",sep=""),FUN=function(l) input[[l]]))
  IntractVals$Vals<-rbind(IntractVals$Vals,IntractV)
 })

lapply(1:length(dataLst),IntractVals=IntractVals,function(i,IntractVals){
output[[paste("slideRsp",i,sep="")]]<-renderPlot({
  responseCurves(fitLst,modelLst,vals=IntractVals$Vals,i,varImp=varImpLst,addImp=input$addImp)
  })
})
  
#==============================================
# Interactions   
#============================  
# predictor interaction
output$sliders <- renderUI({
    
    f<-function(l){
    sliderInput(inputId=as.character(l$Name),label=as.character(l$Name),min=signif(l$min,digits=3),max=signif(l$max,digits=3),value=signif(l$mean,digits=3),round=TRUE)
    }
    getNames<-function(x){as.character(x[[1]])}
    #we're not holding the predictors used in the surface constant so remove them from the
    #input slider list
    datNames<-unlist(lapply(dataLst,getNames))
    match(c(input$FirstPredictor,input$SecondPredictor),datNames)
   datForSliders<-dataLst[-c(match(c(input$FirstPredictor,input$SecondPredictor),datNames))]
   lapply(datForSliders, f)    
    })

output$interact<-renderPlot({
 
 #get the value from the sliders using their position
SlideNames<-names(dat)[-c(which(names(dat)%in%c(input$FirstPredictor,input$SecondPredictor)))]
SlideVals<-unlist(lapply(SlideNames,FUN=function(l) input[[l]]))
    if(!is.null(SlideVals)){
        #slider values are missing the values for the indicies of the first and second predictor so put the spaces back in
        Svals<-vector(length=ncol(dat))
        toAdd<-sort(match(c(input$FirstPredictor,input$SecondPredictor),names(dat)))
        datPos<-seq(1:ncol(dat))[-c(toAdd)]
        Svals[datPos]<-SlideVals
        SlideVals<-Svals
    }
if(input$FirstPredictor==input$SecondPredictor){
 plot(0:1,0:1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")   
}else{if(input$Model=="All"){
  par(mfrow=c(2,2),mar=c(1,1,1,1),oma=c(0,0,0,0))
  for(i in 1:length(fitLst)){
    interactionPlot(fitLst[[i]],modelLst[[i]],vals=SlideVals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor)
    }
} else{
   i<-match(input$Model,unlist(modelLst))
    interactionPlot(fitLst[[i]],modelLst[[i]],vals=Svals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor)
  }
}  
})
#=====================
# named sliders
#creating a named list of sliders so I can put them where I feel like 
lapply(1:length(dataLst),function(i){
output[[paste("slide",i,sep="")]] <- renderUI({ 
    sliderInput(inputId=paste(as.character(dataLst[[i]]$Name),"aa",sep=""),label=as.character(dataLst[[i]]$Name),min=signif(dataLst[[i]]$min,digits=3),max=signif(dataLst[[i]]$max,digits=3),
    value=signif(dataLst[[i]]$mean,digits=3),round=TRUE)
    })
})
#=========================
#a named list of predictor densities
lapply(1:length(dataLst),function(i){
output[[paste("dens",i,sep="")]] <- renderPlot({
           cols<-c("blue","red")
          color.box<-col2rgb(cols,alpha=TRUE)
                           color.box[4,]<-60
          temp.fct<-function(a){return(rgb(red=a[1],green=a[2],blue=a[3],alpha=a[4]))}
          cols<-apply(color.box/255,2,temp.fct)
            presDens<-density(dat[resp==1,i])
            absDens<-density(dat[resp==0,i])
            par(mar=c(2,.3,0,.3),oma=c(0,0,0,0))
            plot(x=range(c(absDens$x,presDens$x)),y=c(0,max(absDens$y,presDens$y)),type="n",
            ylab="",xlab=names(dat)[i],yaxt="n")
            polygon(absDens,col=cols[1],border="blue")
            polygon(presDens,col=cols[2],border="red")
    })
})      

  #output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
   # nearPoints(ras, input$plot_click,threshold=500,addDist=TRUE,maxpoints=1)
     
    # nearPoints() also works with hover and dblclick events
  #})
})