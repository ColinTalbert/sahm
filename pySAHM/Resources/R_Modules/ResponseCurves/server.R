shinyServer(function(input, output) {
 XYs <- reactiveValues(
    Xlocs = NULL,
    Ylocs = NULL,
    vals= NULL
  )

  # Handle clicks on the plot
  observeEvent(input$plot_click, {
    if (is.null(XYs$Xlocs)) {
      # We don't have a first click, so this is the first click
      XYs$Xlocs <- input$plot_click$x
      XYs$Ylocs<-  input$plot_click$y
    } else {
    XYs$Xlocs<-append(input$plot_click$x,XYs$Xlocs)[1:min(8,(length(XYs$Xlocs)+1))]
    XYs$Ylocs<-append(input$plot_click$y,XYs$Ylocs)[1:min(8,(length(XYs$Ylocs)+1))] 
    }
    
      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      XYs$vals<-extract(stk,XYdat)
  })

   
 output$map1 <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE)
      
      plot(mapStk,1,maxpixels=60000,col=Colors,xaxt="n",yaxt="n",bty="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }   
  })  
  
 output$map2 <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE)
      plot(mapStk,2,maxpixels=30000,col=Colors,xaxt="n",yaxt="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }
  })
  
output$map3 <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE)
      plot(mapStk,3,maxpixels=30000,col=Colors,xaxt="n",yaxt="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }
  })
 
 output$map4 <- renderPlot({
  #Plot the Map
      par(oma=c(0,0,0,0),mar=c(0,0,2,0),xpd=FALSE)
      plot(mapStk,4,maxpixels=30000,col=Colors,xaxt="n",yaxt="n")

      XYdat<-as.data.frame(cbind(X=XYs$Xlocs,Y=XYs$Ylocs))
      if((any(!is.na(XYdat)))){
      points(x=XYdat$X,y=XYdat$Y,pch=21,col="black",bg=Cols[1:nrow(XYdat)],cex=2.5)
      }
  })
        
output$curves1 <- renderPlot({
  #Plot the Curves
    response.curvesOneModel(fitLst[[1]],modelLst[[1]],XYs$vals)
  })

output$curves2 <- renderPlot({
  #Plot the Curves
    response.curvesOneModel(fitLst[[2]],modelLst[[2]],XYs$vals)
  })
    
output$curves3 <- renderPlot({
  #Plot the Curves
    response.curvesOneModel(fitLst[[3]],modelLst[[3]],XYs$vals)
  })
  
output$curves4 <- renderPlot({
  #Plot the Curves
    response.curvesOneModel(fitLst[[4]],modelLst[[4]],XYs$vals)
  })
  
output$interact<-renderPlot({
 
 #get the value from the sliders using their position
SlideVals<-unlist(lapply(names(dat),FUN=function(l) input[[l]]))

if(input$Model=="All"){
par(mfrow=c(2,2),mar=c(0,0,2,0),oma=c(0,0,0,0))
  interactionPlot(fitLst[[1]],modelLst[[1]],vals=SlideVals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor)
  interactionPlot(fitLst[[2]],modelLst[[2]],vals=SlideVals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor)
  interactionPlot(fitLst[[3]],modelLst[[3]],vals=SlideVals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor)
  interactionPlot(fitLst[[4]],modelLst[[4]],vals=SlideVals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor)
  } else{
   i<-match(input$Model,unlist(modelLst))
    interactionPlot(fitLst[[i]],modelLst[[i]],vals=SlideVals,phi=input$phi,theta=input$theta,x=input$FirstPredictor,y=input$SecondPredictor)
  }
  
})
      
output$sliders <- renderUI({
    
    f<-function(l){
    sliderInput(inputId=as.character(l$Name),label=as.character(l$Name),min=signif(l$min,digits=3),max=signif(l$max,digits=3),value=signif(l$mean,digits=3),round=TRUE)
    }
    getNames<-function(x){as.character(x[[1]])}
    #we're not holding the predictors used in the surface constant so remove them from the
    #input slider list
    datNames<-unlist(lapply(dataLst,getNames))
    match(c(input$FirstPredictor,input$SecondPredictor),datNames)
   # datForSliders<-dataLst[-c(match(c(input$FirstPredictor,input$SecondPredictor),datNames))]
   # lapply(datForSliders, f)
   lapply(dataLst,f)
      
    })
  #output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
   # nearPoints(ras, input$plot_click,threshold=500,addDist=TRUE,maxpoints=1)
     
    # nearPoints() also works with hover and dblclick events
  #})
})