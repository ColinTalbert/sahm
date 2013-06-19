setwd("I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules")
ScriptPath="I:\\VisTrails\\DevWorkspace\\Marian\\userpackages\\sahm\\pySAHM\\Resources\\R_Modules"
source("PairsExplore.r")
source("PairsExploreHelperFcts.r")
source("read.dat.r")
source("chk.libs.r")
source("read.dat.r")
source("my.panel.smooth.binary.r")
source("Predictor.inspection.r")
#================================================================#
#   Setting up parameters
    num.plots <- 10
    min.cor <- .7
    responseCol <- "responseBinary"
    cors.w.highest <- FALSE
    pres=FALSE
    absn=TRUE
    bgd=TRUE
    infil="J:\Projects\SurrogateSpecies\DerivedData\InitialWorkspace\MarianTesting.csv"
    output.file="C:\\temp\\SAHMDebugJunk\\BRTOut1\\pairCountTesting.jpg"
#================================================================#    
# runnning 
 Pairs.Explore(num.plots=15,                                
    min.cor=min.cor,
    input.file=infil,
		output.file=output.file,
		response.col=responseCol,
		pres=TRUE,
		absn=TRUE,
		bgd=TRUE,
    Debug=FALSE)
    

#================================================================#
# and from the command line
I:\VisTrails\Central_VisTrails_x32\Central_R\R-2.14.1\bin\i386\Rterm.exe --vanilla -f "I:\VisTrails\Central_VisTrails_x32_debug\VisTrails\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\PairsExplore.r" --args p=5 m=.7 o=H:\Desktop\pairPres.jpg i="C:\VisTrails\mtalbert_20110504T132851\readMaTests\Split.csv" rc="ResponseBinary" core=TRUE



if(argSplit[[1]][1]=="p") num.plots <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="m") min.cor <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="o") output.dir <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="i") input.file <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="rc") responseCol <- argSplit[[1]][2]
    	if(argSplit[[1]][1]=="core") cors.w.higest <- argSplit[[1]][2]