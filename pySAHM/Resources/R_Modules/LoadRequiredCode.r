###############################################################################
##
## Copyright (C) 2010-2012, USGS Fort Collins Science Center. 
## All rights reserved.
## Contact: talbertc@usgs.gov
##
## This file is part of the Software for Assisted Habitat Modeling package
## for VisTrails.
##
## "Redistribution and use in source and binary forms, with or without 
## modification, are permitted provided that the following conditions are met:
##
##  - Redistributions of source code must retain the above copyright notice, 
##    this list of conditions and the following disclaimer.
##  - Redistributions in binary form must reproduce the above copyright 
##    notice, this list of conditions and the following disclaimer in the 
##    documentation and/or other materials provided with the distribution.
##  - Neither the name of the University of Utah nor the names of its 
##    contributors may be used to endorse or promote products derived from 
##    this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
## THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
## PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
## EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
## OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
## ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
##
## Although this program has been used by the U.S. Geological Survey (USGS), 
## no warranty, expressed or implied, is made by the USGS or the 
## U.S. Government as to the accuracy and functioning of the program and 
## related program material nor shall the fact of distribution constitute 
## any such warranty, and no responsibility is assumed by the USGS 
## in connection therewith.
##
## Any use of trade, firm, or product names is for descriptive purposes only 
## and does not imply endorsement by the U.S. Government.
###############################################################################

#Reading data and checking libraries
source(paste(ScriptPath,"read.dat.r",sep="\\")) 
source(paste(ScriptPath,"path.check.r",sep="\\")) 
source(paste(ScriptPath,"read.ma.r",sep="\\"))
source(paste(ScriptPath,"chk.libs.r",sep="\\"))


#Fitting Models and making predictions
source(paste(ScriptPath,"generic.model.fit.r",sep="\\"))
source(paste(ScriptPath,"FitModels.r",sep="\\"))
source(paste(ScriptPath,"model.fit.r",sep="\\"))
source(paste(ScriptPath,"pred.fct.r",sep="\\"))

#Calculating Evaluation Metrics and producing plots
source(paste(ScriptPath,"make.auc.r",sep="\\"))
source(paste(ScriptPath,"EvalStats.r",sep="\\"))
source(paste(ScriptPath,"ResidualImage.r",sep="\\"))
source(paste(ScriptPath,"Pred.Surface.r",sep="\\"))
source(paste(ScriptPath,"calc.deviance.r",sep="\\"))
source(paste(ScriptPath,"calibration.r",sep="\\"))
source(paste(ScriptPath,"roc.r",sep="\\"))
source(paste(ScriptPath,"CalcStats.r",sep="\\"))
source(paste(ScriptPath,"EvalStatsHelperFcts.r",sep="\\"))
source(paste(ScriptPath,"TestTrainRocPlot.r",sep="\\"))
source(paste(ScriptPath,"ConfusionMatrix.r",sep="\\"))
source(paste(ScriptPath,"PresenceOnlyCalibration.r",sep="\\"))
source(paste(ScriptPath,"response.curves.r",sep="\\"))
source(paste(ScriptPath,"VariableImportance.r",sep="\\"))

#Writing output metrics
source(paste(ScriptPath,"capture.stats.r",sep="\\"))
source(paste(ScriptPath,"write.txt.r",sep="\\"))
source(paste(ScriptPath,"AppendOut.r",sep="\\"))
source(paste(ScriptPath,"modalDialog.r",sep="\\"))

#Making maps
source(paste(ScriptPath,"proc.tiff.r",sep="\\"))

#Other utility functions
source(paste(ScriptPath,"cv.fctNew.r",sep="\\"))
source(paste(ScriptPath,"place.save.r",sep="\\"))
source(paste(ScriptPath,"SplitBackground.r",sep="\\"))

