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
source(file.path(ScriptPath,"read.dat.r")) 
source(file.path(ScriptPath,"path.check.r")) 
source(file.path(ScriptPath,"read.ma.r"))
source(file.path(ScriptPath,"chk.libs.r"))


#Fitting Models and making predictions
source(file.path(ScriptPath,"generic.model.fit.r"))
source(file.path(ScriptPath,"FitModels.r"))
source(file.path(ScriptPath,"model.fit.r"))
source(file.path(ScriptPath,"pred.fct.r"))

#Calculating Evaluation Metrics and producing plots
source(file.path(ScriptPath,"make.auc.r"))
source(file.path(ScriptPath,"EvalStats.r"))
source(file.path(ScriptPath,"ResidualImage.r"))
source(file.path(ScriptPath,"Pred.Surface.r"))
source(file.path(ScriptPath,"calc.deviance.r"))
source(file.path(ScriptPath,"calibration.r"))
source(file.path(ScriptPath,"roc.r"))
source(file.path(ScriptPath,"CalcStats.r"))
source(file.path(ScriptPath,"EvalStatsHelperFcts.r"))
source(file.path(ScriptPath,"TestTrainRocPlot.r"))
source(file.path(ScriptPath,"ConfusionMatrix.r"))
source(file.path(ScriptPath,"PresenceOnlyCalibration.r"))
source(file.path(ScriptPath,"response.curves.r"))
source(file.path(ScriptPath,"VariableImportance.r"))

#Writing output metrics
source(file.path(ScriptPath,"capture.stats.r"))
source(file.path(ScriptPath,"write.txt.r"))
source(file.path(ScriptPath,"AppendOut.r"))
source(file.path(ScriptPath,"modalDialog.r"))

#Making maps
source(file.path(ScriptPath,"proc.tiff.r"))
source(file.path(ScriptPath,"CalcMESS.r"))
source(file.path(ScriptPath,"parRaster.r"))

#Other utility functions
source(file.path(ScriptPath,"cv.fct.r"))
source(file.path(ScriptPath,"place.save.r"))
source(file.path(ScriptPath,"SplitBackground.r"))

