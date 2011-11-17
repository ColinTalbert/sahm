setwd("I:\\VisTrails\\Central_VisTrailsInstall_debug\\vistrails\\packages\\sahm\\pySAHM\\Resources\\R_Modules\\OldCopies\\OriginalCodeFollowingTiffChanges")

source("FIT_MARS_pluggable.r")
source("FIT_BRT_pluggable.r")
source("FIT_GLM_pluggable.r")
source("FIT_RF_pluggable.r")

#ma.name="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\canadathistle_gcs_grp_r_mds_train.mds"
#tif.dir="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\layers"
#output.dir="H:\\Desktop\\SAHM\\Output\\FixedRasterBRTChange"

ma.name="C:\\VisTrails\\mtalbert_20110504T132851\\readMaTests\\NoSplitOrigFormat.csv"
tif.dir="C:\\VisTrails\\mtalbert_20110504T132851\\PARC_1"
output.dir="C:\\temp\\SAHMDebugJunk\\BRTOut1\\OrigOut"


fit.mars.fct(ma.name=ma.name,
  tif.dir=tif.dir,
  output.dir=output.dir,
  response.col="response.binary",test.resp.col="response",make.p.tif=T,make.binary.tif=T,
      mars.degree=1,mars.penalty=2,debug.mode=F,ma.test=NULL,script.name="mars.r")

set.seed(1)
fit.brt.fct(ma.name=ma.name,
  tif.dir=tif.dir,
  output.dir=output.dir,
  response.col=rc,test.resp.col="response",make.p.tif=T,make.binary.tif=T,
      simp.method="cross-validation",debug.mode=T,tc=NULL,n.folds=3,ma.test=NULL,alpha=1,script.name="brt.r")

fit.glm.fct(ma.name=ma.name,
  tif.dir=tif.dir,
  output.dir=output.dir,
  response.col="response.binary",test.resp.col="response",make.p.tif=T,make.binary.tif=T,
      simp.method="AIC",debug.mode=T,ma.test=NULL,script.name="glm.r")

set.seed(1)
fit.rf.fct(ma.name=ma.name,
  tif.dir=tif.dir,
  output.dir=output.dir,
  response.col="response.binary",test.resp.col="response",make.p.tif=T,make.binary.tif=T,
      debug.mode=T,n.trees=1000,ma.test=NULL,make.r.curves=T,script.name="glm.r")
      
