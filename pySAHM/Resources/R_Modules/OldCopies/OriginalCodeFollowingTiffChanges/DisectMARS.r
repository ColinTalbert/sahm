setwd("H:/Desktop/SAHM/Rcode/FixedRasterBRTChange")

source("FIT_MARS_pluggable.r")
source("FIT_BRT_pluggable.r")
source("FIT_GLM_pluggable.r")
source("FIT_RF_pluggable.r")

#Alaska Dataset that's not working

fit.mars.fct(ma.name="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\canadathistle_gcs_grp_r_mds_train.mds",
  tif.dir="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\layers",
  output.dir="H:\\Desktop\\SAHM\\Output\\FixedRasterBRTChange",
  response.col="response.binary",test.resp.col="response",make.p.tif=T,make.binary.tif=T,
      mars.degree=1,mars.penalty=2,debug.mode=F,ma.test=NULL,script.name="mars.r")
  
fit.brt.fct(ma.name="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\canadathistle_gcs_grp_r_mds_train.mds",
  tif.dir="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\layers",
  output.dir="H:\\Desktop\\SAHM\\Output\\FixedRasterBRTChange",
  response.col="response.binary",test.resp.col="response",make.p.tif=T,make.binary.tif=T,
      simp.method="cross-validation",debug.mode=T,tc=NULL,n.folds=3,ma.test=NULL,alpha=1,script.name="brt.r")

fit.glm.fct(ma.name="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\canadathistle_gcs_grp_r_mds_train.mds",
  tif.dir="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\layers",
  output.dir="H:\\Desktop\\SAHM\\Output\\FixedRasterBRTChange",
  response.col="response.binary",test.resp.col="response",make.p.tif=T,make.binary.tif=T,
      simp.method="AIC",debug.mode=T,ma.test=NULL,script.name="glm.r")


fit.rf.fct(ma.name="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\canadathistle_gcs_grp_r_mds_train.mds",
  tif.dir="H:\\Desktop\\SAHM\\Data\\CanadaThistle\\layers",
  output.dir="H:\\Desktop\\SAHM\\Output\\FixedRasterBRTChange",
  response.col="response.binary",test.resp.col="response",make.p.tif=T,make.binary.tif=T,
      debug.mode=T,n.trees=1000,ma.test=NULL,make.r.curves=T,script.name="glm.r")
      
