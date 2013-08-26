input.file="I:\\VisTrails\\WorkingFiles\\workspace\\__tutorial6\\FDAW_2.csv" 
bias=FALSE 
output.dir="I:\\VisTrails\\WorkingFiles\\workspace\\__tutorial6\\FDAW_2_KDE_adhoc_iso95.tif" 
tmplt="I:\\VisTrails\\Tutorial files\\MarianTutorial\\BrewersSparrowData\\Predictors900m_small\\small_abs1km"

PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="adhoc",isopleth=95,bias=FALSE,template=tmplt)
