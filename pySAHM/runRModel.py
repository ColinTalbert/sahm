import os, sys
import subprocess
import utilities
import shutil

from osgeo import gdal as gdal

def main(args_in):
    print "args used = ", args_in
    
    
    p = subprocess.Popen(args_in, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    ret = p.communicate()
    
    print ret[0]
    sys.stderr.write(ret[1])
    
    for arg in args_in:
        if arg.startswith("o="):
            outDir = arg[2:]
    
    print "outDir=", outDir
    setupGDAL()
    mosaicTiledOutputs(outDir)
    
    print "Finished successfully!"


def setupGDAL():
    parentDir = os.path.split(os.path.dirname(__file__))[0]
    gdal_data = os.path.join(parentDir, "GDAL_Resources", "gdal-data")
    os.environ['GDAL_DATA'] = gdal_data
    projlib = os.path.join(parentDir, "GDAL_Resources", "projlib")
    os.environ['PROJ_LIB'] = projlib
    

def mosaicTiledOutputs(outputDirectory):
    import imp
    curDir = os.path.dirname(__file__)
    parentDir = os.path.dirname(curDir)
    gdal_mergePy = os.path.join(parentDir, "GDAL_Resources", "Utilities", "gdal_merge.py")
    
    gdal_merge = imp.load_source("gdal_merge", gdal_mergePy)
    
    
    for m in ['Bin', 'Prob', 'Resid', 'MESS', 'Mod']:
            tilesFolder = os.path.join(outputDirectory, m + "Tiff")
            print "tilesFolder", tilesFolder
            if os.path.exists(tilesFolder):
                modelAbbrev = os.path.split(outputDirectory)[1].split("_")[0]
                outFname = os.path.join(outputDirectory, "_".join([modelAbbrev, m.lower(), "map.tif"]))
                print "outFname", outFname
                
                onlyfiles = [os.path.join(tilesFolder,f) for f in os.listdir(tilesFolder) 
                             if os.path.isfile(os.path.join(tilesFolder,f)) and f.endswith(".tif") ]
                args = ["placeholder", "-o", outFname] + onlyfiles
                gdal.DontUseExceptions()
                gdal_merge.main(args)
                shutil.rmtree(tilesFolder)
                
    
    
if __name__ == "__main__":

    try:
        main(sys.argv[1:])
    except:
        print "Job failed!", sys.exc_info()[0]