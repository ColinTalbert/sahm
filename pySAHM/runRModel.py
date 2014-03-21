import os, sys
import subprocess
import utilities
import shutil
import time

from osgeo import gdal as gdal
from osgeo import gdalconst as gdalconst

def main(args_in):
    print "args used = ", args_in

    for arg in args_in:
        logger = False
        if arg.startswith("o="):
            outDir = arg[2:]

            while not os.path.isdir(outDir):
                outDir = os.path.split(outDir)[0]

            print "outDir=", outDir
            logger = utilities.logger(os.path.join(outDir, "logfile.txt"), True)

#   if this is an ApplyModel we need to wait for the preceeding model to finish
#   up before launching R
    print "args_in[3]", args_in[3]
    if "EvaluateNewData.r" in args_in[3]:
        inDir = [os.path.split(d[3:])[0] for d in args_in if d.startswith("ws=")][0]
        while True:
            check = utilities.checkIfModelFinished(inDir)
            if  check == "Error in model":
                sys.stderr.write("Error in original model that this ApplyModel needs")
                sys.exit("Error in original model could not apply model")
            elif check.startswith("Completed successfully"):
                time.sleep(5)
                break

    p = subprocess.Popen(args_in, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    ret = p.communicate()

    print ret[0]  #  this sends it to the std out
    sys.stderr.write(ret[1])

    msg = ""
    if 'Error' in ret[1]:
        msg = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        msg += "\n  An error was encountered in the R script for this module."
        msg += "\n     The R error message is below: \n"
        msg += ret[1]
        if logger:
            logger.writetolog(msg)
        sys.stderr.write(msg)
        print msg
        return

    elif 'Warning' in ret[1]:
        msg = "The R scipt returned the following warning(s).  The R warning message is below - \n"
        msg += ret[1]
        if logger:
            logger.writetolog(msg)
    sys.stderr.write(msg)

    sys.stderr.write(ret[1])

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

                onlyfiles = [os.path.join(tilesFolder, f) for f in os.listdir(tilesFolder)
                             if os.path.isfile(os.path.join(tilesFolder, f)) and f.endswith(".tif") ]

                NDValue = getNDVal(onlyfiles[0])

                args = ["placeholder", "-o", outFname] + onlyfiles
                gdal.DontUseExceptions()
                gdal_merge.main(args)

                dataset = gdal.Open(outFname, gdal.GA_Update)
                dataset.GetRasterBand(1).SetNoDataValue(float(NDValue))
                dataset.GetRasterBand(1).ComputeStatistics(1)

                if m == 'Mod':
                    #  we must merge the dbf files as well.  ugg.
                    only_dbf_files = [os.path.join(tilesFolder, f) for f in os.listdir(tilesFolder)
                             if os.path.isfile(os.path.join(tilesFolder, f)) and f.endswith(".vat.dbf") ]
                    dbf_0_f = open(only_dbf_files[0], "rb")
                    dbf_0 = list(utilities.dbfreader(dbf_0_f))
                    dbf_0_f.close()

                    fieldnames, fieldspecs = (dbf_0)[:2]
                    all_values = set([val[1].strip() for val in dbf_0[2:]])

                    for dbf_file in only_dbf_files[1:]:
                        dbf_f = open(dbf_file, 'rb')
                        dbf_list = list(utilities.dbfreader(dbf_f))
                        dbf_f.close()

                        dbf_n_values = set([val[1].strip() for val in dbf_list[2:]])
                        all_values = all_values | dbf_n_values

                    dbf_out_fname = outFname.replace(".tif", ".tif.vat.dbf")
                    dbf_out_f = open(dbf_out_fname, 'wb')
                    all_values = zip(range(1, len(all_values) + 1), list(all_values))
                    utilities.dbfwriter(dbf_out_f, fieldnames, fieldspecs, all_values)
                    dbf_out_f.close()

                try:
                    shutil.rmtree(tilesFolder)
                except:
                    #  can run into latency problems with the thumbs.db in windows.
                    #  if we can't clean up this folder it's not the end of the world.
                    pass


def getNDVal(filename):
    dataset = gdal.Open(filename, gdalconst.GA_ReadOnly)
    band = dataset.GetRasterBand(1)

    NDValue = band.GetNoDataValue()

    min = band.GetMinimum()
    if utilities.approx_equal(NDValue, min):
        upperLeftPixVal = band.ReadAsArray(0, 0, 1, 1, 1, 1)[0][0]
        if utilities.approx_equal(NDValue, upperLeftPixVal):
            NDValue = band.ReadAsArray(0, 0, 1, 1, 1, 1)[0][0]

    dataset = None
    return NDValue


if __name__ == "__main__":

#    try:
    main(sys.argv[1:])
#    except:
#        print "Job failed!", sys.exc_info()[0]
