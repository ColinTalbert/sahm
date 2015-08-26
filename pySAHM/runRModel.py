import os, sys
import subprocess
import utilities
import shutil
import time

from osgeo import gdal as gdal
from osgeo import gdalconst as gdalconst

def main(args_in):
    print "command line to run this model:\n", " ".join(args_in)

    for arg in args_in:
        logger = False
        if arg.startswith("o="):
            outDir = arg[2:]

            while not os.path.isdir(outDir):
                outDir = os.path.split(outDir)[0]
#              outDir = os.path.split(outDir)[0]

            print "outDir=", outDir
            logger = utilities.logger(os.path.split(outDir)[0], True)

#   if this is an ApplyModel we need to wait for the preceeding model to finish
#   up before launching R
    if "EvaluateNewData.r" in args_in[3]:
        inDir = [os.path.split(d[3:])[0] for d in args_in if d.startswith("ws=")][0]
        while True:
            check = utilities.checkIfModelFinished(inDir)
            if  check == "Error in model":
                sys.stderr.write("Error in original model that this ApplyModel needs")
                sys.exit("Error in original model could not apply model")
            elif check.startswith("Completed successfully"):
                time.sleep(3)
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
        print msg
        return

    elif 'Warning' in ret[1]:
        msg = "The R script returned the following warning(s).  The R warning message is below - \n"
        msg += ret[1]
        if logger:
            logger.writetolog(msg)

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
            tiles_dname = os.path.join(outputDirectory, m + "Tiff")
            print "tiles_dname", tiles_dname
            if os.path.exists(tiles_dname):
                modelAbbrev = os.path.split(outputDirectory)[1].split("_")[0]
                outFname = os.path.join(outputDirectory, "_".join([modelAbbrev, m.lower(), "map.tif"]))

                onlyfiles = [os.path.join(tiles_dname, f) for f in os.listdir(tiles_dname)
                             if os.path.isfile(os.path.join(tiles_dname, f)) and f.endswith(".tif") ]

                if onlyfiles:

                    print "outFname", outFname
                    NDValue = get_nd_val(onlyfiles[0])

                    args = ["placeholder", "-o", outFname] + onlyfiles
                    gdal.DontUseExceptions()
                    gdal_merge.main(args)

                    dataset = gdal.Open(outFname, gdal.GA_Update)
                    dataset.GetRasterBand(1).SetNoDataValue(float(NDValue))
                    dataset.GetRasterBand(1).ComputeStatistics(1)

                    if m == 'Mod':
                        #  we must merge the dbf files as well.  ugg.
                        only_dbf_files = [os.path.join(tiles_dname, f) for f in os.listdir(tiles_dname)
                                 if os.path.isfile(os.path.join(tiles_dname, f)) and f.endswith(".vat.dbf") ]
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
                    shutil.rmtree(tiles_dname)
                    tile_log_fname = [f for f in os.listdir(tiles_dname)
                                            if f.endswith('_prob_map.txt')][0]
                    os.remove(os.path.join(tiles_dname, tile_log_fname))
                except:
                    #  can run into latency problems with the thumbs.db in windows.
                    #  if we can't clean up this folder it's not the end of the world.
                    pass


def get_nd_val(filename):
    '''Attemps to determine the nodata value used in this raster
    First it reads the value from the header metadata
    If that value is approximately the same as raster min
    the return the exact pixel value
    '''
    dataset = gdal.Open(filename, gdalconst.GA_ReadOnly)
    band = dataset.GetRasterBand(1)

    nd_value = band.GetNoDataValue()

    min = band.GetMinimum()
    if utilities.approx_equal(nd_value, min):
        upper_left_pix = band.ReadAsArray(0, 0, 1, 1, 1, 1)[0][0]
        if utilities.approx_equal(nd_value, upper_left_pix):
            nd_value = band.ReadAsArray(0, 0, 1, 1, 1, 1)[0][0]

    dataset = None
    return nd_value


if __name__ == "__main__":

#    try:
    main(sys.argv[1:])
#    except:
#        print "Job failed!", sys.exc_info()[0]
