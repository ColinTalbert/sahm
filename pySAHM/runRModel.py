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
                
                NDValue = getNDVal(onlyfiles[0])
                
                args = ["placeholder", "-o", outFname] + onlyfiles
                gdal.DontUseExceptions()
                gdal_merge.main(args)
                
                dataset = gdal.Open( outFname, gdal.GA_Update )
                dataset.GetRasterBand(1).SetNoDataValue(float(NDValue))
                dataset.GetRasterBand(1).ComputeStatistics(1)
                #shutil.rmtree(tilesFolder)
               
#these three functi 
def getNDVal(filename):
    dataset = gdal.Open(filename, gdalconst.GA_ReadOnly)
    band = dataset.GetRasterBand(1)
    
    NDValue = band.GetNoDataValue()
    
    min = band.GetMinimum()
    if approx_equal(NDValue, min):
        upperLeftPixVal = band.ReadAsArray(0, 0, 1, 1, 1, 1)[0][0]
        if approx_equal(NDValue, upperLeftPixVal):
            NDValue = band.ReadAsArray(0, 0, 1, 1, 1, 1)[0][0]    
    
    dataset = None
    return NDValue
    
#these two functions were pulled from: http://code.activestate.com/recipes/577124-approximately-equal/
def _float_approx_equal(x, y, tol=1e-18, rel=1e-7):
    if tol is rel is None:
        raise TypeError('cannot specify both absolute and relative errors are None')
    tests = []
    if tol is not None: tests.append(tol)
    if rel is not None: tests.append(rel*abs(x))
    assert tests
    return abs(x - y) <= max(tests)


def approx_equal(x, y, *args, **kwargs):
    """approx_equal(float1, float2[, tol=1e-18, rel=1e-7]) -> True|False
    approx_equal(obj1, obj2[, *args, **kwargs]) -> True|False

    Return True if x and y are approximately equal, otherwise False.

    If x and y are floats, return True if y is within either absolute error
    tol or relative error rel of x. You can disable either the absolute or
    relative check by passing None as tol or rel (but not both).

    For any other objects, x and y are checked in that order for a method
    __approx_equal__, and the result of that is returned as a bool. Any
    optional arguments are passed to the __approx_equal__ method.

    __approx_equal__ can return NotImplemented to signal that it doesn't know
    how to perform that specific comparison, in which case the other object is
    checked instead. If neither object have the method, or both defer by
    returning NotImplemented, approx_equal falls back on the same numeric
    comparison used for floats.

    >>> almost_equal(1.2345678, 1.2345677)
    True
    >>> almost_equal(1.234, 1.235)
    False

    """
    if not (type(x) is type(y) is float):
        # Skip checking for __approx_equal__ in the common case of two floats.
        methodname = '__approx_equal__'
        # Allow the objects to specify what they consider "approximately equal",
        # giving precedence to x. If either object has the appropriate method, we
        # pass on any optional arguments untouched.
        for a,b in ((x, y), (y, x)):
            try:
                method = getattr(a, methodname)
            except AttributeError:
                continue
            else:
                result = method(b, *args, **kwargs)
                if result is NotImplemented:
                    continue
                return bool(result)
    # If we get here without returning, then neither x nor y knows how to do an
    # approximate equal comparison (or are both floats). Fall back to a numeric
    # comparison.
    return _float_approx_equal(x, y, *args, **kwargs)    


if __name__ == "__main__":

    try:
        main(sys.argv[1:])
    except:
        print "Job failed!", sys.exc_info()[0]