'''
These are utilites used by the VisTrails 
wrappers of the SAHM command line applications

@author: talbertc
'''
import os, sys, shutil
import traceback
import csv
import time
import tempfile

from PyQt4 import QtCore

from core.modules.basic_modules import File, Directory, new_constant, Constant
from core.modules.vistrails_module import ModuleError

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr
import numpy

import packages.sahm.pySAHM.utilities as utilities

import getpass

_roottempdir = ""
#_logfile = ""
_verbose = False
_temp_files = []
_temp_dirs = []
_logger = None

def mktempfile(*args, **kwargs):
    global _temp_files
    global _roottempdir
    kwargs["dir"] = _roottempdir
    (fd, fname) = tempfile.mkstemp(*args, **kwargs)
    os.close(fd)
    _temp_files.append(fname)
    return fname

def mktempdir(*args, **kwargs):
    global _temp_dirs
    global _roottempdir
    kwargs["dir"] = _roottempdir
    dname = tempfile.mkdtemp(*args, **kwargs)
    _temp_dirs.append(dname)
    return dname

def mknextfile(prefix, suffix="", directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir
    files = os.listdir(directory)
    seq = 0
    for f in files:
        if f.startswith(prefix):
            old_seq = f.replace(prefix, '')
            old_seq = old_seq.replace(suffix, '')
            old_seq = old_seq.replace("_", '')
            if old_seq.isdigit():
                if int(old_seq) > seq:
                    seq = int(old_seq)
    seq += 1
    filename = prefix + str(seq) + suffix
    return os.path.join(directory, filename)

def mknextdir(prefix, directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir
    files = os.listdir(directory)
    seq = 0
    for f in files:
        if (f.startswith(prefix) and
            not os.path.isfile(f)):
            f_seq = int(f.replace(prefix, ''))
            if f_seq > seq:
                seq = f_seq
    seq += 1
    dirname = prefix + str(seq)
    os.mkdir(os.path.join(directory, dirname))
    return os.path.join(directory, dirname)

def createrootdir(rootWorkspace):
    global _roottempdir
#    global _logfile
    _roottempdir = os.path.join(rootWorkspace, getpass.getuser() + '_' + time.strftime("%Y%m%dT%H%M%S"))
    if not os.path.exists(_roottempdir):
        os.makedirs(_roottempdir) 
#    _logfile = os.path.join(_roottempdir, "sessionLog.txt")
    
    return _roottempdir
#
#def setverbose(verbose):
#    global _verbose
#    _verbose = verbose

def cleantemps():
    pass
#    global _temp_files, _temp_dirs, _roottempdir
#    for file in _temp_files:
#        os.remove(file)
#    for dir in _temp_dirs:
#        shutil.rmtree(dir)
#    shutil.rmtree(_roottempdir)

def map_ports(module, port_map):
    args = {}
    for port, (flag, access, required) in port_map.iteritems():
        if required or module.hasInputFromPort(port):
            value = module.getInputFromPort(port)
            if access is not None:
                value = access(value)
            args[flag] = value
    return args

def path_value(value):
    return value.name

def dir_path_value(value):
    val = value.name
    sep = os.path.sep
    return val.replace("/", sep)
    
def create_file_module(fname, f=None):
    if f is None:
        f = File()
    f.name = fname
    f.upToDate = True
    return f

def create_dir_module(dname, d=None):
    if d is None:
        d = Directory()
    d.name = dname
    d.upToDate = True
    return d

def collapse_dictionary(dict):
    list = []
    for k,v in dict.items():
        list.append(k)
        list.append(v)
    return list

def tif_to_color_jpeg(input, output, colorBreaksCSV):
    writetolog("    running  tif_to_color_jpeg()", True, False)
    writetolog("        input=" + input, False, False)
    writetolog("        output=" + output, False, False)
    writetolog("        colorBreaksCSV=" + colorBreaksCSV, False, False)
    out_bands = 3
    #output_tmp = mktempfile(prefix='intermediateJpegPic', suffix='.tif')
    output_tmp = mknextfile(prefix='intermediateJpegPic_', suffix='.tif')
    # Print some info
    #print "Creating %s" % (output)
    
    #populate the color breaks dictionary 
    #  from the colorBreaks CSV

    csvfile = open(colorBreaksCSV, "r")
    #dialect = csv.Sniffer().sniff(csvfile.read(1024))
    reader = csv.reader(csvfile)
    usedPixels = {}
    header = reader.next() #skip the header
    
    color_dict = {}
    maxVal = -9999
    for row in reader:
        color_dict[float(row[1])] = [row[3], row[4], row[5]]
        if row[2] > maxVal: maxVal = row[2]
        
    #print color_dict
    # Open source file
    src_ds = gdal.Open( input )
    src_band = src_ds.GetRasterBand(1)
    
    # create destination file
    dst_driver = gdal.GetDriverByName('GTiff')
    dst_ds = dst_driver.Create(output_tmp, src_ds.RasterXSize,
                               src_ds.RasterYSize, out_bands, gdal.GDT_Byte)
    
    # create output bands
    band1 = numpy.zeros([src_ds.RasterYSize, src_ds.RasterXSize])
    band2 = numpy.zeros([src_ds.RasterYSize, src_ds.RasterXSize])
    band3 = numpy.zeros([src_ds.RasterYSize, src_ds.RasterXSize])
    
    # set the projection and georeferencing info
    dst_ds.SetProjection( src_ds.GetProjection() )
    dst_ds.SetGeoTransform( src_ds.GetGeoTransform() )
    
    # read the source file
    #gdal.TermProgress( 0.0 )
    for iY in range(src_ds.RasterYSize):
        src_data = src_band.ReadAsArray(0,iY,src_ds.RasterXSize,1)
        col_values = src_data[0] # array of z_values, one per row in source data
        for iX in range(src_ds.RasterXSize):
            z_value = col_values[iX]
            [R,G,B] = MakeColor(z_value, color_dict, maxVal)
            band1[iY][iX] = R
            band2[iY][iX] = G
            band3[iY][iX] = B
    #gdal.TermProgress( (iY+1.0) / src_ds.RasterYSize )
    
    # write each band out
    dst_ds.GetRasterBand(1).WriteArray(band1)
    dst_ds.GetRasterBand(2).WriteArray(band2)
    dst_ds.GetRasterBand(3).WriteArray(band3)

    # Create jpeg or rename tmp file
    
    jpg_driver = gdal.GetDriverByName("JPEG") 
    jpg_driver.CreateCopy(output, dst_ds, 0 ) 
    
    try:
        os.remove(output_tmp)
    except:
        pass
    try:
        GDALClose(output_tmp)
        os.remove(output_tmp)
    except:
        pass
    
    dst_ds = None
    writetolog("    finished running  tif_to_color_jpeg()", True, False)
    return True

def MakeColor(z_value, color_dict, maxVal):

    
    key_list = color_dict.keys()
    key_list.sort()
    while len(key_list) > 0:
        last_val = key_list[-1]
        #print "lastVal =   ",last_val
        #print "ZVal =   ",z_value
        if z_value >= last_val and z_value <= maxVal:
            #"print color for ", z_value, " is ", last_val, " = ", color_dict[last_val]
            return color_dict[last_val]
            break
        else:
            key_list.remove(last_val)

    #if we get here something is wrong return black
    #print "Value not found defaulting to black"
    return [255, 255, 255]
    
def MDSresponseCol(MDSFile):
    csvfile = open(MDSFile, "r")
    reader = csv.reader(csvfile)
    header = reader.next() #store the header
    responseCol = header[2]
    return responseCol 
   
def print_exc_plus( ):
    """ Print the usual traceback information, followed by a listing of
        all the local variables in each frame.
        lifted from the Python Cookbook
    """
    msg = ""
    tb = sys.exc_info( )[2]
    while tb.tb_next:
        tb = tb.tb_next
    stack = [  ]
    f = tb.tb_frame
    while f:
        if r'\sahm\\' in f.f_code.co_filename:
            stack.append(f)
        f = f.f_back
    stack.reverse( )
    traceback.print_exc( )
    msg += "\n" + "Locals by frame, innermost last"
    for frame in stack:
        msg += "\n"
        msg += "\n" + "Frame %s in %s at line %s" % (frame.f_code.co_name,
                                             frame.f_code.co_filename,
                                             frame.f_lineno)
        msg += "\n"
        for key, value in frame.f_locals.items( ):
            msg += "\t%20s = " % key
            # we must _absolutely_ avoid propagating exceptions, and str(value)
            # COULD cause any exception, so we MUST catch any...:
            try:
                msg += str(value)
            except:
                msg += "<ERROR WHILE PRINTING VALUE>"
                
    msg += "\n\n" + ' '.join([str(i) for i in sys.exc_info()[:2]])
    
    return msg

def informative_untrapped_error(instance, name):
    errorMsg = "An error occurred running " + name + ", error message below:  "
    errorMsg += "\n    " + ' '.join([str(i) for i in sys.exc_info()[:2]])
    try:
        errorMsg += traceback.format_tb(sys.exc_info()[2], 10)[-2]
    except IndexError:
        pass
    writetolog(print_exc_plus(), False, False)
    raise ModuleError(instance, errorMsg)

def breakpoint():
    ''' open up the python debugger here and poke around
    Very helpful, I should have figured this out ages ago!
    '''
    QtCore.pyqtRemoveInputHook()
    import pdb; pdb.set_trace()

def writetolog(*args, **kwargs):
    '''Uses the SAHM log file writting function
    but appends our known logfile to the kwargs.
    '''
    global _logger
    _logger.writetolog(*args, **kwargs)

def createLogger(outputdir, verbose):
    global _logger
    _logger = utilities.logger(outputdir, verbose)
    
def getLogger():
    global _logger
    return _logger

def getShortName(fullPathName):
    if fullPathName.endswith('hdr.adf'):
        shortname = os.path.split(fullPathName)[0]
        shortname = os.path.split(shortname)[1]
    else:
        shortname = os.path.split(fullPathName)[1]
        shortname = os.path.splitext(shortname)[0]
    return shortname
if __name__ == '__main__':
    pass
    
    