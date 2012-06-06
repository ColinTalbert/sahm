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

import os, sys, shutil
import traceback
import csv
import time
import tempfile
import subprocess

import struct, datetime, decimal, itertools

import xml.dom.minidom
import textwrap

from PyQt4 import QtCore, QtGui

from core.modules.basic_modules import File, Path, Directory, new_constant, Constant
from core.modules.vistrails_module import ModuleError

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr
import numpy

import packages.sahm.pySAHM.utilities as utilities
import getpass

_roottempdir = ""
_logger = None
r_path = None

def getpixelsize(filename):
    dataset = gdal.Open(filename, gdalconst.GA_ReadOnly)
    xform  = dataset.GetGeoTransform()
    return xform[1]

def getrasterminmax(filename):
    dataset = gdal.Open(filename, gdalconst.GA_ReadOnly)
    band = dataset.GetRasterBand(1)
    
    min = band.GetMinimum()
    max = band.GetMaximum()
    
    try:
        #our output rasters have approx nodata values
        #which don't equal the specified nodata.
        #set the specified to equal what is actually used.
        if (abs(band.GetNoDataValue() - band.GetMinimum()) < 1e-9 and 
            band.GetNoDataValue() <> band.GetMinimum()):
            band.SetNoDataValue(band.GetMinimum)
        
        if min is None or max is None:
            (min,max) = band.ComputeRasterMinMax(1)
        
    except:
        pass
    return (min, max)
    dataset = None

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
            try:
                f_seq = int(f.replace(prefix, ''))
                if f_seq > seq:
                    seq = f_seq
            except ValueError:
                #someone has renamed a folder to a non-numeric string
                pass 
    seq += 1
    dirname = prefix + str(seq)
    os.mkdir(os.path.join(directory, dirname))
    return os.path.join(directory, dirname)

def setrootdir(session_dir):
    global _roottempdir
    _roottempdir = session_dir
    

def getrootdir():
    global _roottempdir
    return _roottempdir

def createrootdir(rootWorkspace):
    '''Creates a session Directory which will
    contain all of the output produced in a single
    VisTrails/Sahm session.
    '''
    global _roottempdir
    user_nospace = getpass.getuser().split(' ')[0]
    _roottempdir = os.path.join(rootWorkspace, user_nospace + '_' + time.strftime("%Y%m%dT%H%M%S"))
    if not os.path.exists(_roottempdir):
        os.makedirs(_roottempdir) 

    return _roottempdir

def map_ports(module, port_map):
    args = {}
    for port, (flag, access, required) in port_map.iteritems():
        if required or module.hasInputFromPort(port):
            value = module.forceGetInputListFromPort(port)
            if len(value) > 1:
                raise ModuleError(module, 'Multiple items found from Port ' + 
                    port + '.  Only single entry handled.  Please remove extraneous items.')
            elif len(value) == 0:
                try:
                    value = eval([item for item in module._input_ports if item[0] == port][0][2]['defaults'])[0]
                except:
                    raise ModuleError(module, 'No items found from Port ' + 
                        port + '.  Input is required.')
            else:
                value = module.forceGetInputFromPort(port)
                
            if access is not None:
                value = access(value)
            if isinstance(value, File) or \
                        isinstance(value, Directory) or \
                        isinstance(value, Path):
                value = path_port(module, port)
            args[flag] = value
    return args

def path_port(module, portName):
    value = module.forceGetInputListFromPort(portName)
    if len(value) > 1:
        raise ModuleError(module, 'Multiple items found from Port ' + 
                          portName + '.  Only single entry handled.  Please remove extraneous items.')
    value = value[0]
    path = value.name 
    path.replace("/", os.path.sep)
    if os.path.exists(path):
        return path
    else:
        raise RuntimeError, 'The indicated file or directory, ' + \
            path + ', does not exist on the file system.  Cannot continue!'
    
def PySAHM_instance_params(instance, mappedPorts):
    global _logger
    instance.__dict__['logger'] = _logger
    instance.__dict__['verbose'] = _logger.verbose
    for k, v in mappedPorts.iteritems():
            instance.__dict__[k] = v
    
def R_boolean(value):
    if value:
        return 'TRUE'
    else:
        return 'FALSE'

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

def getRasterName(fullPathName):
    if fullPathName.endswith('hdr.adf'):
        rastername = os.path.split(fullPathName)[0]
    else:
        rastername = fullPathName
    return rastername

def getModelsPath():
    return os.path.join(os.path.dirname(__file__), "pySAHM", "Resources", "R_Modules")

def runRScript(script, args, module=None):
    global r_path
    program = '"' + os.path.join(r_path, "i386", "Rterm.exe") + '"' #-q prevents program from running
    scriptFile = '"' + os.path.join(getModelsPath(), script) + '"' 
    
    command = program + " --vanilla -f " + scriptFile + " --args " + args
    
    writetolog("\nStarting R Processing of "  + script , True)
    writetolog("    args: " + args, False, False)
    writetolog("    command: " + command, False, False)
    p = subprocess.Popen(command, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    ret = p.communicate()
    
    if 'Error' in ret[1]:
        msg = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        msg +="\n  An error was encountered in the R script for this module."
        msg += "\n     The R error message is below: \n"
        msg += ret[1]
        writetolog(msg)

    if 'Warning' in ret[1]:
        msg = "The R scipt returned the following warning(s).  The R warning message is below - \n"
        msg += ret[1]
        writetolog(msg)

    if 'Error' in ret[1]:
        #also write the errors to a model specific log file in the model output dir
        #then raise an error
        writeRErrorsToLog(args, ret)
        if module:
            raise ModuleError(module, msg)
        else:
            raise RuntimeError , msg
    elif 'Warning' in ret[1]:
        writeRErrorsToLog(args, ret)
        

    del(ret)
    writetolog("\nFinished R Processing of " + script, True)

def writeRErrorsToLog(args, ret):
    #first check that this is a model run, or has a o= in the args.
    #If so write the output log file in the directory
    argsSplit = args.split()
    output = [val.split("=")[1] for val in argsSplit if val.startswith("o=")][0][1:-1]
    if os.path.isdir(output):
        pass
    elif os.path.isdir(os.path.split(output)[0]):
        output = os.path.split(output)[0]
    else:
        return False
    
    outFileN = os.path.join(output, "errorLogFile.txt")
    outFile = open(outFileN, "w")
    outFile.write("standard out:\n\n")
    outFile.write(ret[0] + "\n\n\n")
    outFile.write("standard error:\n\n")
    outFile.write(ret[1])
    outFile.close()

def merge_inputs_csvs(inputCSVs_list, outputFile):
    oFile = open(outputFile, "wb")
    outputCSV = csv.writer(oFile)
    outputCSV.writerow(["PARCOutputFile", "Categorical",
                         "Resampling", "Aggregation", "OriginalFile"])
    for inputCSV in inputCSVs_list:
        iFile = open(inputCSV, "rb")
        inputreader = csv.reader(iFile)
        inputreader.next()
        for row in inputreader:
            outputCSV.writerow(row)
        iFile.close()
    oFile.close()

def applyMDS_selection(oldMDS, newMDS):
    """Takes a selection from a previous MDS and '
        applies it to a new MDS file.
    """
    oldvals = {}
    if os.path.exists(oldMDS):
        iFile = open(oldMDS, 'r')
        previousout = csv.reader(iFile)
        oldheader1 = previousout.next()
        oldheader2 = previousout.next()
        oldvals = dict(zip(oldheader1, oldheader2))
        iFile.close()
        del previousout
        
    tmpnewMDS = newMDS + ".tmp.csv"
    oFile = open(tmpnewMDS, "wb")
    tmpOutCSV = csv.writer(oFile)
    iFile = open(newMDS, "rb")
    outCSV = csv.reader(iFile)
    
    oldHeader1 = outCSV.next()
    oldHeader2 = outCSV.next()
    oldHeader3 = outCSV.next()
    
    newHeader2 = oldHeader2[:3]
    for val in (oldHeader1[3:]):
        if oldvals.has_key(val) and \
        oldvals[val] == '0':
            newHeader2.append('0')
        else:
            newHeader2.append('1')
            
    tmpOutCSV.writerow(oldHeader1)
    tmpOutCSV.writerow(newHeader2)    
    tmpOutCSV.writerow(oldHeader3)
    for row in outCSV:
        tmpOutCSV.writerow(row)
    
    iFile.close()
    oFile.close()
    shutil.copyfile(tmpnewMDS, newMDS)
    os.remove(tmpnewMDS)
    


#taken from http://code.activestate.com/recipes/362715-dbf-reader-and-writer/
def dbfreader(f):
    """Returns an iterator over records in a Xbase DBF file.

    The first row returned contains the field names.
    The second row contains field specs: (type, size, decimal places).
    Subsequent rows contain the data records.
    If a record is marked as deleted, it is skipped.

    File should be opened for binary reads.

    """
    # See DBF format spec at:
    #     http://www.pgts.com.au/download/public/xbase.htm#DBF_STRUCT

    numrec, lenheader = struct.unpack('<xxxxLH22x', f.read(32))    
    numfields = (lenheader - 33) // 32

    fields = []
    for fieldno in xrange(numfields):
        name, typ, size, deci = struct.unpack('<11sc4xBB14x', f.read(32))
        name = name.replace('\0', '')       # eliminate NULs from string   
        fields.append((name, typ, size, deci))
    yield [field[0] for field in fields]
    yield [tuple(field[1:]) for field in fields]

    terminator = f.read(1)
    assert terminator == '\r'

    fields.insert(0, ('DeletionFlag', 'C', 1, 0))
    fmt = ''.join(['%ds' % fieldinfo[2] for fieldinfo in fields])
    fmtsiz = struct.calcsize(fmt)
    for i in xrange(numrec):
        record = struct.unpack(fmt, f.read(fmtsiz))
        if record[0] != ' ':
            continue                        # deleted record
        result = []
        for (name, typ, size, deci), value in itertools.izip(fields, record):
            if name == 'DeletionFlag':
                continue
            if typ == "N":
                value = value.replace('\0', '').lstrip()
                if value == '':
                    value = 0
                elif deci:
                    value = decimal.Decimal(value)
                else:
                    value = int(value)
            elif typ == 'D':
                y, m, d = int(value[:4]), int(value[4:6]), int(value[6:8])
                value = datetime.date(y, m, d)
            elif typ == 'L':
                value = (value in 'YyTt' and 'T') or (value in 'NnFf' and 'F') or '?'
            elif typ == 'F':
                value = float(value)
            result.append(value)
        yield result
    
def getRasterParams(rasterFile):
    """
    Extracts a series of bits of information from a passed raster
    All values are stored in a dictionary which is returned.
    If errors are encountered along the way the error messages will
    be returned as a list in the Error element.
    """
    try:
        #initialize our params dictionary to have None for all parma
        params = {}
        allRasterParams = ["Error", "xScale", "yScale", "width", "height",
                        "ulx", "uly", "lrx", "lry", "Wkt", 
                        "tUlx", "tUly", "tLrx", "tLry", 
                        "srs", "gt", "prj", "NoData", "PixelType"]
        
        for param in allRasterParams:
            params[param] = None
        params["Error"] = []
        
        # Get the PARC parameters from the rasterFile.
        dataset = gdal.Open(rasterFile, gdalconst.GA_ReadOnly)
        if dataset is None:
            params["Error"].append("Unable to open file")
            #print "Unable to open " + rasterFile
            #raise Exception, "Unable to open specifed file " + rasterFile
            
        
        xform  = dataset.GetGeoTransform()
        params["xScale"] = xform[1]
        params["yScale"] = xform[5]

        params["width"]  = dataset.RasterXSize
        params["height"] = dataset.RasterYSize

        params["ulx"] = xform[0]
        params["uly"] = xform[3]
        params["lrx"] = params["ulx"] + params["width"]  * params["xScale"]
        params["lry"] = params["uly"] + params["height"] * params["yScale"]
            
        
    except:
        #print "We ran into problems extracting raster parameters from " + rasterFile
        params["Error"].append("Some untrapped error was encountered")
    finally:
        del dataset
        return params


class InteractiveQGraphicsView(QtGui.QGraphicsView):
    '''
    Extends a QGraphicsView to enable wheel zooming and scrolling
    The main QGraphicsView contains a graphics scene which is dynamically
    
    l_pix - original picture
    c_view - scaled picture
    '''
    def __init__(self, parent=None):
        self.scene = QtGui.QGraphicsScene()
        self.scene.wheelEvent = self.wheel_event
        QtGui.QGraphicsView.__init__(self, self.scene)
        
        self.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
        

    def load_picture(self, strPicture):
        '''This loads and zooms to a new picture
        a new l_pix is created 
        and a c_view is derived from it.
        '''

        self.picture_fname = strPicture
        self.l_pix = QtGui.QPixmap(strPicture)
        if self.size().width()  <= self.size().height(): 
            self.max_vsize = self.size().width() * 0.95
        else: 
            self.max_vsize = self.size().height() * 0.95
            
        self.c_view = self.l_pix.scaled(self.max_vsize, self.max_vsize, 
                                            QtCore.Qt.KeepAspectRatio, 
                                            QtCore.Qt.SmoothTransformation) 
        self.scene.clear()
        self.scene.setSceneRect(0, 0, self.c_view.size().width(), self.c_view.size().height()) 
        self.scene.addPixmap(self.c_view)
        
        self.view_current()
        
   
    def view_current(self):

        self.scene.clear() 
        self.scene.setSceneRect(0, 0, self.c_view.size().width(), self.c_view.size().height())
        self.scene.addPixmap(self.c_view) 
        QtCore.QCoreApplication.processEvents() 
        

    def wheel_event (self, event):

        self.cur_x = self.scene.sceneRect().x()
        self.cur_y = self.scene.sceneRect().y()
        numDegrees = event.delta() / 8 
        numSteps = numDegrees / 15.0 
        self.zoom(numSteps) 
        event.accept() 

    def zoom(self, step):

        zoom_step = 0.06
        self.scene.clear() 
        w = self.c_view.size().width() 
        h = self.c_view.size().height() 
        w, h = w * (1 + zoom_step*step), h * (1 + zoom_step*step) 
        self.c_view = self.l_pix.scaled(w, h, 
                                            QtCore.Qt.KeepAspectRatio, 
                                            QtCore.Qt.SmoothTransformation) 
        self.view_current() 

    def resizeEvent(self, event):
        old_width = event.oldSize().width()
        old_height = event.oldSize().height()
       
        width_prop = self.size().width() / float(old_width)
        height_prop = self.size().height() / float(old_height)
        
        scaled_pic_width = self.c_view.size().width() 
        scaled_pic_height = self.c_view.size().height() 
        
        w = scaled_pic_width * width_prop
        h = scaled_pic_height * height_prop
        if w < 0:
            w = self.size().width()
        if h < 0:
            h = self.size().height()
        
        if self.c_view.size().width() < old_width * 0.9 and \
            self.c_view.size().height() < old_height * 0.9 :
            self.c_view = self.l_pix.scaled(w, h, 
                                                QtCore.Qt.KeepAspectRatio, 
                                                QtCore.Qt.SmoothTransformation)
        self.view_current()
        

