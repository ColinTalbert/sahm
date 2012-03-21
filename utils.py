'''
These are utilites used by the VisTrails 
wrappers of the SAHM command line applications

@author: talbertc
test

'''
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
            #breakpoint()
            value = module.forceGetInputListFromPort(port)
            if len(value) > 1:
                raise ModuleError(module, 'Multiple items found from Port ' + 
                    port + '.  Only single entry handled.  Please remove extraneous items.')
            elif len(value)  == 0:
                try:
                    value = [item for item in module._input_ports if item[0] == port][0][2]['defaults']
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
    program = os.path.join(r_path, "i386", "Rterm.exe") #-q prevents program from running
    scriptFile = os.path.join(getModelsPath(), script)
    
    command = program + " --vanilla -f " + scriptFile + " --args " + args
    
    writetolog("\nStarting R Processing of " + script, True)
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
    

    



#def construct_port_msg(cls, port_name, direction):
#    #get the po
#    global nodes
#    if direction == "in":
#        ports = nodes[cls.__name__].getElementsByTagName("InputPorts")[0]
#    else:
#        ports = nodes[cls.__name__].getElementsByTagName("OutputPorts")[0]
#    
#    port_nodes = ports.getElementsByTagName("Port")
#    for node in port_nodes:
#        if node.getElementsByTagName("PortName")[0].firstChild.data == port_name:
#            port_node = node
#            
#    indent = " "*0
#    nl = "\n" + indent
#    Definition = port_node.getElementsByTagName("Definition")[0].firstChild.data
#    Definition = cleanupstring(Definition, 0, 0)
#    
#    Mandatory = port_node.getElementsByTagName("Manditory")[0].firstChild.data
#    
#    Default = port_node.getElementsByTagName("Default")[0].firstChild.data
#    
#    Options = port_node.getElementsByTagName("Options")[0].getElementsByTagName("Option")
#    
#    Connections = port_node.getElementsByTagName("Connections")[0].getElementsByTagName("Connection")
#    
#    msg = port_name + ":  "
#    if Mandatory.lower() == "true":
#        msg += "(mandatory)\n"
#    elif Mandatory.lower() == "false":
#        msg += "(optional)\n"
#    ":"
#    msg += "\n"+ Definition
#    
#        
#    if Default != "NA":
#        msg += nl +"Default value = " + Default
#        
#    if Options:
#        msg += nl + "\nOptions are:"
#        for Option in Options:
#            msg += "\n" + cleanupstring(Option.firstChild.data, 4, 12)
#            
#    if Connections:
#        msg += nl + "\nCommon connections:"
#        for Connection in Connections:
#            msg += "\n"+ cleanupstring(Connection.firstChild.data, 4, 12)
#
#    return msg
#
#def cleanupstring(str, indent1, indent2):
#    
#    textwidth = 120
#    if str is None:
#        return ""
#    lines = str.split("\n")
#    cleanstr = ""
#    for line in lines:
#        cleanstr += textwrap.fill(line, initial_indent=' '*indent1, subsequent_indent=' '*indent2, width = textwidth) +"\n"
##    str = textwrap.dedent(str)
#    cleanstr = cleanstr[:-1]
#    return cleanstr


#
#
#def construct_port_msg(cls, port_name, direction):
#    global doc_tree
#    
#    
#    
#    
#    
#    
#    key = cls.__name__ + port_name + direction
#    
#    
#    
#    
#    kwargs = port_docs.get(key, False)
#    
#    if kwargs == False:
#        return "Not used and/or defined by SAHM package."
#    
#    msg = "Definition:\n    " + kwargs['Definition'].replace('\\n', '\n').replace('\\t', '\t') 
#    if kwargs['Mandatory'] == "TRUE":
#        msg += "\n\nThis port is Mandatory"
#    else:
#        msg += "\n\nThis port is Optional"
#        
#    if kwargs["Default"] != "NA":
#        msg += "\n\nDefault value = " + kwargs["Default"].replace('\\n', '\n').replace('\\t', '\t')
#        
#    if kwargs['Options'] != "NA":
#        msg += "\n\nOptions are:\n    " + kwargs['Options'].replace('\\n', '\n').replace('\\t', '\t') 
#    msg += "\n\nCommon port connections:\n    " + kwargs['ConnectsTo'].replace('\\n', '\n').replace('\\t', '\t') 
#    return msg
#    