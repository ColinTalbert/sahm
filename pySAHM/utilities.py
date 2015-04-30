#!/usr/bin/python
###############################################################################
#  #
#  # Copyright (C) 2010-2012, USGS Fort Collins Science Center.
#  # All rights reserved.
#  # Contact: talbertc@usgs.gov
#  #
#  # This file is part of the Software for Assisted Habitat Modeling package
#  # for VisTrails.
#  #
#  # "Redistribution and use in source and binary forms, with or without
#  # modification, are permitted provided that the following conditions are met:
#  #
#  #  - Redistributions of source code must retain the above copyright notice,
#  #    this list of conditions and the following disclaimer.
#  #  - Redistributions in binary form must reproduce the above copyright
#  #    notice, this list of conditions and the following disclaimer in the
#  #    documentation and/or other materials provided with the distribution.
#  #  - Neither the name of the University of Utah nor the names of its
#  #    contributors may be used to endorse or promote products derived from
#  #    this software without specific prior written permission.
#  #
#  # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  # AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#  # THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  # PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
#  # CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  # EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  # PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
#  # OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
#  # WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
#  # OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
#  # ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
#  #
#  # Although this program has been used by the U.S. Geological Survey (USGS),
#  # no warranty, expressed or implied, is made by the USGS or the
#  # U.S. Government as to the accuracy and functioning of the program and
#  # related program material nor shall the fact of distribution constitute
#  # any such warranty, and no responsibility is assumed by the USGS
#  # in connection therewith.
#  #
#  # Any use of trade, firm, or product names is for descriptive purposes only
#  # and does not imply endorsement by the U.S. Government.
###############################################################################

import os, sys
import time
import csv
import string
import struct, datetime, decimal, itertools
import pickle

import tempfile

_logfile = ''
_verbose = False

import subprocess
import multiprocessing

_process_pool = None
_pool_processes = []

_roottempdir = ""

from PyQt4 import QtCore, QtGui

from vistrails.core.cache.hasher import sha_hash

mosaicAllTifsInFolder = None

class logger(object):
    def __init__(self, logfile, verbose, write_continued=True):
        self.logfile = logfile
        self.verbose = verbose

        #  if we mistakenly get a output dir instead of a filename
        if os.path.isdir(self.logfile):
            self.logfile = os.path.join(logfile, 'sessionLog.txt')

        if os.path.exists(self.logfile):
            if write_continued:
                self.writetolog("\nSession continued\n", True, True)
        else:
            logDir = os.path.split(self.logfile)[0]
            self.logfile = os.path.join(logDir, 'sessionLog.txt')
            if not os.path.exists(logDir):
                raise RuntimeError("\n".join([logDir, self.logfile, 'Directory of specified logfile does not exist.']))
            f = open(self.logfile, "a")
            del f
            self.writetolog("\nSession started\n", True, False)


    def writetolog(self, msg, addtime=False, printtoscreen=True):
        '''Opens the log file and writes the passed msg.
    Optionally adds a time slot to the message and
    Prints the msg to the screen.
    If the logfile is not specified tries to use the global variable.
        '''
        f = open(self.logfile, "a")
        if self.verbose and printtoscreen:
            print msg
        if addtime:
            msg = ' at: '.join([msg, time.strftime("%m/%d/%Y %H:%M")])
        msg = "\n" + msg
        f.write(msg)
        del f

class TrappedError(Exception):
    """Exception class indicating that an anticipated problem
    was encountered in a specific module
    """
    def __init__(self, msg=None):
        Exception.__init__(self)
        self.message = msg

def isMDSFile(MDSFile):
    '''performs a check to see if a supplied file is in 'MDS' format
    This means:
    1 - The file has three header lines
        first: x, y, responseBinary or responseCount, a series of covariate names, and an optional Split
        second: '0', '0', '0', 0 or blanks and 0 or 1 for each covariate indicate use or ignore
        third: '', '', '', the full path and filename of the tiff covariate layer.
    2 - additionally lines contain individual occurance location, response, covariate attributes at that location, and test\train for the Split column

        Values of -9999 in the response column indicate a background point.
        covariate names ending in '*_categorical' indicate a categorical variable

    The test used in this check are by no means thorough in checking for covariate files
    existing on the file system, missing values in any columns, appropriate values in
    the response or Split columns, etc.  Instead this function is intended to just give a
    best guess as to to if this is an MDS.
    '''
    MDSreader = csv.reader(open(MDSFile, 'r'))
    header1 = MDSreader.next()
    header2 = MDSreader.next()
    header3 = MDSreader.next()

    return True
    del MDSreader

def find_key(dic, val):
    """return the key of dictionary dic given the value
    from: http://www.daniweb.com/software-development/python/code/217019"""
    return [k for k, v in dic.iteritems() if v == val][0]

#  parallelization, remote processing, etc utilites
#  def process_waiter(popen, description, que):
#    '''This needs to be replaced with something that allow
#    '''
#    try:
#        popen.wait()
#    finally:
#        que.put( (description, popen.returncode) )

def runCondorPythonJob(args, workspace, prefix, wholeMachine=False):
    #  replace all mappedDriveLetters in the argsDict with UNC paths
    global UNCDrives
    for item in args:
        args[args.index(item)] = replaceMappedDrives(item)

    if prefix[0].isdigit():
        prefix = "_" + prefix

    #  create submit file
        #  create condorSubmit file
    submitFname = os.path.join(workspace, prefix + "_CondorSubmit.txt")
    submitFile = open(submitFname, 'w')
    submitFile.write("Universe                = vanilla\n")
    submitFile.write("Executable              = c:\Windows\System32\cmd.exe\n")
    submitFile.write("run_as_owner            = true\n")
    submitFile.write("Getenv                  = true\n")
    submitFile.write("Should_transfer_files   = no\n")
    submitFile.write("transfer_executable     = false\n")

    machines = ['igskbacbwsvis1', 'igskbacbwsvis2', 'igskbacbwsvis3', 'igskbacbwsvis4', 'igskbacbws3151a', 'igskbacbws425']
#    machines = ['igskbacbwsvis3']
    reqsStr = 'Requirements            = (Machine == "'
    reqsStr += '.gs.doi.net" || Machine == "'.join(machines) + '.gs.doi.net")'
    if wholeMachine:
        reqsStr += "&& CAN_RUN_WHOLE_MACHINE\n"
        submitFile.write("+RequiresWholeMachine = True\n")
    else:
        reqsStr += "\n"
    submitFile.write(reqsStr)

    stdErrFname = os.path.join(workspace, prefix + "_stdErr.txt")
    stdOutFname = os.path.join(workspace, prefix + "_stdOut.txt")
    logFname = os.path.join(workspace, prefix + "_log.txt")
    submitFile.write("Output                  = " + replaceMappedDrives(stdOutFname) + "\n")
    submitFile.write("error                   = " + replaceMappedDrives(stdErrFname) + "\n")
    submitFile.write("log                     = " + replaceMappedDrives(logFname) + "\n")
    argsStr = 'Arguments               = "/c pushd ' + "'"
    argsStr += "' '".join(args) + "'" + '"\n'
    argsStr = replaceMappedDrives(argsStr)
    argsStr = argsStr.replace(r"\python.exe'", "' && python.exe")
    submitFile.write(argsStr)
    submitFile.write("Notification            = Never\n")
    submitFile.write("Queue\n")
    submitFile.close()

    curDir = os.getcwd()
    os.chdir(os.path.split(curDir)[0])

    #  launch condor job
    DEVNULL = open(os.devnull, 'wb')
    p = subprocess.Popen(["condor_submit", "-n", 'IGSKBACBWSCDRS3', submitFname], stderr=DEVNULL, stdout=DEVNULL)

    os.chdir(curDir)

def replaceMappedDrives(inStr):
    '''This function replaces all instances of each stored drive letter
    in the format  'i:\' with the full unc path.
    Caution should be used with this function, especially with long input
    strings as the combination could occur in other contexts.
    '''
    global UNCDrives
    for drive in UNCDrives.keys():
        inStr = inStr.replace(drive.upper(), UNCDrives[drive])

    return inStr


def storeUNCDrives():
    '''Condor jobs are run on remote computers which are dynamically logged into.
    Since this method does not map the the current users lettered drives we
    needed to implement a means of replacing the mapped drive letters in file names
    with the fullly qualified unc path (ie replacing i:\ with \\igskbacb...\)
    To do this we run the storeUNCDrives once when the sahm package loads.
    This function parses the 'net use' windows command line utility output to
    extract and store the unc paths to all of the currently mapped unc drives.
    '''
    global UNCDrives
    UNCDrives = {}
    try:
        ret = subprocess.Popen(["net", "use"], stdout=subprocess.PIPE,
                               universal_newlines=True).communicate()[0].split("\n")
        for line in ret:
            line = line.split()
            if line and line[0] in ["OK", "Disconnected"]:
                if len(line) > 2 and \
                    os.path.exists(line[2]):
                    UNCDrives[line[1].lower() + '\\'] = line[2] + "\\"
    except:
        #  this is only intended for Condor jobs running
        #  at the Fort Collins Scienc Center.
        #  If this throws any error move along silently.
        pass

def checkIfFolderIsOnNetwork(dirname):
    global UNCDrives
    if not os.path.splitdrive(dirname)[0].lower() + "\\" in UNCDrives.keys():
        QtGui.QMessageBox.critical(None, "Session folder error", "For Fort Condor execution to work your session folder must be on a network drive." \
            + "\nCurrently this is set to:   " + dirname)
        return False
    else:
        return True

#  def waitForProcessesToFinish(processQueue, maxCount=1):
#    '''Given a list of running processes and a maximum number of running processes
#    this function waits for enough of the processes have finished to have
#    the number of running jobs be less that the maximum number of jobs we want.
#    '''
#    while len(processQueue) > maxCount:
#            time.sleep(1)
#            for process in processQueue:
#                if process.poll() is not None:
#                    processQueue.remove(process)

def get_process_count(strProcessingMode):
    '''The number of concurrently running jobs is dependent on the currently
    selected processingMode.
    If on Condor then send them all and let Condor manage the Queue.
    else we will be running n-1 jobs (this function is only used by PARC now)
    '''

    if strProcessingMode == "FORT Condor":
        process_count = multiprocessing.cpu_count() - 1
    elif strProcessingMode == "multiple models simultaneously (1 core each)":
        process_count = multiprocessing.cpu_count() - 1
    else:
        process_count = 1

    if process_count < 1:
        process_count = 1

    return process_count

def getModelsPath():
    return os.path.join(os.path.dirname(__file__), "Resources", "R_Modules")

#  These three functions are used to manage the process pool
def get_pool():
    '''The _pool is a global multiprocessing pool that is used to queue jobs
    '''
    global _process_pool
    return _process_pool

def start_new_pool(processes=1):
    global _process_pool
    if _process_pool:
        _process_pool.terminate()
    _process_pool = multiprocessing.Pool(processes)

def wait_for_pool_to_finish():
    global _process_pool
    global _pool_processes
    for process in _pool_processes:
        process.get()

def add_process_to_pool(worker, arglist):
    '''
    '''
    global _process_pool
    global _pool_processes
    _pool_processes.append(_process_pool.apply_async(worker, arglist))

def convert_list_to_cmd_str(inlist):
    '''return a string equivalent to the command line used.
    notably it escapes quotes and wraps elements with spaces in double quotes.
    '''
    outlist = []
    for item in inlist:
        item = item.replace('"', '\"')
        if ' ' in item:
            item = '"' + item + '"'
        outlist.append(item)
    return " ".join(outlist)


def launch_cmd(cmd, stdout_fname="", stderr_fname="", async=False):
    #  open the text files we'll be writing our stdOut and stdErr to
    if not stdout_fname or not stderr_fname:
        f = tempfile.NamedTemporaryFile(delete=False)
        fname = f.name
        f.close()
        if not stdout_fname:
            stdout_fname = fname + "stdout.txt"
        if not stderr_fname:
            stderr_fname = fname + "stderr.txt"

    stdErrFile = open(stderr_fname, 'a')
    stdErrFile.seek(0, os.SEEK_END)
    stdOutFile = open(stdout_fname, 'a')
    stdOutFile.seek(0, os.SEEK_END)

    p = subprocess.Popen(cmd, stderr=stdErrFile, stdout=stdOutFile)
    if not async:
        p.wait()

    stdErrFile.close()
    stdOutFile.close()
    errMsg = "\n".join(open(stderr_fname, "r").readlines())
    outMsg = "\n".join(open(stdout_fname, "r").readlines())
    return outMsg, errMsg

#  these two functions were pulled from: http://code.activestate.com/recipes/577124-approximately-equal/
def _float_approx_equal(x, y, tol=1e-18, rel=1e-7):
    if tol is rel is None:
        raise TypeError('cannot specify both absolute and relative errors are None')
    tests = []
    if tol is not None: tests.append(tol)
    if rel is not None: tests.append(rel * abs(x))
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
        #  Skip checking for __approx_equal__ in the common case of two floats.
        methodname = '__approx_equal__'
        #  Allow the objects to specify what they consider "approximately equal",
        #  giving precedence to x. If either object has the appropriate method, we
        #  pass on any optional arguments untouched.
        for a, b in ((x, y), (y, x)):
            try:
                method = getattr(a, methodname)
            except AttributeError:
                continue
            else:
                result = method(b, *args, **kwargs)
                if result is NotImplemented:
                    continue
                return bool(result)
    #  If we get here without returning, then neither x nor y knows how to do an
    #  approximate equal comparison (or are both floats). Fall back to a numeric
    #  comparison.
    return _float_approx_equal(x, y, *args, **kwargs)

#  taken from http://code.activestate.com/recipes/362715-dbf-reader-and-writer/
def dbfreader(f):
    """Returns an iterator over records in a Xbase DBF file.

    The first row returned contains the field names.
    The second row contains field specs: (type, size, decimal places).
    Subsequent rows contain the data records.
    If a record is marked as deleted, it is skipped.

    File should be opened for binary reads.

    """
    #  See DBF format spec at:
    #     http://www.pgts.com.au/download/public/xbase.htm#DBF_STRUCT

    numrec, lenheader = struct.unpack('<xxxxLH22x', f.read(32))
    numfields = (lenheader - 33) // 32

    fields = []
    for fieldno in xrange(numfields):
        name, typ, size, deci = struct.unpack('<11sc4xBB14x', f.read(32))
        name = name.replace('\0', '')  #  eliminate NULs from string
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
            continue  #  deleted record
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

def dbfwriter(f, fieldnames, fieldspecs, records):
    """ Return a string suitable for writing directly to a binary dbf file.

    File f should be open for writing in a binary mode.

    Fieldnames should be no longer than ten characters and not include \x00.
    Fieldspecs are in the form (type, size, deci) where
        type is one of:
            C for ascii character data
            M for ascii character memo data (real memo fields not supported)
            D for datetime objects
            N for ints or decimal objects
            L for logical values 'T', 'F', or '?'
        size is the field width
        deci is the number of decimal places in the provided decimal object
    Records can be an iterable over the records (sequences of field values).

    """
    #  header info
    ver = 3
    now = datetime.datetime.now()
    yr, mon, day = now.year - 1900, now.month, now.day
    numrec = len(records)
    numfields = len(fieldspecs)
    lenheader = numfields * 32 + 33
    lenrecord = sum(field[1] for field in fieldspecs) + 1
    hdr = struct.pack('<BBBBLHH20x', ver, yr, mon, day, numrec, lenheader, lenrecord)
    f.write(hdr)

    #  field specs
    for name, (typ, size, deci) in itertools.izip(fieldnames, fieldspecs):
        name = name.ljust(11, '\x00')
        fld = struct.pack('<11sc4xBB14x', name, typ, size, deci)
        f.write(fld)

    #  terminator
    f.write('\r')

    #  records
    for record in records:
        f.write(' ')  #  deletion flag
        for (typ, size, deci), value in itertools.izip(fieldspecs, record):
            if typ == "N":
                value = str(value).rjust(size, ' ')
            elif typ == 'D':
                value = value.strftime('%Y%m%d')
            elif typ == 'L':
                value = str(value)[0].upper()
            else:
                value = str(value)[:size].ljust(size, ' ')
            assert len(value) == size
            f.write(value)

    #  End of file
    f.write('\x1A')

def covariate_name_is_ok(covname):
    '''Checks if the passed string will work for a covariate name
    Must start with a letter and not have any special characters except '.', '_'
    '''
    if not covname[0].isalpha():
        return False
    if not covname.replace(".", "").replace("_", "").replace("\n", "").isalnum():
        return False
    return True

def checkIfModelFinished(model_dir):
    try:
        out_err = os.path.join(model_dir, "stdErr.txt")
        stdErrLines = "\n".join(open(out_err, "r").readlines())
        if "Error" in stdErrLines:
            return "Error in model"
    except:
        pass

    try:
        outText = [file_name for file_name in os.listdir(model_dir)
                                     if file_name.endswith("_output.txt")][0]
    except IndexError:
        return "Starting ..."

    model_text = os.path.join(model_dir, outText)
    try:
        lastLine = open(model_text, 'r').readlines()[-2]
    except IndexError:
        return "Running ..."

    if lastLine.startswith("Total time"):
        return "Completed successfully in " + lastLine[lastLine.find(" = ") + 3:]
    elif lastLine.startswith("Model Failed"):
        return "Error in model"
    else:
        return "Running ..."

def get_picklehash_fname(directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir
    fname = os.path.join(directory, "vt_hashmap.dat")
    return fname

def write_hash_entry_pickle(hashname, fname, directory=""):

    hash_fname = get_picklehash_fname(directory)
    if os.path.exists(hash_fname):
        try:
            with open(hash_fname, "rb") as f:
                hash_dict = pickle.load(f)
                hash_dict[hashname] = fname
        except:
            hash_dict = {hashname:fname}
    else:
        hash_dict = {hashname:fname}

    with open(hash_fname, "wb") as f:
        pickle.dump(hash_dict, f)

def delete_hash_entry_pickle(signature, directory=""):

    hash_fname = get_picklehash_fname(directory)
    if os.path.exists(hash_fname):
        with open(hash_fname, "rb") as f:
            try:
                hash_dict = pickle.load(f)
                del hash_dict[signature]
            except KeyError:
                pass
    else:
        hash_dict = {}

    with open(hash_fname, "wb") as f:
        pickle.dump(hash_dict, f)

def get_fname_from_hash_pickle(hashname, directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir
    fname = get_picklehash_fname(directory)
    if os.path.exists(fname):
        with open(fname, 'rb') as f:
            try:
                hash_dict = pickle.load(f)
                return hash_dict[hashname]
            except:
                #  if anything goes wrong we'll just rerun it!
                return None
    return None

def hash_file(fname):
    h = sha_hash()

    if isinstance(fname, list):
        h = sha_hash()
        for item in fname:
            h.update(open(item, "rb").read(100 * 1024 * 1024))
    else:
        if os.path.exists(str(fname)):
            #  to prevent memory errors and speed up processing the hashing of files
            #  is limited to the first 100Mb of the file.
            #  This could lead to collisions in some cases.
            h.update(open(fname, "rb").read(100 * 1024 * 1024))
        else:
            h.update(str(fname))
    return h.hexdigest()

def get_raster_files(raster_fname):
    if os.path.exists(os.path.join(raster_fname, "hdr.adf")):
        grid_folder = raster_fname
    elif raster_fname.endswith("hdr.adf"):
        grid_folder = os.path.split(raster_fname)[0]
    else:
        return [raster_fname, ]

    return [os.path.join(grid_folder, f) for f in os.listdir(grid_folder)
                                    if f.endswith(".adf")]

def setrootdir(session_dir):
    global _roottempdir
    _roottempdir = session_dir
