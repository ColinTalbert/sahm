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

import os, sys, shutil
import traceback
import csv
import time
import subprocess
import re
import random
import filecmp
import pickle



import numpy as np

from PyQt4 import QtCore, QtGui

try:
    from vistrails.core.cache.hasher import sha_hash
    from vistrails.core.modules.basic_modules import File, Path, Directory, new_constant, Constant, Color
    from vistrails.core.modules.vistrails_module import ModuleError, ModuleSuspended
    from vistrails.core.modules.vistrails_module import ModuleError
    from vistrails.core import system
    from vistrails.gui import application
except:
    from core.cache.hasher import sha_hash
    from core.modules.basic_modules import File, Path, Directory, new_constant, Constant
    from core.modules.vistrails_module import ModuleError, ModuleSuspended
    from core import system
    from gui import application



import numpy

import pySAHM.utilities as utilities
import getpass

_roottempdir = ""
_logger = None
r_path = None
default_seed = 1234

gdalconst = None
gdal = None
osr = None
gdal_merge = None

def importOSGEO():
    global gdalconst
    from osgeo import gdalconst as gdalconst
    global gdal
    from osgeo import gdal as gdal
    global osr
    from osgeo import osr as osr
    global gdal_merge
    from GDAL_Resources.Utilities import gdal_merge as gdal_merge

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
    filename = os.path.join(directory, prefix + str(seq) + suffix)
    file = open(filename, 'w')
    file.close()
    return filename

def get_last_dir(prefix, directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir

    files = os.listdir(directory)
    seq = 0
    found = False
    for f in files:
        if (f.lower().startswith(prefix.lower()) and
            os.path.isdir(os.path.join(directory, f))):
            try:
                f_seq = int(f.lower().replace(prefix.lower(), ''))
                if f_seq > seq:
                    found = True
                    seq = f_seq
            except ValueError:
                #  someone has renamed a folder to a non-numeric string
                pass

    if found:
        return os.path.join(directory, prefix + str(seq))
    return None

def mknextdir(prefix, directory="", skipSequence=False):
    global _roottempdir
    if directory == "":
        directory = _roottempdir

    if skipSequence:
        dirname = os.path.join(directory, prefix)
        if os.path.exists(dirname):
            shutil.rmtree(dirname)
    else:
        files = os.listdir(directory)
        seq = 0
        for f in files:
            if (f.lower().startswith(prefix.lower()) and
                os.path.isdir(os.path.join(directory, f))):
                try:
                    f_seq = int(f.lower().replace(prefix.lower(), ''))
                    if f_seq > seq:
                        seq = f_seq
                except ValueError:
                    #  someone has renamed a folder to a non-numeric string
                    pass
        seq += 1
        dirname = os.path.join(directory, prefix + str(seq))
    os.mkdir(dirname)
    return dirname

def get_picklehash_filename(directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir
    fname = os.path.join(directory, "vt_hashmap_pickle.dat")
    return fname

def get_hash_filename(directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir
    fname = os.path.join(directory, "vt_hashmap.dat")
    return fname

def write_hash_entry(hashname, dirname, directory=""):
    fname = get_hash_filename(directory)
    with open(fname, 'a') as f:
        print >> f, hashname, os.path.basename(dirname)


def write_hash_entry_pickle(hashname, fname, directory=""):

    hash_fname = get_picklehash_filename(directory)
    if os.path.exists(hash_fname):
        with open(hash_fname, "rb") as f:
            hash_dict = pickle.load(f)
            hash_dict[hashname] = fname
    else:
        hash_dict = {hashname:fname}

    with open(hash_fname, "wb") as f:
        pickle.dump(hash_dict, f)

def get_fname_from_hash_pickle(hashname, directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir
    fname = get_picklehash_filename(directory)
    if os.path.exists(fname):
        with open(fname, 'rb') as f:
            hash_dict = pickle.load(f)
            try:
                return hash_dict[hashname]
            except KeyError:
                return None
    return None

#  def get_dir_from_hash_pickle(hashname, directory=""):
#      global _roottempdir
#      if directory == "":
#          directory = _roottempdir
#      fname = get_picklehash_filename(directory)
#      if os.path.exists(fname):
#          with open(fname, 'rb') as f:
#              hash_dict = pickle.load(f)
#              dirname = hash_dict[hashname]['directory']
#              return os.path.join(directory, dirname)
#      return None

def get_dir_from_hash(hashname, directory=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir
    fname = get_hash_filename(directory)
    if os.path.exists(fname):
        with open(fname, 'r') as f:
            for line in f:
                arr = line.strip().split()
                if arr[0] == hashname:
                    return os.path.join(directory, arr[1])
    return None

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
                    port_tuple = [item for item in module._input_ports if item[0] == port][0]
                    port_info = [port_type for port_type in module._input_ports if port_type[0] == port]
                    port_type = re.split("\.|:", port_info[0][1])[-1][:-1]
                    if port_type in ['Float', 'Integer', 'Boolean']:
                        value = eval(eval(port_tuple[2]['defaults'])[0])
                    else:
                        value = eval(port_tuple[2]['defaults'])[0]
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
    path = path.replace("/", os.path.sep)
    if os.path.exists(path):
        return path
    elif os.path.exists(getFileRelativeToCurrentVT(path, module)):
        return getFileRelativeToCurrentVT(path, module)
    else:
        raise RuntimeError, 'The indicated file or directory, ' + \
            path + ', does not exist on the file system.  Cannot continue!'

def PySAHM_instance_params(instance, mappedPorts):
    global _logger
    instance.__dict__['logger'] = _logger
    instance.__dict__['verbose'] = _logger.verbose
    for k, v in mappedPorts.iteritems():
            instance.__dict__[k] = v

def vt_color_to_tuple(vtcolor):
    try:
        return vtcolor.tuple
    except:
        (random.random(), random.random(), random.random())

def R_boolean(value):
    if value:
        return 'TRUE'
    else:
        return 'FALSE'

def set_seed(value):
    global default_seed
    default_seed = int(value)

def get_seed(value=None):
    global default_seed
    if value:
        return value
    else:
        return default_seed
#          return random.randint(-1 * ((2 ** 32) / 2 - 1), (2 ** 32) / 2 - 1)

def dir_path_value(value, module=None):
    val = getFileRelativeToCurrentVT(value.name, module)
    sep = os.path.sep
    return val.replace("/", sep)

def create_file_module(fname, f=None, module=None):

    if f is None:
        f = File()
    f.name = getFileRelativeToCurrentVT(fname)
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
    header = reader.next()  #  store the header
    responseCol = header[2]
    return responseCol

def checkModelCovariatenames(MDSFile):
    """These R models break if any of the covariate names used
    start with anything other than an alpha character
    contain any special characters besides "." and "_"
    """
    covariates = open(MDSFile, "r").readline().strip().split(",")[3:]
    for covariate in covariates:
        if not utilities.covariate_name_is_ok(covariate):
            return False
    return True

def print_exc_plus():
    """ Print the usual traceback information, followed by a listing of
        all the local variables in each frame.
        lifted from the Python Cookbook
    """
    msg = ""
    tb = sys.exc_info()[2]
    while tb.tb_next:
        tb = tb.tb_next
    stack = [  ]
    f = tb.tb_frame
    while f:
        if r'\sahm\\' in f.f_code.co_filename:
            stack.append(f)
        f = f.f_back
    stack.reverse()
    traceback.print_exc()
    msg += "\n" + "Locals by frame, innermost last"
    for frame in stack:
        msg += "\n"
        msg += "\n" + "Frame %s in %s at line %s" % (frame.f_code.co_name,
                                             frame.f_code.co_filename,
                                             frame.f_lineno)
        msg += "\n"
        for key, value in frame.f_locals.items():
            msg += "\t%20s = " % key
            #  we must _absolutely_ avoid propagating exceptions, and str(value)
            #  COULD cause any exception, so we MUST catch any...:
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
    return getFileRelativeToCurrentVT(rastername)

def gen_R_cmd(script, args_dict):
    '''Formats the cmd used to launch a SAHM R script
    Returns a list of the cmd elements
    '''
    #  get the path to the R exe
    global r_path
    if system.systemType in ['Microsoft', 'Windows']:
        R_exe = getR_application()  #  -q prevents R_exe from running
    else:
        R_exe = r_path

    #  get the path the the SAHM specific R script that we'll be running
    sahm_R_script = os.path.join(utilities.getModelsPath(), script)

    #  reformat are args into the form expected by our R scripts
    args = ["%s=%s" % pair for pair in args_dict.iteritems()]

    if system.systemType in ['Microsoft', 'Windows']:
        command_arr = [R_exe, '--vanilla', '-f', sahm_R_script, "--args"] + args
    else:
        command_arr = [R_exe, '--vanilla', sahm_R_script] + args

    return command_arr

def run_R_script(script, args_dict, module=None, async=False,
               stdout_fname=None, stderr_fname=None, new_r_path=None):
    '''Runs a SAHM R script
    if async is False it waits for the script to finish processing and checks
    the output for warning and error messages.

    if async is True it launches the script on the global queue and immediately
    returns.

    if stdout_fname or stderr_fname are provided these files will receive the
    output, which is helpful for debugging/logging
    '''
    global r_path
    if not new_r_path is None and \
       os.path.abspath(new_r_path) != r_path:
        set_r_path(new_r_path)

    cmd = gen_R_cmd(script, args_dict)

    writetolog("\nStarting processing of " + script , True)
    writetolog("    command used: \n" + utilities.convert_list_to_cmd_str(cmd), False, False)

    #  run our script
    if async:
        utilities.add_process_to_pool(utilities.launch_cmd,
                                [cmd, stdout_fname, stderr_fname])
        writetolog("\nLaunched asynchronously " + script , True)
    else:
        stdout, stderr = utilities.launch_cmd(cmd, stdout_fname, stderr_fname)
        check_R_output(stdout, stderr, module, args_dict)
        writetolog("\nFinished processing of " + script , True)
        return stdout, stderr

#    runRModelPy = os.path.join(os.path.dirname(__file__), "pySAHM", runner_script)
#    command_arr = [sys.executable, runRModelPy] + command_arr

def check_R_output(stdout, stderr, module=None, args_dict=None):
    #  handle the errors and warnings
    if 'Error' in stderr:
        msg = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        msg += "\n  An error was encountered in the R script for this module."
        msg += "\n     The R error message is below: \n"
        msg += stderr
        writetolog(msg)

    if 'Warning' in stderr:
        msg = "The R scipt returned the following warning(s).  The R warning message is below - \n"
        msg += stderr
        writetolog(msg)

    if 'Error' in stderr:
        #  also write the errors to a model specific log file in the model output dir
        #  then raise an error
        writeRErrorsToLog(args_dict, stdout, stderr)
        if module:
            raise ModuleError(module, msg)
        else:
            raise RuntimeError , msg
    elif 'Warning' in stderr:
        writeRErrorsToLog(args_dict, stdout, stderr)

class ModelJobMonitor(object):
    '''The job monitor object that checks for model run completion and
    errors.  The signal that a model is finished is that the last line
    in the model output text file starts with 'Total time' The signal
    that a model failed is that either the last line starts with
    'Model Failed' or the stderr (which is getting written to a text
    file in the model output directory) contains the word 'Error'.
    This file can contain warning messages.

    '''
    def __init__(self, module, stdout_fname, stderr_fname, output_txt=None):
        self.module = module
        self.stderr_fname = stderr_fname
        self.stdout_fname = stdout_fname
        self.out_fname = output_txt
        self.stderr = ""
        self.stdout = ""
        self.has_error = False

    def finished(self):
        self.store_stdout()
        self.check_for_error()
        return self.check_if_model_finished()

    def store_stdout(self):
        if os.path.exists(self.stdout_fname):
            self.stdout = open(self.stdout_fname, "r").read()

    def check_for_error(self):
        if os.path.exists(self.stderr_fname):
            self.stderr = open(self.stderr_fname, "r").read()
        if "Error" in self.stderr:
            self.has_error = True

    def check_if_model_finished(self):
        if self.out_fname is None:
            #  this script run is not one of our models and doesn't have a
            #  model output file to parse for a 'Total time:' but since these models
            #  are not run asyncronously we can assume it's done now.
            return False

        try:
            lastLine = open(self.out_fname, 'r').readlines()[-2]
        except (IndexError, IOError):
            return False

        if lastLine.startswith("Total time"):
            return True
        elif lastLine.startswith("Model Failed"):
            self.has_error = True
            return True
        else:
            return False

def check_if_modle_finished(model_out_fname):
    '''checks to see if a model output text file indicates the model finished
    '''


def get_job_monitor(module, model_args):
    stdout_fname = os.path.join(model_args['o'], "stdOut.txt")
    stderr_fname = os.path.join(model_args['o'], "stdErr.txt")
    if model_args.has_key('ws'):
        #  this is an ApplyModel not a standard model run!
        model_prefix = os.path.split(os.path.split(model_args['ws'])[0])[1].split("_")[0]
    else:
        model_prefix = os.path.split(model_args['o'])[1].split("_")[0]
    output_txt = os.path.join(model_args['o'], model_prefix + "_output.txt")

    return ModelJobMonitor(module, stdout_fname, stderr_fname, output_txt)

def run_model_script(script, args_dict, module=None, runner_script="runRModel.py"):
    '''Our SAHM R model scripts now require a python wrapper to handle complications
    introduced by multiprocessing.  Additionally these model scripts require
    specific processing depending on the current processing mode.
    '''
    processing_mode = args_dict.get("cur_processing_mode",
                                "single models sequentially (n - 1 cores each)")
    args_dict["multicore"] = R_boolean(not processing_mode ==
                                "multiple models simultaneously (1 core each)")

    stderr_fname = os.path.join(args_dict['o'], "stdErr.txt")
    stdout_fname = os.path.join(args_dict['o'], "stdOut.txt")

    cmd = gen_R_cmd(script, args_dict)
    runRModelPy = os.path.join(os.path.dirname(__file__), "pySAHM", runner_script)
    cmd = [sys.executable, runRModelPy] + cmd

    job_monitor = get_job_monitor(module, args_dict)
    if not job_monitor.finished():
        writetolog("\nStarting processing of " + script , True)
        writetolog("    command used: \n" + utilities.convert_list_to_cmd_str(cmd), False, False)

        if processing_mode == "FORT Condor":
            runModelOnCondor(script, args_dict)
            writetolog("\n R Processing launched using Condor " + script, True)
            raise ModuleSuspended(module, 'Model running on Condor',
                                  queue=job_monitor)
        else:
            #  utilities.add_process_to_pool(utilities.launch_cmd,
            #                         [cmd, stdout_fname, stderr_fname])
            out_msg, err_msg = utilities.launch_cmd(cmd, stdout_fname,
                                                    stderr_fname, True)
            writetolog("\n R Processing launched asynchronously " + script,
                       True)
            raise ModuleSuspended(module, 'Model running asynchronously',
                                  queue=job_monitor)
    else:
        check_R_output(job_monitor.stdout, job_monitor.stderr,
                       module, args_dict)


def runModelOnCondor(script, args_dict, command_arr):
    #  copy MDS file and convert all refs to K:, I:, N:, J: to UNC paths
    mdsDir, mdsFile = os.path.split(args_dict["c"])
    newMDSfname = os.path.join(args_dict['o'], mdsFile)
    newMDSFile = open(newMDSfname, 'w')
    oldLines = open(args_dict["c"], 'r').readlines()
    for headerLine in oldLines[:3]:
        newMDSFile.write(utilities.replaceMappedDrives(headerLine))

    newMDSFile.writelines(oldLines[3:])

    os.chdir(args_dict['o'])

    mdsArgIndex = command_arr.index('c=' + args_dict["c"])
    command_arr[mdsArgIndex] = 'c=' + newMDSfname
    for index in range(len(command_arr)):
        command_arr[index] = utilities.replaceMappedDrives(command_arr[index])

    #  create condorSubmit file
    submitFname = os.path.join(args_dict['o'], "modelSubmit.txt")
    submitFile = open(submitFname, 'w')
    submitFile.write("Universe                = vanilla\n")
    submitFile.write("Executable              = c:\Windows\System32\cmd.exe\n")
    submitFile.write("run_as_owner            = true\n")
    submitFile.write("Getenv                  = true\n")
    submitFile.write("Should_transfer_files   = no\n")
    submitFile.write("transfer_executable     = false\n")

    machines = ['igskbacbwsvis1', 'igskbacbwsvis2', 'igskbacbwsvis3', 'igskbacbwsvis4', 'igskbacbws3151', 'igskbacbws425']
    reqsStr = 'Requirements            = (Machine =="'
    reqsStr += '.gs.doi.net"||Machine =="'.join(machines) + '.gs.doi.net")\n'
    submitFile.write(reqsStr)
    stdErrFname = os.path.join(args_dict['o'], "stdErr.txt")
    stdOutFname = os.path.join(args_dict['o'], "stdOut.txt")
    logFname = os.path.join(args_dict['o'], "log.txt")
    submitFile.write("Output                  = " + utilities.replaceMappedDrives(stdOutFname) + "\n")
    submitFile.write("error                   = " + utilities.replaceMappedDrives(stdErrFname) + "\n")
    submitFile.write("log                     = " + utilities.replaceMappedDrives(logFname) + "\n")
    argsStr = 'Arguments               = "/c pushd ' + "'"
    argsStr += "' '".join(command_arr) + "'" + '"\n'
    argsStr = utilities.replaceMappedDrives(argsStr)
    argsStr = argsStr.replace(r"\python.exe'", "' && python.exe")
    submitFile.write(argsStr)
    submitFile.write("+RequiresWholeMachine = True\n")
    submitFile.write("Notification            = Never\n")
    submitFile.write("Queue\n")
    submitFile.close()

    #  launch condor job
    DEVNULL = open(os.devnull, 'wb')
    p = subprocess.Popen(["condor_submit", "-n", 'IGSKBACBWSCDRS3', submitFname], stderr=DEVNULL, stdout=DEVNULL)

def get_r_path():
    global r_path
    return str(r_path)

def  set_r_path(r_bin_path):
    global r_path
    if os.path.exists(os.path.abspath(r_bin_path)):
        r_bin_path = os.path.abspath(r_bin_path)
    r_path = str(r_bin_path)

def getR_application(module=None):
    global r_path
    #  are we in 64 or 32 bit?  If 64 use the 64 bit install of R otherwise 32 bit.
    #  if we don't have the matching version and the other exists use it.
    version_dirs = ["i386", "x64"]
    possible_exes = [os.path.join(r_path, version_dir, "Rterm.exe") for version_dir in version_dirs]
    if sys.maxsize > 2 ** 32 and os.path.exists(possible_exes[1]):
        program = possible_exes[1]
    elif os.path.exists(possible_exes[0]):
        program = possible_exes[0]
    elif os.path.exists(possible_exes[1]):
        program = possible_exes[1]
    else:
        #  no R exe found we can't go on
        msg = "No R executable found.\nPlease check the install folder:  "
        msg += r_path + "\nfor either a ..\i386\Rterm.exe or a ..\x64\Rterm.exe"
        if module:
            raise ModuleError(module, msg)
        else:
            raise RuntimeError , msg

    return program

def pull_R_install_from_reg():
    #  searches in the registry for an installation of R and returns the path
    #  to the bin folder within it if that folder exists
    regCmds = [r'reg query "HKEY_LOCAL_MACHINE\SOFTWARE\R-core\R" /v "InstallPath"',
               r'reg query "HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\R-core\R" /v "InstallPath"']
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    for regCmd in regCmds:
        regValue = subprocess.Popen(regCmd, startupinfo=startupinfo, stdout=subprocess.PIPE).stdout.read()

        for line in regValue.split("\n"):
            if line.strip() and os.path.isdir(line.split("    ")[-1].strip()):
                R_path = os.path.abspath(os.path.join(line.split("    ")[-1].strip(), "bin"))
                if os.path.exists(R_path):
                    msg = "Using the autodetected R installation location found in the registry:\n"
                    msg += R_path
                    writetolog(msg, True, True)

                    return R_path

    msgbox = QtGui.QMessageBox()
    msgbox.setText("SAHM is unable to autodetect an installation of R on this machine\nYou must manually set the 'r_path' configuration value\n\nSee the SAHM installation section of the user manual for details.")
    msgbox.exec_()
    return "R not found!"

def find_java_exe(java_bin):
    '''Used on windows to check if we can execute java based on the
    java_bin file they have selected.
    The default is 'java' which works if java is installed correctly and on the path
    if they have specified something else that we can get to, we're good as well.
    If not look for an environmental variable 'java_home'
    otherwise look in c:\program files\java or C:\Program Files (x86)\Java
    '''
    tryed_locs = [java_bin]
    try:
        p = subprocess.Popen(java_bin, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out, err = p.communicate()
        if err.startswith('Usage: java'):
            #  Fantastic, what they have or the default is legit
            return java_bin
    except WindowsError:
        pass

    #  Too bad what they have didn't work as we expected
    try:
        java_home = os.environ['JAVA_HOME']
        java_exe = os.path.join(java_home, 'bin', 'java.exe')
        tryed_locs.append(java_exe)
        if os.path.exists(java_exe):
            return os.path.abspath(java_exe)
    except KeyError, IndexError:
        pass

    for java_dir in [r"C:\Program Files (x86)\java", r"C:\Program Files\java"]:
        jre_folders = [os.path.join(java_dir, j) for j in os.listdir(java_dir)
                if j.lower().startswith('jre') or j.lower().startswith('jdk')]
        tryed_locs.append(java_dir)
        try:
            java_exe = os.path.join(jre_folders[-1], "bin", 'java.exe')

            if os.path.exists(java_exe):
                return os.path.abspath(java_exe)
        except IndexError:
            pass

    #  we have gone above and beyond trying to find java on this system
    #  it is time to give up and alert the user.
    msg = "The current java_path does not appear to be a valid instalation of java.\n"
    msg += "SAHM is also unable to autodetect an installation of java on this machine.\n"
    msg += "\nLocations searched for java:"
    for loc in tryed_locs:
        msg += "\n\t" + loc
    msg += "\nThis only affects the Maxent model."
    msg += "\nSee the SAHM installation section of the user manual for details."

    print msg
    return java_bin



def writeRErrorsToLog(args, outMsg, errMsg):
    #  first check that this is a model run, or has a o= in the args.
    #  If so write the output log file in the directory
    if os.path.isdir(args["o"]):
        outputfolder = args["o"]
    elif os.path.isdir(os.path.split(args["o"])[0]):
        outputfolder = os.path.split(args["o"])[0]
    else:
        return False

    outFileN = os.path.join(outputfolder, "errorLogFile.txt")
    outFile = open(outFileN, "w")
    outFile.write("standard out:\n\n")
    outFile.write(outMsg + "\n\n\n")
    outFile.write("standard error:\n\n")
    outFile.write(errMsg)
    outFile.close()

def merge_inputs_csvs(input_csvs, outputFile):
    '''This function takes a list of inputCSV and merges them into a single
    file.  The template from the first will be used
    '''
    #  get the first template specified
    templatefname = "None specified"
    for input_csv in input_csvs:
        infile1 = open(input_csv, "rb")
        infile1csv = csv.reader(infile1)
        firstline = infile1csv.next()
        templatefname = firstline[-2]
        if getFileRelativeToCurrentVT(templatefname):
            infile1.close()
            break
        infile1.close()

    #  open a csv we will write all the outputs into
    oFile = open(outputFile, "wb")
    outputCSV = csv.writer(oFile)
    outputCSV.writerow(["PARCOutputFile", "Categorical",
                         "Resampling", "Aggregation", "OriginalFile"])

    #  write all the inputs out to this file
    for inputCSV in input_csvs:
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
#
#
#
#
#
#  def getRasterParams(rasterFile):
#      """
#      Extracts a series of bits of information from a passed raster
#      All values are stored in a dictionary which is returned.
#      If errors are encountered along the way the error messages will
#      be returned as a list in the Error element.
#      """
#      try:
#          #  initialize our params dictionary to have None for all parma
#          params = {}
#          allRasterParams = ["Error", "xScale", "yScale", "width", "height",
#                          "ulx", "uly", "lrx", "lry", "Wkt",
#                          "tUlx", "tUly", "tLrx", "tLry",
#                          "srs", "gt", "prj", "NoData", "PixelType"]
#
#          for param in allRasterParams:
#              params[param] = None
#          params["Error"] = []
#
#          #  Get the PARC parameters from the rasterFile.
#          dataset = gdal.Open(rasterFile, gdalconst.GA_ReadOnly)
#          if dataset is None:
#              params["Error"].append("Unable to open file")
#              #  print "Unable to open " + rasterFile
#              #  raise Exception, "Unable to open specifed file " + rasterFile
#
#
#          xform = dataset.GetGeoTransform()
#          params["xScale"] = xform[1]
#          params["yScale"] = xform[5]
#
#          params["width"] = dataset.RasterXSize
#          params["height"] = dataset.RasterYSize
#
#          params["ulx"] = xform[0]
#          params["uly"] = xform[3]
#          params["lrx"] = params["ulx"] + params["width"] * params["xScale"]
#          params["lry"] = params["uly"] + params["height"] * params["yScale"]
#
#
#      except:
#          #  print "We ran into problems extracting raster parameters from " + rasterFile
#          params["Error"].append("Some untrapped error was encountered")
#      finally:
#          del dataset
#          return params


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
        if self.size().width() <= self.size().height():
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
        w, h = w * (1 + zoom_step * step), h * (1 + zoom_step * step)
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


def print_timing(func):
    def wrapper(*args, **kwargs):
        t1 = time.time()
        res = func(*args, **kwargs)
        t2 = time.time()
#        print "len(traceback.extract_stack())", len(traceback.extract_stack())
        tabs = "  ...  " * (len(traceback.extract_stack()) - 20)
#        print 'tabs', tabs, "end"
        print tabs, '%s took %0.3f ms' % (func.func_name, (t2 - t1) * 1000.0)
        return res
    return wrapper

def check_if_model_finished(model_dir):

    try:
        outText = find_file(model_dir, "_output.txt")
    except RuntimeError:
        return False

    model_text = os.path.join(model_dir, outText)
    try:
        lastLine = open(model_text, 'r').readlines()[-2]
    except IndexError:
        return False

    if lastLine.startswith("Total time"):
        return True
    elif lastLine.startswith("Model failed"):
        return "Error"
    else:
        return False

def find_file(model_dir, suffix):
    try:
        return [file_name for file_name in os.listdir(model_dir)
                                 if file_name.lower().endswith(suffix.lower())][0]
    except IndexError:
        raise RuntimeError('The expected model output '
                               + suffix + ' was not found in the model output directory')


def mosaicAllTifsInFolder(inDir, outFileName):
    onlyfiles = [os.path.join(inDir, f) for f in os.listdir(inDir)
            if os.path.isfile(os.path.join(inDir, f)) and f.endswith(".tif") ]
    args = ["placeholder", "-o", outFileName] + onlyfiles
    gdal.DontUseExceptions()
    gdal_merge.main(args)


def waitForProcessesToFinish(processQueue, maxCount=1):
    while len(processQueue) >= maxCount:
            time.sleep(1)
            for process in processQueue:
                if process.poll() is not None:
                    processQueue.remove(process)

def getParentDir(f, x=None):
    parentdirf = os.path.dirname(f.name)
    return create_dir_module(parentdirf)

def get_filename_relative(f):
    return getFileRelativeToCurrentVT(f.name)

def getFileRelativeToCurrentVT(fname, curModule=None):
    #  This is three step approach:
    #  step 1: if fname exists assume it's the one we want and return it.
    #  step 2: Look for the file relative to the current VT.
    #        In effect loop through all the sibling and descendant folders
    #        of the vt file's parent directory and look for the base filename in each.
    #        If we find an identically named file hope for the best and return it.
    #  step 3: Do what we did in step 2 but relative to the current session folder.
    #
    #  If no fname is found in the above three steps raise an error.
    def couldntFindFile():
        msg = "Could not find file: " + fname + "\nPlease point to valid location for this file."
        if curModule is None:
            raise Exception(msg)
        else:
            raise ModuleError(curModule, msg)

    try:
        fname = fname.replace ("\\", "/")
        #  step 1
        if os.path.exists(fname):
            return fname

        #  step 2 (and then step3)
        try:
            app = application.get_vistrails_application()()
            curlocator = app.get_vistrail().locator.name
            curVTdir = os.path.split(curlocator)[0]
        except:
            curVTdir = ""

        root_dir, justfname = os.path.split(fname)
        if justfname.lower() == "hdr.adf":
            justfname = os.path.sep.join([os.path.split(root_dir)[1], justfname])
        for rootdir in [curVTdir, getrootdir()]:
            if os.path.exists(os.path.join(rootdir, justfname)):
                return os.path.join(rootdir, justfname)
            for root, dirnames, filenames in os.walk(rootdir):
                for dirname in dirnames:
                    if os.path.exists(os.path.join(root, dirname, justfname)):
                        return os.path.join(root, dirname, justfname)

        #  we did our best but couldn't find the file
        couldntFindFile()

    except Exception, e:
        #  if something goes wrong we couldn't find the file throw an error
        couldntFindFile()

def compare_files(f1, f2):
    return filecmp.cmp(f1, f2, shallow=False)

def compare_mds(mds1, mds2):
    '''checks if the two mds files entered are identical
    ignores the path components so that relative paths will work
    '''
    orig_mds = np.genfromtxt(mds1, dtype='S25', delimiter=",")
    new_mds = np.genfromtxt(mds2, dtype='S25', delimiter=",")
    orig_mds[1, :2] = 'skip'
    new_mds[1, :2] = 'skip'
    orig_mds[2] = 'skip'
    new_mds[2] = 'skip'
    #  np.array_equiv(orig_mds, new_mds) doesn't work on string types in this np version
    return np.count_nonzero(np.logical_not(orig_mds == new_mds)) == 0

def make_next_file_complex(curModule, prefix, suffix="", directory="",
                          key_inputs=[], file_or_dir='file'):
    '''How we're handling file can lead to some unanticipated results.
    To handle this we want to re-use file names if
    1) all ports are identical
    2) key input files we want to monitor for changes
    '''
    h = sha_hash()
    h.update(curModule.signature)
    for key in sorted(curModule.inputPorts):
        if curModule.hasInputFromPort(key):
            h.update(bytes(curModule.getInputFromPort(key)))

    for input in key_inputs:
        h.update(input + hash_file(input))

    signature = h.hexdigest()

    fname = get_fname_from_hash_pickle(signature)

    if fname is None or not os.path.exists(fname):
        if file_or_dir == 'file':
            fname = mknextfile(prefix=prefix, suffix=suffix, directory=directory)
        else:
            fname = mknextdir(prefix=prefix)
        write_hash_entry_pickle(signature, fname)
        already_run = False
    else:
        already_run = True

    return fname, already_run


def hash_file(fname):
    h = sha_hash()
    h.update(open(fname, "rb").read())
    return h.hexdigest()
