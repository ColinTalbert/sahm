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
import math


import numpy as np

from PyQt4 import QtCore, QtGui

from vistrails.core.cache.hasher import sha_hash
import vistrails.api as api
from vistrails.core.modules.basic_modules import File, Path, Directory, PathObject
from vistrails.packages.spreadsheet.basic_widgets import CellLocation
from vistrails.packages.spreadsheet.spreadsheet_base import StandardSheetReference
from vistrails.core.modules.vistrails_module import ModuleError, ModuleSuspended
from vistrails.core import system
from vistrails.gui import application

import numpy

import pySAHM.utilities as utilities
from CreatePredictorCurves import CreatePredictorCurvesDialog

write_hash_entry_pickle = utilities.write_hash_entry_pickle
delete_hash_entry_pickle = utilities.delete_hash_entry_pickle
get_fname_from_hash_pickle = utilities.get_fname_from_hash_pickle
hash_file = utilities.hash_file
get_raster_files = utilities.get_raster_files

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

def mknextfile(prefix, suffix="", directory="", subfolder="", runname=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir

    if subfolder:
        directory = os.path.join(directory, subfolder)

    files = os.listdir(directory)

    if runname:
        prefix = prefix + "_" + runname + "_"
    else:
        prefix = prefix + "_"

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

def get_last_dir(prefix, directory="", subfolder="", name=""):
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

def mknextdir(prefix, directory="", skipSequence=False, subfolder="", runname=""):
    global _roottempdir
    if directory == "":
        directory = _roottempdir

    if subfolder:
        directory = os.path.join(directory, subfolder)

    if runname:
        prefix = prefix + "_" + runname + "_"
    else:
        prefix = prefix + "_"

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



def setrootdir(session_dir):
    global _roottempdir
    _roottempdir = session_dir
    utilities.setrootdir(session_dir)

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
                        isinstance(value, Path) or\
                        isinstance(value, PathObject):
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

        for out_fname in [self.out_fname,
                          os.path.join(os.path.dirname(self.out_fname), "ApplyModel_output.txt")]:
            try:
                lastLine = open(out_fname, 'r').readlines()[-2]
            except (IndexError, IOError):
                return False

            if lastLine.startswith("Total time"):
                return True
            elif self.has_error:
                return True
            elif lastLine.startswith("Model Failed"):
                self.has_error = True
                return True

        return False

def get_job_monitor(module, model_args):
    stdout_fname = os.path.join(model_args['o'], "stdOut.txt")
    stderr_fname = os.path.join(model_args['o'], "stdErr.txt")
    if model_args.has_key('ws'):
        #  this is an ApplyModel not a standard model run!
        model_prefix = os.path.split(os.path.split(model_args['ws'])[0])[1].split("_")[0]
    else:
        model_prefix = os.path.split(model_args['o'])[1].split("_")[0]
     
    if model_prefix == "udc":
        model_prefix = "hsc"
        
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

        if module.abbrev == "udc":
            orig_mds = args_dict['c'].replace(".csv", "_orig.csv")
            shutil.copyfile(args_dict['c'], orig_mds)

            json_fname = os.path.join(module.output_dname, 'udc.json')
            kwargs_mod = {'inputMDS':args_dict['c'],
                      'output_json':json_fname}
            args_dict['udc'] = json_fname
            cmd.append("udc=" + json_fname)
            dialog = CreatePredictorCurvesDialog(kwargs_mod)
            #  dialog.setWindowFlags(QtCore.Qt.WindowMaximizeButtonHint)
            retVal = dialog.exec_()
            #  outputPredictorList = dialog.outputList
            if retVal == 1:
                raise ModuleError(module, "Cancel or Close selected (not OK) workflow halted.")

        if processing_mode == "FORT Condor":
            runModelOnCondor(script, args_dict)
            writetolog("\n R Processing launched using Condor " + script, True)
            raise ModuleSuspended(module, 'Model running on Condor',
                                  queue=job_monitor)
        else:
            utilities.add_process_to_pool(utilities.launch_cmd,
                                   [cmd, stdout_fname, stderr_fname])
#              out_msg, err_msg = utilities.launch_cmd(cmd, stdout_fname,
#                                                      stderr_fname, True)
            writetolog("\n R Processing launched asynchronously " + script,
                       True)
            raise ModuleSuspended(module, 'Model running asynchronously',
                                  handle=job_monitor)
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

    return "SAHM is unable to autodetect an installation of R on this machine\nYou must manually set the 'r_path' configuration value\n\nSee the SAHM installation section of the user manual for details."

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
        tryed_locs.append(java_dir)
        try:
            jre_folders = [os.path.join(java_dir, j) for j in os.listdir(java_dir)
                if j.lower().startswith('jre') or j.lower().startswith('jdk')]
            java_exe = os.path.join(jre_folders[-1], "bin", 'java.exe')
            if os.path.exists(java_exe):
                return os.path.abspath(java_exe)
        except:
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
        if len(firstline) > 4:
            templatefname = firstline[-2]
            if getFileRelativeToCurrentVT(templatefname):
                infile1.close()
                break
        else:
            templatefname = None
        infile1.close()

    #  open a csv we will write all the outputs into
    oFile = open(outputFile, "wb")
    outputCSV = csv.writer(oFile)
    if templatefname:
        outputCSV.writerow(["PARCOutputFile", "Categorical",
                         "Resampling", "Aggregation", "OriginalFile", templatefname, templatefname])
    else:
        outputCSV.writerow(["PARCOutputFile", "Categorical",
                         "Resampling", "Aggregation", "OriginalFile"])

    #  write all the inputs out to this file
    for inputCSV in input_csvs:
        iFile = open(inputCSV, "rb")
        inputreader = csv.reader(iFile)
        inputreader.next()
        for row in inputreader:
            try:
                outputCSV.writerow([getFileRelativeToCurrentVT(row[0]), row[1], row[2], row[3], row[6]])
            except:
                outputCSV.writerow([getFileRelativeToCurrentVT(row[0]), row[1], row[2], row[3]])
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

def convert_tom(old_f, new_module):
    controller = api.get_current_controller()
    param = old_f.parameters[0]
    param_value = param.strValue
    alias = param.alias

    new_val_lookup = ["Threshold=0.5", "Sensitivity=Specificity",
                       "Maximizes (sensitivity+specificity)/2",
                       "Maximizes Cohen's Kappa",
                       "Maximizes PCC (percent correctly classified)",
                       "Predicted prevalence=observed prevalence",
                       "Threshold=observed prevalence",
                       "Mean predicted probability",
                       "Minimizes distance between ROC plot and (0,1)",
                       ]

    if param.type == 'Integer':
        param_value = new_val_lookup[int(param.strValue) - 1]
    else:
        param_value = param.strValue

    new_function = controller.create_function(new_module,
                                              'ThresholdOptimizationMethod',
                                              [param_value],
                                              [alias])
    # !!! I should do this, but there is an ordering bug so I do the
    # following lines instead !!!
    #  return [('add', new_function, new_module.vtType, new_module.id)]

    new_module.add_function(new_function)
    return []

def get_filename_relative(f):
    return getFileRelativeToCurrentVT(f.name)

def getFileRelativeToCurrentVT(fname, curModule=None):
    #  This is three step approach:
    #  step 1: if fname exists assume it's the one we want and return it.
    #  step 2: Look for the file relative to the current VT.
    #        If there exists in the fname path one and only one folder with the same name as
    #        the folder that contains our current vt file, take the portion
    #        of the fname from that folder on and join it to the folder that contains
    #        our vt file.  If that fname exists on the file system assume it is the right one.
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

        #  step 2
        match_fname_parts = fname.split("/")
        try:
            app = application.get_vistrails_application()()
            curlocator = app.get_vistrail().locator.name
            vt_fname = curlocator.replace("\\", "/")

            vt_dname = os.path.split(vt_fname)[0]
            if os.path.exists(os.path.join(vt_dname, fname)):
                return os.path.join(vt_dname, fname)
            vt_just_dname = os.path.split(vt_dname)[1]

            if match_fname_parts.count(vt_just_dname) == 1:
                #  a directory with the same name as our current vt parent folder exists in this files path
                local_fname = "/".join(match_fname_parts[match_fname_parts.index(vt_just_dname) + 1:])
                fname_relative_to_vt = os.path.join(vt_dname, local_fname)
                if os.path.exists(fname_relative_to_vt):
                    #  a file path exists with the same local path in the folder our vt is in
                    #  we will assume this isn't a coincidence...
                    return fname_relative_to_vt
        except:
            pass

        #  step 3
        try:
            session_dir = getrootdir()
            session_dir = session_dir.replace("\\", "/")

            just_session_dir = os.path.split(session_dir)[1]

            if match_fname_parts.count(just_session_dir) == 1:
                #  a directory with the same name as our current vt session folder exists in this files path
                local_fname = "/".join(match_fname_parts[match_fname_parts.index(just_session_dir) + 1:])
                fname_relative_to_session = os.path.join(session_dir, local_fname)
                if os.path.exists(fname_relative_to_session):
                    #  a file path exists with the same local path in the folder our vt is in
                    #  we will assume this isn't a coincidence...
                    return fname_relative_to_session
        except:
            pass

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
                          key_inputs=[], file_or_dir='file',
                                                    subfolder="", runname=""):
    '''How we're handling file can lead to some unanticipated results.
    To handle this we want to re-use file names if
    1) all ports are identical
    2) key input files we want to monitor for changes
    '''
    h = sha_hash()
    h.update(curModule.signature)
    for key in sorted(curModule.inputPorts):
        if curModule.hasInputFromPort(key):
#              print bytes(curModule.getInputFromPort(key))
            h.update(bytes(curModule.getInputFromPort(key)))

    for input in key_inputs:
#          print str(input) + hash_file(input)
        h.update(str(input) + hash_file(input))

    signature = h.hexdigest()

    fname = get_fname_from_hash_pickle(signature)

    if fname is None and file_or_dir == 'file':
        fname = mknextfile(prefix=prefix, suffix=suffix,
                           directory=directory, subfolder=subfolder, runname=runname)
        already_run = False
    elif fname is None:
        fname = mknextdir(prefix=prefix, subfolder=subfolder, runname=runname)
        already_run = False
    elif not os.path.exists(fname) or \
        (file_or_dir == 'file' and os.path.getsize(fname) == 0) or \
        (file_or_dir != 'file' and os.listdir(fname) == []):
        already_run = False
        dir_path = os.path.dirname(fname)
        if os.path.abspath(dir_path) == os.path.abspath(getrootdir()) or \
            os.path.abspath(dir_path) == os.path.abspath(os.path.join(getrootdir(), subfolder)):
            #  We're in the same directory but maybe they deleted the original file
            if file_or_dir == 'file':
                file = open(os.path.abspath(fname), 'w+')
                file.close()
            else:
                os.makedirs(fname)
        else:
            #  The old fname was in some other directory.
            delete_hash_entry_pickle(signature=signature, directory=directory)
            if file_or_dir == 'file':
                fname = mknextfile(prefix=prefix, suffix=suffix,
                           directory=directory, subfolder=subfolder, runname=runname)
            else:
                fname = mknextdir(prefix=prefix,
                           directory=directory, subfolder=subfolder, runname=runname)
    else:
        already_run = True

    return fname, signature, already_run




def get_sheet_location(_module):
    '''given a sahm spreadsheet module, finds all the other sahm spreadsheet cells
    in the currently executing pipeline and returns a CellLocation
    with the name of the currently executing pipeline and dimensions set to
    hold the number of cells needed.
    '''
    auto_location = None
    if _module.inputPorts.has_key('Location'):
        auto_location = _module.inputPorts['Location'][0].obj
    else:
        try:
            cur_vt = _module.moduleInfo['controller'].vistrail

            cur_pipeline = _module.moduleInfo['pipeline']
            cur_version = _module.moduleInfo['controller'].current_version
            cur_name = cur_vt.get_pipeline_name(cur_version)
            if "+" in cur_name:
                cur_name = " ".join(cur_name.split()[:-2])

            cur_cells = 0
            rows = []
            cols = []
            for m in cur_pipeline.modules.itervalues():
                if m.name in ['SAHMSpatialOutputViewerCell',
                                'SAHMModelOutputViewerCell',
                                    'GeoSpatialViewerCell']:
                    cur_cells += 1
                    for function in m.functions:
                        if function.name == 'row':
                            rows.append(int(function.params[0].strValue))
                        elif function.name == 'column':
                            cols.append(int(function.params[0].strValue))
            if rows:
                max_row = max(rows)
            else:
                max_row = 2

            if cols:
                max_col = max(cols)
            else:
                max_col = math.ceil(cur_cells / 2)

            if max_row * max_col < cur_cells:
                max_col = math.ceil(cur_cells / 2)

            sheet_ref = StandardSheetReference()
            sheet_ref.sheetName = cur_name
            sheet_ref.minimumColumnCount = max_col
            sheet_ref.minimumRowCount = max_row
            auto_location = CellLocation.Location()
            auto_location.sheetReference = sheet_ref

        except AttributeError:
            auto_location = None

    if _module.hasInputFromPort("row"):
        if not auto_location:
            auto_location = CellLocation.Location()
        auto_location.row = _module.getInputFromPort('row') - 1

    if _module.hasInputFromPort("column"):
        if not auto_location:
            auto_location = CellLocation.Location()
        auto_location.col = _module.getInputFromPort('column') - 1

    return auto_location

def get_previous_run_info(full_fname):
    '''given a fname in in the format:
                ..\sessiondir\<subfolder>\prefix_runname_count.suffix"
    returns the subfolder and runname if applicable
    '''
    folder, fname = os.path.split(full_fname)
    parentfolder, subfolder = os.path.split(folder)

    if folder == getrootdir():
        #  there was no subfolder this file is at the root of our session dir
        subfolder = ""
    elif os.path.abspath(parentfolder) == os.path.abspath(getrootdir()):
        pass  #  The subfolder we have is legit.
    else:
        subfolder = ""

    fname_parts = fname.split("_")
    if len(fname_parts) == 2 or len(fname_parts) == 1:
        #  there was no runname
        runname = ""
    else:
        runname = fname_parts[1]

    return subfolder, runname




