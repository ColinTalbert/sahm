#!/usr/bin/python
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

import os, sys
import time
import csv
import string

import tempfile

_logfile = ''
_verbose = False

import subprocess
import multiprocessing

from PyQt4 import QtCore, QtGui

mosaicAllTifsInFolder = None

class logger(object):
    def __init__(self, logfile, verbose):
        self.logfile = logfile
        self.verbose = verbose
        
        #if we mistakenly get a output dir instead of a filename
        if os.path.isdir(logfile):
            self.logfile = os.path.join(logfile, 'sessionLog.txt')
        else:
            self.logfile = logfile
            
        if os.path.exists(self.logfile):
            self.writetolog("\nSession continued\n", True, True)
        else:
            logDir = os.path.split(self.logfile)[0]
            if not os.path.exists(logDir):
                raise RuntimeError('Directory of specified logfile does not exist.')
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

#parallelization, remote processing, etc utilites
def process_waiter(popen, description, que):
    '''This needs to be replaced with something that allow
    '''
    try: 
        popen.wait()     
    finally: 
        que.put( (description, popen.returncode) ) 

def runCondorPythonJob(args, workspace, prefix, wholeMachine=False):
    #replace all mappedDriveLetters in the argsDict with UNC paths
    global UNCDrives
    for item in args:
        args[args.index(item)] = replaceMappedDrives(item)
         
    if prefix[0].isdigit():
        prefix = "_" + prefix
               
    #create submit file
        #create condorSubmit file
    submitFname = os.path.join(workspace, prefix + "_CondorSubmit.txt")
    submitFile = open(submitFname, 'w')
    submitFile.write("Universe                = vanilla\n")
    submitFile.write("Executable              = c:\Windows\System32\cmd.exe\n")
    submitFile.write("run_as_owner            = true\n")
    submitFile.write("Getenv                  = true\n")
    submitFile.write("Should_transfer_files   = no\n")
    submitFile.write("transfer_executable     = false\n")
    
    machines = ['igskbacbwsvis1', 'igskbacbwsvis2', 'igskbacbwsvis3', 'igskbacbwsvis4', 'igskbacbws3151a', 'igskbacbws425', 'igskbacbws108']
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
    submitFile.write("Output                  = " + replaceMappedDrives(stdOutFname) +"\n")
    submitFile.write("error                   = " + replaceMappedDrives(stdErrFname) +"\n")
    submitFile.write("log                     = " + replaceMappedDrives(logFname) +"\n")
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
    
    #launch condor job
    DEVNULL = open(os.devnull, 'wb')
    p = subprocess.Popen(["condor_submit", "-n", 'igskbacbws108', submitFname], stderr=DEVNULL, stdout=DEVNULL)    
    
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
    ret = subprocess.Popen(["net","use"], stdout=subprocess.PIPE, 
                           universal_newlines=True).communicate()[0].split("\n")
    for line in ret:
        line = line.split()
        if line and line[0] in ["OK", "Disconnected"]:
            if len(line) > 2 and \
                os.path.exists(line[2]):
                UNCDrives[line[1].lower() + '\\'] = line[2] + "\\"

def checkIfFolderIsOnNetwork(dirname):
    global UNCDrives
    if not os.path.splitdrive(dirname)[0].lower() + "\\" in UNCDrives.keys():
        QtGui.QMessageBox.critical(None, "Session folder error", "For Fort Condor execution to work your session folder must be on a network drive." \
            + "\nCurrently this is set to:   " + dirname)
        return False
    else:
        return True

def waitForProcessesToFinish(processQueue, maxCount=1):
    '''Given a list of running processes and a maximum number of running processes
    this function waits for enough of the processes have finished to have
    the number of running jobs be less that the maximum number of jobs we want.
    '''
    while len(processQueue) > maxCount:
            time.sleep(1)
            for process in processQueue:
                if process.poll() is not None:
                    processQueue.remove(process)
    
def getProcessCount(strProcessingMode):
    '''The number of concurrently running jobs is dependent on the currently 
    selected processingMode. 
    If on Condor then send them all and let Condor manage the Queue.
    else we will be running n-1 jobs (this function is only used by PARC now)
    '''
    if strProcessingMode == "FORT Condor":
        return  2**32
    else:
        return multiprocessing.cpu_count() - 1
    
#Spatial utilities
def mosaicAllTifsInFolder(inDir, outFileName, gdal_merge):
    onlyfiles = [os.path.join(inDir,f) for f in os.listdir(inDir) 
            if os.path.isfile(os.path.join(inDir,f)) and f.endswith(".tif") ]
    args = ["placeholder", "-o", outFileName] + onlyfiles
    gdal.DontUseExceptions()
    gdal_merge.main(args)
