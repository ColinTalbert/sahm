#!/usr/bin/python

''' These utility functions are shared 
between all of the Python SAHM command line
modules
'''

import os
import time
import csv

from osgeo import gdal
from osgeo import osr

_logfile = ''
_verbose = False

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
            f = open(self.logfile, "w")
            del f
            self.writetolog("\nSession started\n", True, True)
            
            
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

            

#def createsessionlog(roottempdir, verbose):
#    '''Creates a new log file if one doesn't already exist.
#    If one exists then writes that the session has continued.
#    '''
#    global _logfile, _verbose
#    _verbose = verbose
#    
#    logfile = os.path.join(roottempdir, "sessionLog.txt")
#    _logfile = logfile
#    
#    if os.path.exists(logfile):
#        writetolog("\nSession continued\n", True, True, logfile=logfile)
#    else:
#        f = open(os.path.join(roottempdir, "sessionLog.txt"), "w")
#        del f
#    
#    return logfile
#
#def writetolog(msg, addtime=False, printtoscreen=True, logfile=''):
#    '''Opens the log file and writes the passed msg.
#    Optionally adds a time slot to the message and
#    Prints the msg to the screen.
#    If the logfile is not specified tries to use the global variable.
#    '''
#    global _logfile
#    if logfile == '':
#        logfile = _logfile
#    
#    f = open(logfile, "a")
#    if _verbose and printtoscreen:
#        print msg
#    if addtime:
#        msg = ' at: '.join([msg, time.strftime("%m/%d/%Y %H:%M")])
#    msg = "\n" + msg
#    f.write(msg)
#    del f

    
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

    for i in [0, 1, 2]:
        if not(header2[0] == '' or header2[0] == '0'):
            return False
    
    for item in header2[3:-1]:
        if not item in ['0', '1']:
            return False
    
    return True
    del MDSreader

def process_waiter(popen, description, que):
    try: 
        popen.wait()     
    finally: 
        que.put( (description, popen.returncode) ) 

#class SAHMLogger(object):
#    def __init__(self, sessiondir):
#        self.sessiondir = sessiondir
#    
#        logfile = os.path.join(roottempdir, "sessionLog.txt")
#        if os.path.exists(logfile):
#            self.logfile = open(os.path.join(roottempdir, "sessionLog.txt"), "a")
#        else:
#            self.logfile = open(os.path.join(roottempdir, "sessionLog.txt"), "w")
#            
#        writetolog("\nSession started", True)
#        writetolog("\n")
#
#    def writetolog(msg, addtime=False, printtoscreen=True):
#        if _verbose and show:
#            print msg
#        if addtime:
#            msg = ' at: '.join([msg, time.strftime("%m/%d/%Y %H:%M")])
#            self.logfile.write(msg)
#        else:
#            self.logfile.write(msg)
#        self.logfile.write("\n")
#        self.logfile.flush()


