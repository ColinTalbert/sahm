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

def process_waiter(popen, description, que):
    try: 
        popen.wait()     
    finally: 
        que.put( (description, popen.returncode) ) 

def find_key(dic, val):
    """return the key of dictionary dic given the value
    from: http://www.daniweb.com/software-development/python/code/217019"""    
    return [k for k, v in dic.iteritems() if v == val][0]


    

