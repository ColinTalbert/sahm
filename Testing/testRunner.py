###############################################################################
##
## Copyright (C) 2006-2011, University of Utah. 
## All rights reserved.
## Contact: contact@vistrails.org
##
## This file is part of VisTrails.
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
###############################################################################

#Please update the sys.path.append() line below with the path to your vistrails
# folder
# We need to make sure VisTrails is initialized before trying to execute workflows
import sys, os

pathToVisTrails = os.path.sep.join(__file__.split(os.path.sep)[:-4])
sys.path.append(pathToVisTrails)

import core.requirements
#core.requirements.check_pyqt4()
from PyQt4 import QtGui
import gui.application
import os

options = {'useCache':False}
try:
    v = gui.application.start_application(options)
    if v != 0:
        if gui.application.VistrailsApplication:
            gui.application.VistrailsApplication.finishSession()
        sys.exit(v)
    app = gui.application.VistrailsApplication()
except SystemExit, e:
    if gui.application.VistrailsApplication:
        gui.application.VistrailsApplication.finishSession()
    sys.exit(e)
except Exception, e:
    if gui.application.VistrailsApplication:
        gui.application.VistrailsApplication.finishSession()
    print "Uncaught exception on initialization: %s" % e
    import traceback
    traceback.print_exc()
    sys.exit(255)
#api must be imported after vistrails initialization
import api
from core.db.locator import FileLocator
from core.console_mode import run
from core import debug

#### load your workflows here
filename = "/path/to/examples/head.vt"
workflow = "aliases"
locator = FileLocator(os.path.abspath(filename))
#build a list of (locator,workflow) to pass to run
w_list = [(locator, workflow)]

#parameters should be exactly as you would send in the command line
#ex: "filename=other.png$&$param2=value"
#set update_vistrail=True if you want the log of this execution to
#be stored in the vistrail file. Otherwise set it to False. The
#default is False
parameters = "isovalue=60"
errs = run(w_list,parameters=parameters, update_vistrail=False)
if len(errs) > 0:
    for err in errs:
        debug.critical("*** Error in %s:%s:%s -- %s" % err)
#### end of your code

#uncomment the line below if you need vistrails running after 
#executing your code, else it will just close.
#v = app.exec_()
gui.application.stop_application()
sys.exit(v)