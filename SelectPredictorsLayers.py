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

from core.modules.vistrails_module import Module
from PyQt4 import QtCore, QtGui
import csv
import utils
from utils import writetolog
import shutil
import os
from core.system import execute_cmdline
import subprocess

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class SelectListDialog(QtGui.QDialog):

    def __init__(self, kwargs, parent=None):
        #print inputMDS, outputMDS, rPath, modelsPath
        self.rPath = kwargs['r_path']
        self.kwargs = kwargs
        if not self.kwargs.has_key('numPlots'):
            self.kwargs['numPlots'] = 8
        if not self.kwargs.has_key('minCor'):
            self.kwargs['minCor'] = "0.7"
        
        self.selection_name = os.path.split(kwargs['outputMDS'])[1]
        self.selection_name = os.path.splitext(self.selection_name)[0]
        self.selection_name = self.selection_name.split('_')[-1]
        
        self.displayJPEG = kwargs['displayJPEG']
        
        QtGui.QDialog.__init__(self, parent)
        
        self.inputMDS = kwargs['inputMDS']
        self.outputMDS = kwargs['outputMDS']
        self.outputDir = os.path.split(kwargs['outputMDS'])[0]
        
        layout = QtGui.QVBoxLayout()
        self.setWindowFlags(QtCore.Qt.Window)
        
        self.horizontalLayout_4 = QtGui.QHBoxLayout()
        self.horizontalLayout_4.setSpacing(5)
        self.horizontalLayout_4.setMargin(5)
        self.horizontalLayout_4.setObjectName(_fromUtf8("horizontalLayout_4"))
        self.verticalLayout_3 = QtGui.QVBoxLayout()
        self.verticalLayout_3.setObjectName(_fromUtf8("verticalLayout_3"))
        self.splitter = QtGui.QSplitter()
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.splitter.sizePolicy().hasHeightForWidth())
        self.splitter.setSizePolicy(sizePolicy)
        self.splitter.setMinimumSize(QtCore.QSize(0, 0))
        self.splitter.setFrameShape(QtGui.QFrame.Box)
        self.splitter.setFrameShadow(QtGui.QFrame.Plain)
        self.splitter.setLineWidth(1)
        self.splitter.setOrientation(QtCore.Qt.Horizontal)
        self.splitter.setHandleWidth(10)
        self.splitter.setObjectName(_fromUtf8("splitter"))
        self.widget = QtGui.QWidget(self.splitter)
        self.widget.setObjectName(_fromUtf8("widget"))
        self.verticalLayout = QtGui.QVBoxLayout(self.widget)
        self.verticalLayout.setSpacing(4)
        self.verticalLayout.setSizeConstraint(QtGui.QLayout.SetDefaultConstraint)
        self.verticalLayout.setContentsMargins(6, 6, 3, 3)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.label_2 = QtGui.QLabel(self.widget)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.verticalLayout.addWidget(self.label_2)
        self.treeview = QtGui.QTreeWidget(self.widget)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Ignored, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.treeview.sizePolicy().hasHeightForWidth())
        self.treeview.setSizePolicy(sizePolicy)
        self.treeview.setMinimumSize(QtCore.QSize(75, 0))
        self.treeview.setMaximumSize(QtCore.QSize(16777215, 16777215))
        self.treeview.setSizeIncrement(QtCore.QSize(100, 0))
        self.treeview.setBaseSize(QtCore.QSize(0, 0))
        self.treeview.setObjectName(_fromUtf8("treeview"))
        self.verticalLayout.addWidget(self.treeview)
        self.horizontalLayout_3 = QtGui.QHBoxLayout()
        self.horizontalLayout_3.setSizeConstraint(QtGui.QLayout.SetDefaultConstraint)
        self.horizontalLayout_3.setObjectName(_fromUtf8("horizontalLayout_3"))
        self.btnRunR = QtGui.QPushButton(self.widget)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnRunR.sizePolicy().hasHeightForWidth())
        self.btnRunR.setSizePolicy(sizePolicy)
        self.btnRunR.setObjectName(_fromUtf8("btnRunR"))
#        self.horizontalLayout_3.addWidget(self.btnRunR)

        self.label = QtGui.QLabel(self.widget)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.label.sizePolicy().hasHeightForWidth())
        self.label.setSizePolicy(sizePolicy)
        self.label.setObjectName(_fromUtf8("label"))
        self.horizontalLayout_3.addWidget(self.label)
        self.lineEdit = QtGui.QLineEdit(self.widget)        
            
        self.lineEdit.setText(_fromUtf8(str(self.kwargs['minCor'])))
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.lineEdit.sizePolicy().hasHeightForWidth())
        self.lineEdit.setSizePolicy(sizePolicy)
        self.lineEdit.setMaximumSize(QtCore.QSize(75, 16777215))
        self.lineEdit.setObjectName(_fromUtf8("lineEdit"))
        self.horizontalLayout_3.addWidget(self.lineEdit)
        spacerItem = QtGui.QSpacerItem(10, 0, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_3.addItem(spacerItem)
        self.numPlots = QtGui.QLineEdit(self.widget)
        self.numPlots.setText(_fromUtf8(str(self.kwargs['numPlots'])))
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.lineEdit.sizePolicy().hasHeightForWidth())
        
        self.horizontalLayout_6 = QtGui.QHBoxLayout()
        self.horizontalLayout_6.setSpacing(5)
        self.horizontalLayout_6.setMargin(5)
        self.numPlots.setSizePolicy(sizePolicy)
        self.numPlots.setMaximumSize(QtCore.QSize(75, 16777215))
        self.numPlots.setObjectName(_fromUtf8("numPlots"))
        self.numPlotsLabel = QtGui.QLabel(self.widget)
        self.numPlotsLabel.setSizePolicy(sizePolicy)
        self.numPlotsLabel.setObjectName(_fromUtf8("numPlotsLabel"))
        self.horizontalLayout_6.addWidget(self.numPlotsLabel)
        self.horizontalLayout_6.addWidget(self.numPlots)
        
        
        self.chkPresence = QtGui.QCheckBox(self.widget)
        self.chkPresence.setChecked(True)
        self.chkPresence.setText(_fromUtf8("Include presence/count points"))
        self.chkPresence.setObjectName(_fromUtf8("chkPresence"))
        self.verticalLayout.addWidget(self.chkPresence)
        self.chkAbsence = QtGui.QCheckBox(self.widget)
        self.chkAbsence.setChecked(True)
        self.chkAbsence.setText(_fromUtf8("Include absence points"))
        self.chkAbsence.setObjectName(_fromUtf8("chkAbsence"))
        self.verticalLayout.addWidget(self.chkAbsence)
        self.chkBackground = QtGui.QCheckBox(self.widget)
        self.chkBackground.setChecked(True)
        self.chkBackground.setText(_fromUtf8("Include background points"))
        self.chkBackground.setObjectName(_fromUtf8("chkBackground"))
        self.verticalLayout.addWidget(self.chkBackground)
        
        self.verticalLayout.addLayout(self.horizontalLayout_3)
        self.verticalLayout.addLayout(self.horizontalLayout_6)
        self.horizontalLayout_5 = QtGui.QHBoxLayout()
        self.horizontalLayout_5.addWidget(self.btnRunR)
        self.verticalLayout.addLayout(self.horizontalLayout_5)
        
        self.scene = QtGui.QGraphicsScene() 
        self.view = QtGui.QGraphicsView(self.scene)
        #self.view = customGraphicsView(self.scene)
        self.view.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
        
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(4)
        sizePolicy.setVerticalStretch(0)
        #sizePolicy.setHeightForWidth(self.view.sizePolicy().hasHeightForWidth())
        self.view.setSizePolicy(sizePolicy)
        self.view.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.view.setObjectName(_fromUtf8("view"))
        
        self.splitter.addWidget(self.view)
        
        self.verticalLayout_3.addWidget(self.splitter)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setContentsMargins(-1, 3, -1, 3)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.btnOK = QtGui.QPushButton()
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnOK.sizePolicy().hasHeightForWidth())
        self.btnOK.setSizePolicy(sizePolicy)
        self.btnOK.setBaseSize(QtCore.QSize(100, 0))
        self.btnOK.setToolTip(_fromUtf8(""))
        self.btnOK.setObjectName(_fromUtf8("btnOK"))
        self.horizontalLayout.addWidget(self.btnOK)
        self.btnCancel = QtGui.QPushButton()
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnCancel.sizePolicy().hasHeightForWidth())
        self.btnCancel.setSizePolicy(sizePolicy)
        self.btnCancel.setBaseSize(QtCore.QSize(100, 0))
        self.btnCancel.setObjectName(_fromUtf8("btnCancel"))
        self.horizontalLayout.addWidget(self.btnCancel)
        self.verticalLayout_3.addLayout(self.horizontalLayout)
        self.horizontalLayout_4.addLayout(self.verticalLayout_3)

        self.setWindowTitle(_fromUtf8("Covariate Coorelation viewer"))
        self.label_2.setText(_fromUtf8("Covariates"))
        self.btnRunR.setText(_fromUtf8("Update"))
        self.label.setText(_fromUtf8("Threshold"))
        self.numPlotsLabel.setText(_fromUtf8("Number of Plots"))
        self.btnOK.setText(_fromUtf8("OK"))
        self.btnCancel.setText(_fromUtf8("Cancel"))
        self.resize(1100, 800)
        self.view.resize(800, 750)
        
        ##End  autogenerated code from QtDesigner
        
        layout.addLayout(self.horizontalLayout_4)
        
        self.btnCancel.setShortcut('Esc')
        self.connect(self.btnOK, QtCore.SIGNAL('clicked(bool)'),
                     self.okTriggered)
        self.connect(self.btnCancel, QtCore.SIGNAL('clicked(bool)'),
                     self.cancel)
        self.connect(self.btnRunR, QtCore.SIGNAL('clicked(bool)'),
                     self.updateROutput)
        self.connect(self.lineEdit, QtCore.SIGNAL('textChanged(QString)'),
                     self.thresholdEdit)
        self.connect(self.numPlots, QtCore.SIGNAL('textChanged(QString)'),
             self.numPlotsEdit)

        
        self.scene.wheelEvent = self.wheel_event
        #code to populate the treeview with the contents of our MDS
        self.PopulateTreeview()
        
        self.setLayout(layout)
        self.repaint()

        #code to add in pictureviewer stuff
        outputPic = self.runR(self.inputMDS)
        self.load_picture(outputPic)
        
    def okTriggered(self):
        self.SaveMDSFromTreeview()
        self.done(0)

    def cancel(self):
        self.done(1)
        
    def thresholdEdit(self):
        try:
            self.kwargs['minCor'] = float(str(self.lineEdit.text()))
        except ValueError:
            pass
     
    def numPlotsEdit(self):
        try:
            self.kwargs['numPlots'] = int(str(self.numPlots.text()))
        except ValueError:
            pass   
        
    
    def PopulateTreeview(self):
        ''' Reads in the input MDS and populates the treeview widget
        with the items in covariate columns.  
        Sets the check state to be the same as the 0/1 include flag.
        '''
        writetolog("    PopulateTreeview inputMDS = " + self.inputMDS, False, False)
        self.treeview.setColumnCount(1)
        #If an outputMDS already exists then the user has run this module before.
        #We need to pull and apply their previous selections from that output file


            
        csvfile = open(self.inputMDS, "r")
        #print "MDS", self.inputMDS
        reader = csv.reader(csvfile)
        header = reader.next() #store the header
        header2 = reader.next() #the 2nd line of the mds with use/don't use
        header3 = reader.next() #the 3rd line of the mds with the path
        
        self.responseCol = header[2]
        
        headerList = []
        n = 0
        for i in range(0, len(header)):
            headerList.append([header[i], header2[i], header3[i]])
        
        noncovariate_columns = ['Split', 'EvalSplit']
        for item in headerList[3:]:
            if not item[0] in noncovariate_columns:
                child_item = QtGui.QTreeWidgetItem([item[0],])
                child_item.setFlags(QtCore.Qt.ItemIsUserCheckable |
                                QtCore.Qt.ItemIsEnabled)
                checked = True
                if int(item[1]) == 0:
                    checked = False 
                
                if not checked:
                    child_item.setCheckState(0, QtCore.Qt.Unchecked)
                else:
                    child_item.setCheckState(0, QtCore.Qt.Checked)
                
                self.treeview.addTopLevelItem(child_item)
                #self.tree_items[file] = child_item
                n += 1
        csvfile.close()
        #update the tree view label to show how many covariates there are
        self.label_2.setText(_fromUtf8("Covariates   (n=" + str(n) + ")"))
        
    def SaveMDSFromTreeview(self):
        #updates the second header line on the input MDS file 
        #to reflect the checked items in the tree view 
        #and saves the results to the output MDS.
        
        reader = csv.reader(open(self.inputMDS, "r"))
        header = reader.next() #store the header
        header2 = reader.next() #the 2nd line of the mds with use/don't use
        header3 = reader.next() #the 3rd line of the mds with the path
        
        outHeader2 = header2
        outHeader2[2] = self.selection_name    
                  
        treeviewIter = QtGui.QTreeWidgetItemIterator(self.treeview)
        while treeviewIter.value():
            item = treeviewIter.value()
            col_index = header.index(item.text(0))
            if item.checkState(0) == QtCore.Qt.Checked:
                outHeader2[col_index] = "1"
            else:
                outHeader2[col_index] = "0"
            treeviewIter += 1

        oFile = open(self.outputMDS, 'wb')
        writer = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(header)
        writer.writerow(outHeader2)
        writer.writerow(header3)
        for row in reader:
            writer.writerow(row)
        oFile.close

    def updateROutput(self):
        self.SaveMDSFromTreeview()
        outputPic = self.runR(self.outputMDS)
        self.load_picture(outputPic)
        
    def runR(self, MDSfile):
#        
#        program = os.path.join(self.rPath, "i386", "Rterm.exe") 
#        script = os.path.join(utils.getModelsPath(), "PairsExplore.r")

        args = "i=" + '"' + MDSfile + '"' + " o=" + '"' + self.displayJPEG + '"' 
        args += " rc=" + self.responseCol
        
        if self.chkPresence.checkState() == QtCore.Qt.Checked:
            args += " pres=TRUE"
        else:
            args += " pres=FALSE"
        if self.chkAbsence.checkState() == QtCore.Qt.Checked:
            args += " absn=TRUE"
        else:
            args += " absn=FALSE"
        if self.chkBackground.checkState() == QtCore.Qt.Checked:
            args += " bgd=TRUE"
        else:
            args += " bgd=FALSE"
            
        if self.kwargs.has_key('numPlots'):
            args += " p=" + str(self.kwargs['numPlots'])
        
        if self.kwargs.has_key('minCor'):
            args += " m=" + str(self.kwargs['minCor'])
            
        if self.kwargs.has_key('corsWithHighest'):
            args += " core=" + str(self.kwargs['corsWithHighest'])

#        command = program + " --vanilla -f " + script + " --args " + args
#        writetolog("    " + command, False, False)
        if os.path.exists(os.path.join(self.outputDir, "Predictor_Correlation.jpg")):
            os.remove(os.path.join(self.outputDir, "Predictor_Correlation.jpg"))
            
        utils.runRScript('PairsExplore.r', args)

#        p = subprocess.Popen(command, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
#
#        writetolog("   Calculating covariate correlation in r R ")

#        ret = p.communicate()
#        if ret[1]:
#            msg = "An error was encountered in the R script for this module.  The R error message is below - \n"
#            msg += ret[1]
#            writetolog(msg)
#            raise Exception, msg
#        else:
#            writetolog("   Finished in R. ")
#        del(ret)
        
        if os.path.exists(os.path.join(self.displayJPEG)):
            return os.path.join(self.displayJPEG)
        else:
            writetolog("Missing output from R processing: " + self.displayJPEG)
            raise Exception, "Missing output from R processing"

    def load_picture(self, strPicture):
        self.l_pix = QtGui.QPixmap(strPicture)
        if self.view.size().width()  <= self.view.size().height(): 
            self.max_vsize = self.view.size().width() * 0.95
        else: 
            self.max_vsize = self.view.size().height() * 0.95
            
        self.c_view = self.l_pix.scaled(self.max_vsize, self.max_vsize, 
                                            QtCore.Qt.KeepAspectRatio, 
                                            QtCore.Qt.SmoothTransformation) 
        self.view_current()
   
    def view_current(self):
        size_img = self.c_view.size() 
        wth, hgt = QtCore.QSize.width(size_img), QtCore.QSize.height(size_img) 
        self.scene.clear() 
        self.scene.setSceneRect(0, 0, wth, hgt) 
        self.scene.addPixmap(self.c_view) 
        QtCore.QCoreApplication.processEvents() 
        

    def wheel_event (self, event):
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

    def closeEvent(self, event):
        self.cancel()
        
    def resizeEvent(self, event):
        self.load_picture(self.displayJPEG)

