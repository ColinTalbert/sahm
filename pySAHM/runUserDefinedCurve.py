import os
import sys
import csv

import utilities

import copy

import multiprocessing

from PyQt4 import QtCore, QtGui
import csv
import utils
from utils import writetolog
import shutil
import os
import subprocess

try:
    from vistrails.core.modules.vistrails_module import Module
    from vistrails.core.system import execute_cmdline
except:
    from core.modules.vistrails_module import Module
    from core.system import execute_cmdline

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class CreateUserDefinedCurveDialog(QtGui.QDialog):

    def __init__(self, kwargs, parent=None):
        #  print input_mds, output_json, rPath, modelsPath
        self.rPath = kwargs['r_path']
        self.kwargs = kwargs
        if not self.kwargs.has_key('numPlots'):
            self.kwargs['numPlots'] = 8
        if not self.kwargs.has_key('minCor'):
            self.kwargs['minCor'] = "0.7"

        self.selection_name = os.path.split(kwargs['output_json'])[1]
        self.selection_name = os.path.splitext(self.selection_name)[0]
        self.selection_name = self.selection_name.split('_')[-1]
        self.module = self.kwargs['module']
        self.displayJPEG = kwargs['displayJPEG']

        QtGui.QDialog.__init__(self, parent)

        self.input_mds = kwargs['input_mds']
        self.output_json = kwargs['output_json']
        self.outputDir = os.path.split(kwargs['output_json'])[0]

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
        self.treeview.setColumnCount(2)
        self.treeview.setSortingEnabled(True)
        self.treeview.headerItem().setText(0, "Include")
        self.treeview.setColumnWidth(0, 200)
        self.treeview.headerItem().setText(1, "% Deviance Explained")
        self.treeview.setColumnWidth(1, 125)
        self.treeview.setToolTip(_fromUtf8("Double click to view detailed information for single covariate."))
#        self.treeview.setHeaderLabels(['include', 'covariate'])
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


        self.view = utils.InteractiveQGraphicsView(self)

        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(4)
        sizePolicy.setVerticalStretch(0)
        #  sizePolicy.setHeightForWidth(self.view.sizePolicy().hasHeightForWidth())
        self.view.setSizePolicy(sizePolicy)
        self.view.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.view.setObjectName(_fromUtf8("view"))

        self.splitter.addWidget(self.view)

        self.verticalLayout_3.addWidget(self.splitter)
        self.button_layout = QtGui.QHBoxLayout()
        self.button_layout.setContentsMargins(-1, 3, -1, 3)
        self.button_layout.setObjectName(_fromUtf8("button_layout"))
        self.btnOK = QtGui.QPushButton()
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnOK.sizePolicy().hasHeightForWidth())
        self.btnOK.setSizePolicy(sizePolicy)
        self.btnOK.setBaseSize(QtCore.QSize(100, 0))
        self.btnOK.setToolTip(_fromUtf8(""))
        self.btnOK.setObjectName(_fromUtf8("btnOK"))
        self.button_layout.addWidget(self.btnOK)
        self.btnCancel = QtGui.QPushButton()
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnCancel.sizePolicy().hasHeightForWidth())
        self.btnCancel.setSizePolicy(sizePolicy)
        self.btnCancel.setBaseSize(QtCore.QSize(100, 0))
        self.btnCancel.setObjectName(_fromUtf8("btnCancel"))
        self.button_layout.addWidget(self.btnCancel)
        self.verticalLayout_3.addLayout(self.button_layout)
        self.horizontalLayout_4.addLayout(self.verticalLayout_3)

        self.setWindowTitle(_fromUtf8("Covariate Correlation viewer"))
        self.label_2.setText(_fromUtf8("Covariates"))
        self.btnRunR.setText(_fromUtf8("Update"))
        self.label.setText(_fromUtf8("Threshold"))
        self.numPlotsLabel.setText(_fromUtf8("Number of Plots"))
        self.btnOK.setText(_fromUtf8("OK"))
        self.btnCancel.setText(_fromUtf8("Cancel"))
        self.resize(1100, 800)
        self.view.resize(800, 750)

        #  #End  autogenerated code from QtDesigner

        layout.addLayout(self.horizontalLayout_4)

        self.btnCancel.setShortcut('Esc')
        self.connect(self.btnOK, QtCore.SIGNAL('clicked(bool)'),
                     self.okTriggered)
        self.connect(self.btnCancel, QtCore.SIGNAL('clicked(bool)'),
                     self.cancel)
        self.connect(self.btnRunR, QtCore.SIGNAL('clicked(bool)'),
                     self.update_pairs_plot)
        self.connect(self.lineEdit, QtCore.SIGNAL('textChanged(QString)'),
                     self.thresholdEdit)
        self.connect(self.numPlots, QtCore.SIGNAL('textChanged(QString)'),
             self.numPlotsEdit)

        self.connect(self.treeview, QtCore.SIGNAL('itemDoubleClicked(QTreeWidgetItem*, int)'), self.on_item_doublclick)



        #  code to populate the treeview with the contents of our MDS
        self.PopulateTreeview()

        self.setLayout(layout)
        self.repaint()

        #  code to add in pictureviewer stuff
        outputPic = self.make_new_pairs_plot(self.input_mds)
        self.view.load_picture(outputPic)


    def on_item_doublclick(self, item, column):
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
        output_dir = os.path.join(self.outputDir, "PredictorInspections")
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

        outputPic = self.makeNewCovariatePlot(output_dir, str(item.text(0)))
        QtGui.QApplication.restoreOverrideCursor()
        self.popup = QtGui.QDialog()
#        self.popup.setBaseSize(1200, 1200)
        size = 800
        self.popup.resize(size, size)

        viewWindow = utils.InteractiveQGraphicsView(self.popup)
        viewWindow.resize(size, size)
        layout = QtGui.QVBoxLayout()
        layout.addWidget(viewWindow)
        self.popup.setLayout(layout)
        viewWindow.load_picture(outputPic)
        viewWindow.view_current()

        retVal = self.popup.exec_()


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
        writetolog("    PopulateTreeview input_mds = " + self.input_mds, False, False)

#        self.treeview.setColumnCount(2)
        #  If an output_json already exists then the user has run this module before.
        #  We need to pull and apply their previous selections from that output file
        csvfile = open(self.input_mds, "r")
        #  print "MDS", self.input_mds
        reader = csv.reader(csvfile)
        header = reader.next()  #  store the header
        header2 = reader.next()  #  the 2nd line of the mds with use/don't use
        header3 = reader.next()  #  the 3rd line of the mds with the path

        self.responseCol = header[2]

        headerList = []
        n = 0
        for i in range(0, len(header)):
            headerList.append([header[i], header2[i], header3[i]])

        noncovariate_columns = ['Split', 'EvalSplit']
        for item in headerList[3:]:
            if not item[0] in noncovariate_columns:
                child_item = QtGui.QTreeWidgetItem([_fromUtf8(item[0]), "0"])
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
                #  self.tree_items[file] = child_item
                n += 1
        csvfile.close()
        #  update the tree view label to show how many covariates there are
        self.label_2.setText(_fromUtf8("Covariates   (n=" + str(n) + ")"))

    def SaveMDSFromTreeview(self):
        #  updates the second header line on the input MDS file
        #  to reflect the checked items in the tree view
        #  and saves the results to the output MDS.

        reader = csv.reader(open(self.input_mds, "r"))
        header = reader.next()  #  store the header
        header2 = reader.next()  #  the 2nd line of the mds with use/don't use
        header3 = reader.next()  #  the 3rd line of the mds with the path

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

        oFile = open(self.output_json, 'wb')
        writer = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(header)
        writer.writerow(outHeader2)
        writer.writerow(header3)
        for row in reader:
            writer.writerow(row)
        oFile.close

    def update_pairs_plot(self):
        self.SaveMDSFromTreeview()
        outputPic = self.make_new_pairs_plot(self.output_json)

        self.view.load_picture(outputPic)

    def make_new_pairs_plot(self, MDSfile):

        args = {'i':  MDSfile,
                'o': self.displayJPEG,
                'rc': self.responseCol}

        checkboxes = [('pres', self.chkPresence),
                      ('absn', self.chkAbsence),
                      ('bgd', self.chkBackground)]
        for arg, checkbox in checkboxes:
            args[arg] = str(checkbox.checkState() == QtCore.Qt.Checked).upper()

        kwarg_args = [('p', 'numPlots'),
                      ('m', 'minCor'),
                      ('core', 'corsWithHighest')]
        for arg, kwarg_key in kwarg_args:
            if kwarg_key in self.kwargs:
                args[arg] = str(self.kwargs[kwarg_key])

        if os.path.exists(os.path.join(self.outputDir, "Predictor_Correlation.jpg")):
            os.remove(os.path.join(self.outputDir, "Predictor_Correlation.jpg"))

        utils.run_R_script('PairsExplore.r', args, self.module, new_r_path=self.kwargs['r_path'])

        if os.path.exists(os.path.join(self.outputDir, "devinfo.csv")):
            self.loadDeviances()

        if os.path.exists(os.path.join(self.displayJPEG)):
            return os.path.join(self.displayJPEG)
        else:
            writetolog("Missing output from R processing: " + self.displayJPEG)
            raise Exception, "Missing output from R processing"

    def saveExploreOptions(self, args):
        checkboxes = [('pres', self.chkPresence),
                      ('absn', self.chkAbsence),
                      ('bgd', self.chkBackground)]
        for arg, checkbox in checkboxes:
            args[arg] = str(checkbox.checkState() == QtCore.Qt.Checked).upper()

    def loadDeviances(self):
        #  store the deviances explained in dev
        deviances = {}
        devfname = os.path.join(self.outputDir, "devinfo.csv")
        devcsv = open(devfname)
        devreader = csv.reader(devcsv)
        header = devreader.next()
        for line in devreader:
            try:
                deviance = "%.1f" % float(line[1])
                deviance = deviance.rjust(7)
                try:
                    item = self.treeview.findItems(_fromUtf8(line[0]), QtCore.Qt.MatchFlags())
                    item[0].setData(1, 0, deviance)
                except:
                    print "Problem encountered with item: ", line[0]
            except:
                print "problem loading deviances"
        del devcsv


    def makeNewCovariatePlot(self, output_dir, covariate):
        output_fname = os.path.join(output_dir, covariate + ".jpg")

        args = {"i":self.input_mds,
            "o":output_dir,
            "rc":self.responseCol,
            "p":covariate}
        self.saveExploreOptions(args)

        if os.path.exists(output_fname):
            os.remove(output_fname)

        utils.run_R_script('Predictor.inspection.r', args)

        if os.path.exists(output_fname):
            return output_fname
        else:
            writetolog("Missing output from R processing: " + self.displayJPEG)
            raise Exception, "Missing output from R processing"

    def closeEvent(self, event):
        self.cancel()






def main(argv):
    """Process our command line args and initiate a UserDefinedCurve run
    """
    pass
#      ourMaxent = UserDefinedCurveRunner()
#
#      args = sys.argv[sys.argv.index("--args") + 1:]
#      for arg in args:
#          k, v = arg.split("=")
#          if k == "maxent_args":
#              ourMaxent.maxent_args = eval(v)
#          elif v.lower() in ['true', 'false']:
#              ourMaxent.args_dict[k] = eval(v.title())
#          else:
#              ourMaxent.args_dict[k] = v
#
#      ourMaxent.run()

if __name__ == "__main__":

#    try:
    main(sys.argv)
