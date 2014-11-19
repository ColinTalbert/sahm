import os
import sys
import csv

import utilities

import copy

import multiprocessing


class HSCRunner(object):
    '''This is a class to run the maxent jar.  The output from will need
    to be sent through WrapMaxent.r to generate outputs.
    '''
    def __init__(self):
        self.verbose = False
        self.maxent_path = ''
        self.java_path = 'java'
        self.mdsfile = ''
        self.projectionlayers = ''
        self.environmentallayers = ''
        self.testCSV = ''
        self.trainingCSV = ''
        self.backgroundCSV = ''
        self.outputdir = ''
        self.categoricals = []
        self.logger = None
        self.test_key = 'test'
        self.sub_run = False

        self.processing_mode = "single models sequentially (n - 1 cores each)"
        self.args_dict = {}
        self.maxent_args = {}

    def run(self):
        if self.sub_run:
            self.maxent_args['plots'] = 'false'
            self.maxent_args['responsecurves'] = 'false'
            self.maxent_args['outputgrids'] = 'false'

        self.validateInputs()

        self.start_pool()

        if self.args_dict['c'] != '':
            self.prepInputs()
        else:
            raise Exception, 'No MDS supplied.'
        if not self.maxent_args.has_key('projectionlayers'):
            self.maxent_args['projectionlayers'] = ''
        if self.trainingCSV != '':
            self.maxent_args['samplesfile'] = self.trainingCSV
        else:
            raise Exception, 'No Samples file supplied'
        if self.testCSV != '':
            self.maxent_args['testsamplesfile'] = self.testCSV
        if self.maxent_args['environmentallayers'] != '':
            self.maxent_args['environmentallayers'] = self.maxent_args['environmentallayers']
            if self.maxent_args.has_key('biasfile'):
                self.maxent_args['biasfile'] = self.maxent_args['biasfile']
        elif self.backgroundCSV != '':
            self.maxent_args['environmentallayers'] = self.backgroundCSV
            if self.maxent_args.has_key('biasfile'):
                self.writetolog('The supplied biasfile will be ignored since no environmentallayers were provided', True, True)
                self.maxent_args.pop('biasfile')
        self.maxent_args['autorun'] = 'true'
        if ' ' in self.args_dict['species_name']:
            self.args_dict['species_name'] = self.args_dict['species_name'].replace(' ', '_')
        if(not self.test_key):  #  if there is no testsplit (ie cross validation, remove the empty csv
            del self.maxent_args['testsamplesfile']

        if self.categoricals:
            catstr = ",".join([cat.replace('_categorical', '')
                               for cat in self.categoricals])
            self.maxent_args['togglelayertype'] = catstr

        if not self.maxent_path.endswith('.jar'):
            self.maxent_path = os.path.join(self.maxent_path, 'maxent.jar')

        cmd = self.gen_maxent_cmd()
        stdout_fname = os.path.join(self.outputdir, "stdOut.txt")
        stderr_fname = os.path.join(self.outputdir, "stdErr.txt")
        self.writetolog('    running command:  \n' +
                        utilities.convert_list_to_cmd_str(cmd) + "\n", True, False)
        utilities.add_process_to_pool(utilities.launch_cmd,
                                [cmd, stdout_fname, stderr_fname])
        utilities.wait_for_pool_to_finish()

        r_cmd = [sys.executable]
        r_cmd.extend(sys.argv)
        r_cmd[1] = r_cmd[1].replace('runMaxent.py', 'runRModel.py')
        r_cmd.append('lam=' + self.outputdir)
        self.writetolog('    running command:  \n' +
                    utilities.convert_list_to_cmd_str(r_cmd) + "\n", True, False)
        utilities.add_process_to_pool(utilities.launch_cmd,
                                [r_cmd, stdout_fname, stderr_fname])
        utilities.wait_for_pool_to_finish()


    def start_pool(self):
        #  note that except for Condor this is the inverse of how the main
        #  application allocates cores.
        if self.cur_processing_mode == "FORT Condor":
            process_count = 2 ** 32
        elif self.cur_processing_mode == "multiple models simultaneously (1 core each)":
            #  there might be multiple models running so we only get one core
            process_count = 1
        else:
            #  no other models should be running, we get to use all the cores
            process_count = multiprocessing.cpu_count() - 1

        if process_count < 1:
            process_count = 1

        utilities.start_new_pool(process_count)

    def gen_maxent_cmd(self):
        cmd = [self.java_path, '-mx512m', '-jar', self.maxent_path]
        for k, v in self.maxent_args.iteritems():
            if v in [True, False]:
                cmd.append(k + "=" + str(v).lower())
            else:
                cmd.append(k + "=" + str(v))
        return cmd

    def validateInputs(self):
        #  first manually set some instance variables from the args
        self.mdsfile = self.args_dict['c']
        self.outputdir = self.args_dict['o']
        self.maxent_args['outputdirectory'] = self.outputdir
        self.cur_processing_mode = self.args_dict['cur_processing_mode']
        self.test_key = self.args_dict.get('test_key', 'test')
        self.sub_run = self.args_dict.get('sub_run', False)
        self.maxent_path = self.args_dict.get('maxent_path', "")
        self.java_path = self.args_dict.get('java_path', self.java_path)

        #  next run our valid inputs checks.
        if not os.path.exists(self.mdsfile):
            raise RuntimeError(self, 'Input MDS, ' + self.mdsfile + ', could not be found on file system')
        if not self.maxent_args.has_key('projectionlayers'):
            self.maxent_args['projectionlayers'] = ''
        if not self.maxent_args.has_key('environmentallayers'):
            self.maxent_args['environmentallayers'] = ''
        elif self.maxent_args['environmentallayers'] != '' and \
            not os.path.isdir(self.maxent_args['environmentallayers']):
            raise RuntimeError(self, 'Input environmentallayers directory, ' + self.maxent_args['environmentallayers'] + ', could not be found on file system')
        if self.maxent_args['projectionlayers'] != '':
            dirs = self.maxent_args['projectionlayers'].split(',')
            for d in dirs:
                if not os.path.isdir(d):
                    raise RuntimeError(self, "Input 'projectionlayers' must be a directory")

        if not utilities.isMDSFile(self.mdsfile):
            raise RuntimeError(self, 'Input MDS, ' + self.mdsfile + ', does not appear to be formated as an MDS file.')
        if not os.path.exists(self.outputdir):
            raise RuntimeError(self, 'Output directory, ' + self.outputdir + ', could not be found on file system')
        if self.logger is None:
            self.logger = utilities.logger(self.outputdir, self.verbose)
        self.writetolog = self.logger.writetolog

    def prepInputs(self):
        if not self.sub_run:
            self.handleCrossValidations()

        """parses out input MDS file into the 1 to 3 SWD files that Maxent requires.
        """
        self.testCSV = os.path.join(self.outputdir, 'testSamples.csv')
        self.trainingCSV = os.path.join(self.outputdir, 'trainingSamples.csv')
        self.backgroundCSV = os.path.join(self.outputdir, 'backgroundPoints.csv')

        testWriter = csv.writer(open(self.testCSV, 'wb'))
        trainingWriter = csv.writer(open(self.trainingCSV, 'wb'))
        backgroundWriter = csv.writer(open(self.backgroundCSV, 'wb'))
        MDSreader = csv.reader(open(self.args_dict['c'], 'r'))
        header1 = MDSreader.next()
        header2 = MDSreader.next()
        header3 = MDSreader.next()
        self.pullCategoricals(header1)
        splitcol = None
        evalsplit = None
        if 'EvalSplit' in header1:
            evalsplit = header1.index('EvalSplit')
        try:
            splitcol = header1.index('Split')
            deleteTest = False
        except ValueError:
            self.writetolog("    The supplied MDS does not have a 'Split' column defaulting to having Maxent apply test/training split.")
            deleteTest = True
        covariateIndexes = self.usedIndexes(header1, header2)
        covariateNames = self.usedValues(header1, covariateIndexes)
        covariateNamesClean = [ name.replace('_categorical', '') for name in covariateNames ]
        usedCovariateFiles = self.usedValues(header3, covariateIndexes)
        self.writetolog('    Used covariates:' + ', '.join(covariateNames), False, False)
        testWriter.writerow(['full_name', 'x', 'y'] + covariateNamesClean)
        trainingWriter.writerow(['full_name', 'x', 'y'] + covariateNamesClean)
        backgroundWriter.writerow(['full_name', 'x', 'y'] + covariateNamesClean)
        hasBackground = False
        for row in MDSreader:
            self.convertNA(row)
            vals = self.usedValues(row, covariateIndexes)
            if evalsplit and row[evalsplit] == 'test':  #  handle evaluation split
                pass
            elif splitcol is None:  #  no split column
                if row[2] == '1':
                    trainingWriter.writerow([self.args_dict['species_name']] + row[:2] + vals)
                elif row[2] in ['-9999', '-9998', '0']:
                    hasBackground = True
                    backgroundWriter.writerow([''] + row[:2] + vals)
            elif not row[splitcol] == self.test_key:  #  train split
                if row[2] in ['-9999', '-9998', '0']:
                    hasBackground = True
                    backgroundWriter.writerow([''] + row[:2] + vals)
                elif row[2] == '1':
                    trainingWriter.writerow([self.args_dict['species_name']] + row[:2] + vals)
            elif row[splitcol] == self.test_key or self.testCSV == '':  #  test split only used by Maxent and only using pres data
                if row[2] == '1':
                    testWriter.writerow([self.args_dict['species_name']] + row[:2] + vals)
            else :
                pass

        if not hasBackground and self.maxent_args['environmentallayers'] == '':
            msg = '    No environmental layers were provided and '
            msg += '\n no background points were detected in the input file.'
            msg += '\n    Maxent requires pregenerated background points or environmental layer'
            msg += '\nfrom which to extract values.'
            self.writetolog(msg)
            raise RuntimeError(msg)
        elif hasBackground and self.maxent_args['environmentallayers'] != '':
            msg = '    Both background points in the MDS file and a '
            msg += '\n folder of environmental layers were specified.'
            msg += '\n    Either of these could be used by Maxent to specify/create background points.\n\n'
            msg += 'Remove either the background points from the MDSBuilder or the environmental layer.'
            self.writetolog(msg)
            raise RuntimeError(msg)
        try:
            del testWriter
            if deleteTest:
                os.remove(self.testCSV)
                self.testCSV = ''
            del backgroundWriter
            if not hasBackground:
                os.remove(self.backgroundCSV)
                self.backgroundCSV = ''
            del trainingWriter
        except:
            print ' '.join([ str(i) for i in sys.exc_info()[:2] ])
        if not self.maxent_args['projectionlayers'] != '':
            if self.maxent_args['environmentallayers'] != '':
                pass
            else:
                self.maxent_args['outputgrids'] = 'false'
            return

    def handleCrossValidations(self):
        #  Start marian adding junk to the code
        MDSreader = csv.reader(open(self.args_dict['c'], 'r'))
        header1 = MDSreader.next()
        header2 = MDSreader.next()
        header3 = MDSreader.next()

        if 'Split' in header1:

            newLine = MDSreader.next()
            ttList = ["test", "train"]
            cvList = ["NA"]
            #  find the first line of the mds that isn't na to determine if test/train or cv split
            while newLine[header1.index("Split")] == "NA":
                newLine = MDSreader.next()

            #  loop through the mds and fit a maxent model withholding each new cv fold
            if (not newLine[header1.index("Split")] in ttList):
                subrun_args = copy.deepcopy(self.args_dict)
                subrun_args["sub_run"] = 'True'
#                cvMaxent = copy.deepcopy(self)
#                cvMaxent.sub_run=True
                for row in MDSreader:
                    if (not row[header1.index("Split")] in cvList):
                        subrun_args["test_key"] = row[header1.index("Split")]
                        outdir = os.path.join(self.outputdir,
                                        "cvSplit" + row[header1.index("Split")])
                        subrun_args["o"] = outdir
                        stdout_fname = os.path.join(outdir, "stdOut.txt")
                        stderr_fname = os.path.join(outdir, "stdErr.txt")
                        os.mkdir(subrun_args["o"])

                        cvList.append(row[header1.index("Split")])
                        try:
                            cmd = [sys.executable]
                            cmd += sys.argv[:sys.argv.index("--args") + 1]

                            for k, v in subrun_args.iteritems():
                                if v in [True, False]:
                                    cmd.append(k + "=" + str(v).upper())
                                else:
                                    cmd.append(k + "=" + str(v))
                            cmd.append("maxent_args=" + str(self.maxent_args))

                            utilities.add_process_to_pool(utilities.launch_cmd,
                                [cmd, stdout_fname, stderr_fname])
                        except utilities.TrappedError as e:
                            raise RuntimeError(self, e.message)

                    #  here we need to run Maxent without the test split csv which breaks it
                    self.test_key = None
            utilities.wait_for_pool_to_finish()

    def convertNA(self, vals):
        """Switches the NA value used in our R models
        to the value expected by Maxent
        """
        for (index, item,) in enumerate(vals):
            if item == 'NA':
                vals[index] = '-9999'

    def usedIndexes(self, header1, header2):
        covariateIndexes = []
        for i in range(len(header1)):
            if header2[i] == '1' and header1[i] not in ('Weights', 'Split', 'EvalSplit'):
                covariateIndexes.append(i)

        return covariateIndexes

    def usedValues(self, values, indexes):
        usedvals = []
        for i in indexes:
            usedvals.append(values[i])

        return usedvals

    def pullCategoricals(self, headerline):
        for item in headerline:
            if item.endswith('_categorical'):
                if item not in self.categoricals:
                    self.categoricals.append(item)

    def isSWD(self, f):
        """Checks the format of a file to see if it is in the
        Maxent samples with data (SWD) format.
        """
        if os.path.exists(f):
            reader = csv.reader(open(f, 'r'))
            header = reader.next()
            if header[0].lower() in ('species', 'full_name', 'fullname'):
                return True
        return False




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

class CreatePredictorCurvesDialog(QtGui.QDialog):

    def __init__(self, kwargs, parent=None):
        #  print input_mds, output_mds, rPath, modelsPath
        self.rPath = kwargs['r_path']
        self.kwargs = kwargs
        if not self.kwargs.has_key('numPlots'):
            self.kwargs['numPlots'] = 8
        if not self.kwargs.has_key('minCor'):
            self.kwargs['minCor'] = "0.7"

        self.selection_name = os.path.split(kwargs['output_mds'])[1]
        self.selection_name = os.path.splitext(self.selection_name)[0]
        self.selection_name = self.selection_name.split('_')[-1]
        self.module = self.kwargs['module']
        self.displayJPEG = kwargs['displayJPEG']

        QtGui.QDialog.__init__(self, parent)

        self.input_mds = kwargs['input_mds']
        self.output_mds = kwargs['output_mds']
        self.outputDir = os.path.split(kwargs['output_mds'])[0]

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
        #  If an output_mds already exists then the user has run this module before.
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

        oFile = open(self.output_mds, 'wb')
        writer = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(header)
        writer.writerow(outHeader2)
        writer.writerow(header3)
        for row in reader:
            writer.writerow(row)
        oFile.close

    def update_pairs_plot(self):
        self.SaveMDSFromTreeview()
        outputPic = self.make_new_pairs_plot(self.output_mds)

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
    """Process our command line args and initiate a Maxent run
    """

    ourMaxent = HSCRunner()

    args = sys.argv[sys.argv.index("--args") + 1:]
    for arg in args:
        k, v = arg.split("=")
        if k == "maxent_args":
            ourMaxent.maxent_args = eval(v)
        elif v.lower() in ['true', 'false']:
            ourMaxent.args_dict[k] = eval(v.title())
        else:
            ourMaxent.args_dict[k] = v

    ourMaxent.run()

if __name__ == "__main__":

#    try:
    main(sys.argv)
