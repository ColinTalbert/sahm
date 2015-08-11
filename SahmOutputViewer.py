###############################################################################
#  #
#  # This file is part of the Software for Assisted Habitat Modeling package
#  # for VisTrails.
#  #
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

################################################################################
#  ImageViewer widgets/toolbar implementation
################################################################################
import time
import subprocess
import socket

from PyQt4 import QtCore, QtGui, QtWebKit

from vistrails.core.system import systemType
from vistrails.core.modules.vistrails_module import Module
from vistrails.packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
from vistrails.packages.spreadsheet.widgets.webview.webview import WebViewCellWidget
from vistrails.packages.spreadsheet.spreadsheet_base import StandardSheetReference
from vistrails.packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
from vistrails.packages.spreadsheet.spreadsheet_controller import spreadsheetController
from vistrails.core.packagemanager import get_package_manager

if systemType in ['Microsoft', 'Windows']:
    from PyQt4 import QAxContainer

import utils
import pySAHM.utilities as utilities

import os
import itertools

import GenerateModuleDoc as GenModDoc
doc_file = os.path.abspath(os.path.join(os.path.dirname(__file__), "documentation.xml"))
GenModDoc.load_documentation(doc_file)

################################################################################

class ModelOutputViewer(SpreadsheetCell):
    """
    """
    __doc__ = GenModDoc.construct_module_doc('ModelOutputViewer')
    _input_ports = [("row", "(edu.utah.sci.vistrails.basic:Integer)", {'optional': True}),
                    ("column", "(edu.utah.sci.vistrails.basic:Integer)", {'optional': True}),
                    ('ModelWorkspace', '(edu.utah.sci.vistrails.basic:Directory)'),
                    ('InitialModelOutputDisplay', '(edu.utah.sci.vistrails.basic:String)',
                     {'entry_types': "['enum']",
                      'values': "[['Text', 'Response Curves', 'AUC', 'Calibration', 'Confusion', 'Residuals']]", 'optional': True,
                      'defaults':'["AUC"]'}),
                    ('Location', '(org.vistrails.vistrails.spreadsheet:CellLocation)',
                                    {'optional':True})]
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'out')

    def findFile(self, modelDir, suffix):
        try:
            files = os.listdir(modelDir)
            for f in files:
                if f.endswith(suffix):
                    return os.path.join(modelDir, f)
                elif f.endswith(suffix.replace(".png", ".jpg")):
                    return os.path.join(modelDir, f[:-4] + ".jpg")
        except:
            return "Couldn't find file"
        return "Couldn't find file"

    def compute(self):
        """ compute() -> None
        Dispatch the display event to the spreadsheet with images and labels

        """
        model_workspace = utils.get_relative_path(self.get_input("ModelWorkspace"), self)

        if self.has_input("ModelWorkspace") and \
            utils.check_if_model_finished(model_workspace):

            auc_graph = text_output = response_curves = calibration_graph = None
            confusion_graph = residuals_graph = variable_graph = model_label = initial_display = None

            window = spreadsheetController.findSpreadsheetWindow()

            model_dir_full = os.path.normcase(model_workspace)
            model_dir = os.path.split(model_dir_full)[1]
            model_name = model_dir[:model_dir.index('_')]
            auc_graph_path = self.findFile(model_dir_full, '_modelEvalPlot.png')  #  os.path.join(model_dir_full, model_name + '_modelEvalPlot.jpg')
            if os.path.exists(auc_graph_path):
                auc_graph = window.file_pool.make_local_copy(auc_graph_path)

            text_output_path = self.findFile(model_dir_full, '_output.txt')  #  os.path.join(model_dir_full, model_name + '_output.txt')
            if os.path.exists(text_output_path):
                text_output = window.file_pool.make_local_copy(text_output_path)

            response_directory = os.path.join(model_dir_full, 'responseCurves')
            if os.path.exists(response_directory):
                responseCurveFiles = os.listdir(response_directory)
                response_curves = []
                for response_curve in responseCurveFiles:
                    if response_curve != "Thumbs.db":  #  Windows grief
                        response_curves.append(os.path.join(response_directory, response_curve))
            else:
                response_curves = []

            calibration_graph_path = self.findFile(model_dir_full, '_CalibrationPlot.png')  #  os.path.join(model_dir_full, model_name + '_CalibrationPlot.jpg')
            if os.path.exists(calibration_graph_path):
                calibration_graph = window.file_pool.make_local_copy(calibration_graph_path)

            confusion_graph_path = self.findFile(model_dir_full, '.confusion.matrix.png')  #  os.path.join(model_dir_full, model_name + '.confusion.matrix.jpg')
            if os.path.exists(confusion_graph_path):
                confusion_graph = window.file_pool.make_local_copy(confusion_graph_path)

            residuals_graph_path = self.findFile(model_dir_full, '.resid.plot.png')  #  os.path.join(model_dir_full, model_name + '.resid.plot.jpg')
            if os.path.exists(residuals_graph_path):
                residuals_graph = window.file_pool.make_local_copy(residuals_graph_path)

            variable_imp_path = self.findFile(model_dir_full, '_variable.importance.png')  #  os.path.join(model_dir_full, model_name + '_variable.importance.jpg')
            if os.path.exists(variable_imp_path):
                variable_graph = window.file_pool.make_local_copy(variable_imp_path)

            model_label = model_dir.capitalize().replace('output', 'Output')

            self.location = utils.get_sheet_location(self)

            if self.has_input('InitialModelOutputDisplay'):
                initial_display = self.get_input('InitialModelOutputDisplay')
            else:
                initial_display = 'AUC'

            self.cellWidget = self.displayAndWait(SAHMOutputViewerCellWidget, (auc_graph,
                                                                          text_output,
                                                                          response_curves,
                                                                          calibration_graph,
                                                                          confusion_graph,
                                                                          residuals_graph,
                                                                          variable_graph,
                                                                          model_label,
                                                                          initial_display))
        else:
            fileValue = None

class SAHMOutputViewerCellWidget(QCellWidget):
    """
    SAHMOutputViewerCellWidget is the widget that will display the various
    non spatial outputs from a model run
    """
    def __init__(self, parent=None):
        QCellWidget.__init__(self, parent)

        self.sync_changes = "all"

        centralLayout = QtGui.QVBoxLayout()
        self.setLayout(centralLayout)
        centralLayout.setMargin(0)
        centralLayout.setSpacing(0)

        self.Frame = QtGui.QFrame()
        self.ui = Ui_Frame()
        self.ui.setupUi(self.Frame)

        self.gs_crv_graph = QtGui.QGraphicsScene()
        self.ui.gv_crv.setScene(self.gs_crv_graph)
        QtCore.QObject.connect(self.ui.crv_combobox, QtCore.SIGNAL("currentIndexChanged(QString)"), self.changeResponseCurve)
        self.gs_crv_graph.wheelEvent = self.wheel_event_crv

        self.gs_auc_graph = QtGui.QGraphicsScene()
        self.ui.gv_auc.setScene(self.gs_auc_graph)
        self.gs_auc_graph.wheelEvent = self.wheel_event_auc

        self.gs_calibration_graph = QtGui.QGraphicsScene()
        self.ui.gv_calibration.setScene(self.gs_calibration_graph)
        self.gs_calibration_graph.wheelEvent = self.wheel_event_calibration

        self.gs_confusion_graph = QtGui.QGraphicsScene()
        self.ui.gv_confusion.setScene(self.gs_confusion_graph)
        self.gs_confusion_graph.wheelEvent = self.wheel_event_confusion

        self.gs_residuals_graph = QtGui.QGraphicsScene()
        self.ui.gv_residuals.setScene(self.gs_residuals_graph)
        self.gs_residuals_graph.wheelEvent = self.wheel_event_residuals

        self.gs_variable_graph = QtGui.QGraphicsScene()
        self.ui.gv_variable.setScene(self.gs_variable_graph)
        self.gs_variable_graph.wheelEvent = self.wheel_event_variable

        self.text_browser = QtWebKit.QWebView()
        self.text_browser.setMouseTracking(True)
        self.ui.text_output_layout.addWidget(self.text_browser)

        self.text_urlSrc = None


        self.connect(self.ui.tabWidget, QtCore.SIGNAL('currentChanged(int)'), self.tabChanged)

        self.layout().addWidget(self.Frame)

    def changeResponseCurve(self, event):
        active_cells = self.get_active_cells()
        for cell in active_cells:
            try:
                responseCurve = [rc for rc in cell.response_curves if os.path.splitext(os.path.split(rc)[1])[0] == event][0]
                pixmap_crv = QtGui.QPixmap(responseCurve)
                max_size = self.getMaxSize(cell.ui.gv_crv)
                scaled_pixmap_crv = pixmap_crv.scaled(max_size, max_size,
                                            QtCore.Qt.KeepAspectRatio,
                                            QtCore.Qt.SmoothTransformation)

                cell.images['crv_graph'] = [pixmap_crv,
                                       scaled_pixmap_crv,
                                       cell.gs_crv_graph,
                                       cell.ui.gv_crv,
                                       max_size]
                cell.ui.crv_combobox.blockSignals(True)
                curIndex = [i for i in range(cell.ui.crv_combobox.count()) if cell.ui.crv_combobox.itemText(i) == event][0]
                cell.ui.crv_combobox.setCurrentIndex(curIndex)
                cell.ui.crv_combobox.blockSignals(False)
            except IndexError:
                cell.images['crv_graph'] = [QtGui.QPixmap(),
                                       QtGui.QPixmap(),
                                       cell.gs_crv_graph,
                                       cell.ui.gv_crv,
                                       100]

                cell.ui.crv_combobox.blockSignals(True)
                cell.ui.crv_combobox.setCurrentIndex(-1)
                cell.ui.crv_combobox.blockSignals(False)
            cell.view_current()


    def tabChanged(self):
        active_cells = self.get_active_cells()

        for cell in active_cells:
            cell.ui.tabWidget.setCurrentIndex(self.ui.tabWidget.currentIndex())

    def updateContents(self, inputPorts):
        """ updateContents(inputPorts: tuple) -> None
        Update the widget contents based on the input data

        """
        (auc_graph, text_output, self.response_curves, calibration_graph, confusion_graph,
         residuals_graph, variable_graph, model_label, inital_display) = inputPorts

        self.images = {}

        if auc_graph:
            pixmap = QtGui.QPixmap(auc_graph.name)
            max_size = self.getMaxSize(self.ui.gv_auc)
            scaled_pixmap = pixmap.scaled(max_size, max_size,
                                            QtCore.Qt.KeepAspectRatio,
                                            QtCore.Qt.SmoothTransformation)

            self.images['auc_graph'] = [pixmap,
                                       scaled_pixmap,
                                       self.gs_auc_graph,
                                       self.ui.gv_auc,
                                       max_size]

        self.ui.crv_combobox.clear()
        curindex = 0
        if self.response_curves:
            for response_curve in self.response_curves:
                shortName = os.path.split(response_curve)[1]
                shortName = os.path.splitext(shortName)[0]
                if shortName != "Thumbs":
                    self.ui.crv_combobox.addItem(shortName)
                if shortName == "all_response_curves":
                    all_curves_index = curindex
                curindex += 1
            try:
                self.ui.crv_combobox.setCurrentIndex(all_curves_index)
            except:
                pass



        if calibration_graph:
            pixmap_cal = QtGui.QPixmap(calibration_graph.name)
            max_size = self.getMaxSize(self.ui.gv_calibration)
            scaled_pixmap_cal = pixmap_cal.scaled(max_size, max_size,
                                            QtCore.Qt.KeepAspectRatio,
                                            QtCore.Qt.SmoothTransformation)

            self.images['calibration_graph'] = [pixmap_cal,
                                       scaled_pixmap_cal,
                                       self.gs_calibration_graph,
                                       self.ui.gv_calibration,
                                       max_size]
        if confusion_graph:
            pixmap_con = QtGui.QPixmap(confusion_graph.name)
            max_size = self.getMaxSize(self.ui.gv_confusion)
            scaled_pixmap_con = pixmap_con.scaled(max_size, max_size,
                                            QtCore.Qt.KeepAspectRatio,
                                            QtCore.Qt.SmoothTransformation)

            self.images['confusion_graph'] = [pixmap_con,
                                       scaled_pixmap_con,
                                       self.gs_confusion_graph,
                                       self.ui.gv_confusion,
                                       max_size]

        if residuals_graph:
            pixmap_res = QtGui.QPixmap(residuals_graph.name)
            max_size = self.getMaxSize(self.ui.gv_residuals)
            scaled_pixmap_res = pixmap_res.scaled(max_size, max_size,
                                            QtCore.Qt.KeepAspectRatio,
                                            QtCore.Qt.SmoothTransformation)

            self.images['residuals_graph'] = [pixmap_res,
                                       scaled_pixmap_res,
                                       self.gs_residuals_graph,
                                       self.ui.gv_residuals,
                                       max_size]
        if variable_graph:
            pixmap_res = QtGui.QPixmap(variable_graph.name)
            max_size = self.getMaxSize(self.ui.gv_variable)
            scaled_pixmap_res = pixmap_res.scaled(max_size, max_size,
                                            QtCore.Qt.KeepAspectRatio,
                                            QtCore.Qt.SmoothTransformation)

            self.images['variable_graph'] = [pixmap_res,
                                       scaled_pixmap_res,
                                       self.gs_variable_graph,
                                       self.ui.gv_variable,
                                       max_size]

        self.text_urlSrc = QtCore.QUrl.fromLocalFile(text_output.name)
        self.text_browser.load(self.text_urlSrc)

        choices = ['Text', 'Response Curves', 'AUC', 'Calibration', 'Confusion', 'Residuals', 'Variable Importance']
        selected_index = choices.index(inital_display)
        self.ui.tabWidget.setCurrentIndex(selected_index)

        self.view_current()

        #  QCellWidget.updateContents(self, inputPorts)

    def getMaxSize(self, view):
        return self.Frame.size().width() - 10
#        if view.size().width()  <= view.size().height():
#            return view.size().width() * 0.95
#        else:
#            return view.size().height() * 0.95

    def view_current(self):
        for k, v in self.images.iteritems():
            size_img = v[1].size()
            wth, hgt = QtCore.QSize.width(size_img), QtCore.QSize.height(size_img)
            v[2].clear()
            v[2].setSceneRect(0, 0, wth, hgt)
            v[2].addPixmap(v[1])
        QtCore.QCoreApplication.processEvents()

    def wheel_event_prob(self, event):
        self.wheel_event(event, 'prob_map', QtCore.Qt.FastTransformation)

    def wheel_event_auc(self, event):
        self.wheel_event(event, 'auc_graph', QtCore.Qt.SmoothTransformation)

    def wheel_event_crv(self, event):
        self.wheel_event(event, 'crv_graph', QtCore.Qt.SmoothTransformation)

    def wheel_event_calibration(self, event):
        self.wheel_event(event, 'calibration_graph', QtCore.Qt.SmoothTransformation)

    def wheel_event_confusion(self, event):
        self.wheel_event(event, 'confusion_graph', QtCore.Qt.SmoothTransformation)

    def wheel_event_residuals(self, event):
        self.wheel_event(event, 'residuals_graph', QtCore.Qt.SmoothTransformation)

    def wheel_event_variable(self, event):
        self.wheel_event(event, 'variable_graph', QtCore.Qt.SmoothTransformation)

    def wheel_event (self, event, id, transform):
        numDegrees = event.delta() / 8
        numSteps = numDegrees / 15.0
#        self.zoom(numSteps, self.images[id], transform)
#        event.accept()

        active_cells = self.get_active_cells()
        for cell in active_cells:
#            if cell != self:
            cell.zoom(numSteps, cell.images[id], transform)
            cell.view_current()


    def zoom(self, step, images, transform):
        zoom_step = 0.06
        images[2].clear()
        w = images[1].size().width()
        h = images[1].size().height()
        w, h = w * (1 + zoom_step * step), h * (1 + zoom_step * step)
        images[1] = images[0].scaled(w, h,
                                            QtCore.Qt.KeepAspectRatio,
                                            transform)





    def saveToPNG(self, filename):
        """ saveToPNG(filename: str) -> bool
        Save the current widget contents to an image file

        """

        pixmap = QtGui.QPixmap(self.Frame.size())
        painter = QtGui.QPainter(pixmap)
        self.Frame.render(painter)
        painter.end()

        if pixmap and (not pixmap.isNull()):
            return pixmap.save(filename)
        return False

    def saveToPDF(self, filename):
        """ saveToPDF(filename: str) -> bool
        Save the current widget contents to a pdf file

        """
        printer = QtGui.QPrinter()

        printer.setOutputFormat(QtGui.QPrinter.PdfFormat)
        printer.setOutputFileName(filename)
        painter = QtGui.QPainter()
        painter.begin(printer)
        rect = painter.viewport()
        pixmap = self.label.pixmap()
        size = pixmap.size()
        size.scale(rect.size(), QtCore.Qt.KeepAspectRatio)
        painter.setViewport(rect.x(), rect.y(), size.width(), size.height())
        painter.setWindow(pixmap.rect())
        painter.drawPixmap(0, 0, pixmap)
        painter.end()


    def findSheetTabWidget(self):
        """ findSheetTabWidget() -> QTabWidget
        Find and return the sheet tab widget

        """
        p = self.parent()
        while p:
            if hasattr(p, 'isSheetTabWidget'):
                if p.isSheetTabWidget() == True:
                    return p
            p = p.parent()
        return None

    def getSAHMOutputsInCellList(self, sheet, cells):
        """  Get the list of SAHM output viewers
         inside a list of (row, column) cells.
        """
        SAHMspatials = []
        for (row, col) in cells:
            cell = sheet.getCell(row, col)
            if hasattr(cell, 'gs_auc_graph'):
                SAHMspatials.append(cell)
        return SAHMspatials

    def getSelectedCellWidgets(self):
        sheet = self.findSheetTabWidget()
        if sheet:
            selected_cells = sheet.getSelectedLocations()
            return self.getSAHMOutputsInCellList(sheet, selected_cells)
        return []

    def get_allCellWidgets(self):
        sheet = self.findSheetTabWidget()
        if sheet:
            all_cells = list(itertools.product(range(sheet.getDimension()[0]), range(sheet.getDimension()[1])))
            return self.getSAHMOutputsInCellList(sheet, all_cells)
        return []

    def get_active_cells(self):
        if self.sync_changes == "all":
            return self.get_allCellWidgets()
        elif self.sync_changes == "sel":
            return self.getSelectedCellWidgets()
        else:
            return [self]

class ImageViewerFitToCellAction(QtGui.QAction):
    """
    ImageViewerFitToCellAction is the action to stretch the image to
    fit inside a cell

    """
    def __init__(self, parent=None):
        """ ImageViewerFitToCellAction(parent: QWidget)
                                       -> ImageViewerFitToCellAction
        Setup the image, status tip, etc. of the action

        """
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(":/images/fittocell.png"),
                               "&Fit To Cell",
                               parent)
        self.setStatusTip("Scale image content to fit cell frame")
        self.setCheckable(True)
        self.setChecked(True)

    def toggledSlot(self, checked):
        """ toggledSlot(checked: boolean) -> None
        Execute the action when the button is toggled

        """
        cellWidget = self.toolBar.getSnappedWidget()
        cellWidget.label.setScaledContents(checked)
        self.toolBar.slider.updateStatus((self.toolBar.sheet,
                                          self.toolBar.row,
                                          self.toolBar.col,
                                          cellWidget))

    def updateStatus(self, info):
        """ updateStatus(info: tuple) -> None
        Updates the status of the button based on the input info

        """
        (sheet, row, col, cellWidget) = info
        self.setChecked(cellWidget.label.hasScaledContents())

class ImageViewerSaveAction(QtGui.QAction):
    """
    ImageViewerSaveAction is the action to save the image to file

    """
    def __init__(self, parent=None):
        """ ImageViewerSaveAction(parent: QWidget) -> ImageViewerSaveAction
        Setup the image, status tip, etc. of the action

        """
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(":/images/save.png"),
                               "&Save image as...",
                               parent)
        self.setStatusTip("Save image to file")

    def triggeredSlot(self, checked=False):
        """ toggledSlot(checked: boolean) -> None
        Execute the action when the button is clicked

        """
        cellWidget = self.toolBar.getSnappedWidget()
        if not cellWidget.label.pixmap() or cellWidget.label.pixmap().isNull():
            return
        fn = QtGui.QFileDialog.getSaveFileName(None, "Save image as...",
                                               "screenshot.png",
                                               "Images (*.png);;PDF files (*.pdf)")
        if not fn:
            return
        if fn.endsWith(QtCore.QString("png"), QtCore.Qt.CaseInsensitive):
            cellWidget.label.pixmap().toImage().save(fn, "png")
        elif fn.endsWith(QtCore.QString("pdf"), QtCore.Qt.CaseInsensitive):
            cellWidget.saveToPDF(str(fn))

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_Frame(object):
    def setupUi(self, Frame):
        Frame.setObjectName(_fromUtf8("Frame"))
        Frame.resize(546, 402)
        Frame.setWindowTitle(QtGui.QApplication.translate("Frame", "Frame", None, QtGui.QApplication.UnicodeUTF8))
        Frame.setFrameShape(QtGui.QFrame.StyledPanel)
        Frame.setFrameShadow(QtGui.QFrame.Raised)
        self.horizontalLayout = QtGui.QHBoxLayout(Frame)
        self.horizontalLayout.setSpacing(0)
        self.horizontalLayout.setMargin(0)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.tabWidget = QtGui.QTabWidget(Frame)
        self.tabWidget.setEnabled(True)
        self.tabWidget.setTabPosition(QtGui.QTabWidget.North)
        self.tabWidget.setObjectName(_fromUtf8("tabWidget"))

        self.text_output = QtGui.QWidget()
        self.text_output.setObjectName(_fromUtf8("text_output"))
        self.text_output_layout = QtGui.QHBoxLayout(self.text_output)
        self.text_output_layout.setSpacing(0)
        self.text_output_layout.setMargin(0)
        self.text_output_layout.setObjectName(_fromUtf8("text_output_layout"))
        self.tabWidget.addTab(self.text_output, _fromUtf8(""))
        self.tabWidget.setTabToolTip(self.tabWidget.indexOf(self.text_output), QtGui.QApplication.translate("Frame", "Textual model output ", None, QtGui.QApplication.UnicodeUTF8))

#        self.response_curves = QtGui.QWidget()
#        self.response_curves.setObjectName(_fromUtf8("response_curves"))
#        self.response_curves_layout = QtGui.QHBoxLayout(self.response_curves)
#        self.response_curves_layout.setSpacing(0)
#        self.response_curves_layout.setMargin(0)
#        self.response_curves_layout.setObjectName(_fromUtf8("response_curves_layout"))
#  #        self.response_combobox = QtGui.QComboBox(self.response_curves)
#  #        self.response_curves_layout.addWidget(self.response_combobox)
#        self.gv_response = QtGui.QGraphicsView(self.response_curves)
#        self.gv_response.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
#        self.gv_response.setTransformationAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
#        self.gv_response.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
#        self.gv_response.setObjectName(_fromUtf8("gv_response"))
#        self.response_curves_layout.addWidget(self.gv_response)
#        self.tabWidget.addTab(self.response_curves, _fromUtf8(""))
#        self.tabWidget.setTabToolTip(self.tabWidget.indexOf(self.response_curves), QtGui.QApplication.translate("Frame", "Response curves", None, QtGui.QApplication.UnicodeUTF8))

        self.crv = QtGui.QWidget()
        self.crv.setObjectName(_fromUtf8("crv"))
        self.horizontalLayout_4crv = QtGui.QVBoxLayout(self.crv)
        self.horizontalLayout_4crv.setSpacing(0)
        self.horizontalLayout_4crv.setMargin(0)
        self.horizontalLayout_4crv.setObjectName(_fromUtf8("horizontalLayout_4crv"))
        self.crv_combobox = QtGui.QComboBox(self.crv)
        self.horizontalLayout_4crv.addWidget(self.crv_combobox)
        self.gv_crv = QtGui.QGraphicsView(self.crv)
        self.gv_crv.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
        self.gv_crv.setTransformationAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_crv.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_crv.setObjectName(_fromUtf8("gv_crv"))
        self.horizontalLayout_4crv.addWidget(self.gv_crv)
        self.tabWidget.addTab(self.crv, _fromUtf8(""))

        self.auc = QtGui.QWidget()
        self.auc.setObjectName(_fromUtf8("auc"))
        self.horizontalLayout_4 = QtGui.QHBoxLayout(self.auc)
        self.horizontalLayout_4.setSpacing(0)
        self.horizontalLayout_4.setMargin(0)
        self.horizontalLayout_4.setObjectName(_fromUtf8("horizontalLayout_4"))
        self.gv_auc = QtGui.QGraphicsView(self.auc)
        self.gv_auc.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
        self.gv_auc.setTransformationAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_auc.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_auc.setObjectName(_fromUtf8("gv_auc"))
        self.horizontalLayout_4.addWidget(self.gv_auc)
        self.tabWidget.addTab(self.auc, _fromUtf8(""))
        self.tabWidget.setTabToolTip(self.tabWidget.indexOf(self.auc), QtGui.QApplication.translate("Frame", "Area under the curve (AUC)", None, QtGui.QApplication.UnicodeUTF8))

        self.calibration = QtGui.QWidget()
        self.calibration.setObjectName(_fromUtf8("calibration"))
        self.horizontalLayout_5 = QtGui.QHBoxLayout(self.calibration)
        self.horizontalLayout_5.setSpacing(0)
        self.horizontalLayout_5.setMargin(0)
        self.horizontalLayout_5.setObjectName(_fromUtf8("horizontalLayout_5"))
        self.gv_calibration = QtGui.QGraphicsView(self.calibration)
        self.gv_calibration.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
        self.gv_calibration.setTransformationAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_calibration.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_calibration.setObjectName(_fromUtf8("gv_calibration"))
        self.horizontalLayout_5.addWidget(self.gv_calibration)
        self.tabWidget.addTab(self.calibration, _fromUtf8(""))
        self.tabWidget.setTabToolTip(self.tabWidget.indexOf(self.calibration), QtGui.QApplication.translate("Frame", "Calibration plot", None, QtGui.QApplication.UnicodeUTF8))

        self.confusion = QtGui.QWidget()
        self.confusion.setObjectName(_fromUtf8("confusion"))
        self.horizontalLayout_6 = QtGui.QHBoxLayout(self.confusion)
        self.horizontalLayout_6.setSpacing(0)
        self.horizontalLayout_6.setMargin(0)
        self.horizontalLayout_6.setObjectName(_fromUtf8("horizontalLayout_6"))
        self.gv_confusion = QtGui.QGraphicsView(self.confusion)
        self.gv_confusion.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
        self.gv_confusion.setTransformationAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_confusion.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_confusion.setObjectName(_fromUtf8("gv_confusion"))
        self.horizontalLayout_6.addWidget(self.gv_confusion)
        self.tabWidget.addTab(self.confusion, _fromUtf8(""))
        self.tabWidget.setTabToolTip(self.tabWidget.indexOf(self.confusion), QtGui.QApplication.translate("Frame", "Confusion matrix", None, QtGui.QApplication.UnicodeUTF8))

        self.residuals = QtGui.QWidget()
        self.residuals.setObjectName(_fromUtf8("residuals"))
        self.horizontalLayout_7 = QtGui.QHBoxLayout(self.residuals)
        self.horizontalLayout_7.setSpacing(0)
        self.horizontalLayout_7.setMargin(0)
        self.horizontalLayout_7.setObjectName(_fromUtf8("horizontalLayout_7"))
        self.gv_residuals = QtGui.QGraphicsView(self.residuals)
        self.gv_residuals.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
        self.gv_residuals.setTransformationAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_residuals.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_residuals.setObjectName(_fromUtf8("gv_residuals"))
        self.horizontalLayout_7.addWidget(self.gv_residuals)
        self.tabWidget.addTab(self.residuals, _fromUtf8(""))

        self.variable = QtGui.QWidget()
        self.variable.setObjectName(_fromUtf8("variable"))
        self.horizontalLayout_7 = QtGui.QHBoxLayout(self.variable)
        self.horizontalLayout_7.setSpacing(0)
        self.horizontalLayout_7.setMargin(0)
        self.horizontalLayout_7.setObjectName(_fromUtf8("horizontalLayout_7"))
        self.gv_variable = QtGui.QGraphicsView(self.variable)
        self.gv_variable.setDragMode(QtGui.QGraphicsView.ScrollHandDrag)
        self.gv_variable.setTransformationAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_variable.setResizeAnchor(QtGui.QGraphicsView.AnchorUnderMouse)
        self.gv_variable.setObjectName(_fromUtf8("gv_variable"))
        self.horizontalLayout_7.addWidget(self.gv_variable)
        self.tabWidget.addTab(self.variable, _fromUtf8(""))
        self.horizontalLayout.addWidget(self.tabWidget)
        self.retranslateUi(Frame)
        self.tabWidget.setCurrentIndex(2)
        QtCore.QMetaObject.connectSlotsByName(Frame)

    def retranslateUi(self, Frame):
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.text_output), QtGui.QApplication.translate("Frame", "Text Results", None, QtGui.QApplication.UnicodeUTF8))
#        self.tabWidget.setTabText(self.tabWidget.indexOf(self.response_curves), QtGui.QApplication.translate("Frame", "Response", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.crv), QtGui.QApplication.translate("Frame", "Response Curves", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.auc), QtGui.QApplication.translate("Frame", "AUC", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.calibration), QtGui.QApplication.translate("Frame", "Calibration", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.confusion), QtGui.QApplication.translate("Frame", "Confusion", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.residuals), QtGui.QApplication.translate("Frame", "Residuals", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.variable), QtGui.QApplication.translate("Frame", "Variable Importance", None, QtGui.QApplication.UnicodeUTF8))



class ResponseCurveExplorer(SpreadsheetCell):
    """
    """
    __doc__ = GenModDoc.construct_module_doc('ResponseCurveExplorer')
    _input_ports = [('ModelWorkspaces', '(edu.utah.sci.vistrails.basic:Directory)'),
                    ('run_name_info', '(gov.usgs.sahm:OutputNameInfo:Other)', {'optional':True}),
                    ('Location', '(org.vistrails.vistrails.spreadsheet:CellLocation)',
                                    {'optional':True})]
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'out')

    def compute(self):
        """ compute() -> None
        Dispatch the display event to the spreadsheet with images and labels

        """
        model_workspaces = self.get_input_list("ModelWorkspaces")
        if len(model_workspaces) < 1 or len(model_workspaces) > 4:
            raise RuntimeError('Between 1 and 4 ModelWorkspaces must be supplied!')
         
        #TODO add in check to make sure all models finished successfully
        #if still running raise module suspended   
        workspaces = []
        for model_workspace in model_workspaces:
            rel_workspace = utils.get_relative_path(model_workspace, self)
            rel_workspace = os.path.join(rel_workspace, 'modelWorkspace')
            workspaces.append(os.path.normpath(rel_workspace))

        self.location = utils.get_curve_sheet_location(self)

        #  find the next available port
        port = 5678
        while port_occupied('127.0.0.1', port):
            print "port ({}) occupied trying next".format(port)
            port += 1
            if port > 6000:
                raise RuntimeError('Unable to find unoccupied port!')

        #  launch the Shiny app
        args = {}
        args['port'] = str(port)
        args['wsList'] = ",".join(workspaces)
        script = "ResponseCurveShinyApp.r"
        cmd = utils.gen_R_cmd(script, args)


        if self.has_input('run_name_info'):
            runinfo = self.force_get_input('run_name_info')
            subfolder = runinfo.contents.get('subfolder', "")
            runname = runinfo.contents.get('runname', "")
        else:
            subfolders = []
            runnames = []
            for outdname in workspaces:
                _subfolder, _runname = utils.get_previous_run_info(os.path.split(outdname)[0])
                subfolders.append(_subfolder)
                runnames.append(_runname)

            if all(x == subfolders[0] for x in subfolders):
                subfolder = subfolders[0]
            else:
                subfolder = ''
            if all(x == runnames[0] for x in runnames):
                runname = runnames[0]
            else:
                runname = ''


        outdname = os.path.join(utils.getrootdir(), subfolder, "ResponseCurveExplorerOutput")
        if not os.path.exists(outdname):
            os.makedirs(outdname)

        outdname = utils.mknextdir(prefix="ResponseCurveExplorerOutput", skipSequence=False, directory=os.path.join(utils.getrootdir(), subfolder))
        outfname = utils.mknextfile("ResponseCurveExplorer_stdout", suffix=".txt", directory=outdname,
                                   runname=runname)
        errfname = utils.mknextfile("ResponseCurveExplorer_stderr", suffix=".txt", directory=outdname,
                                   runname=runname)


        utils.writetolog("\nStarting processing of " + script , True)
        utils.writetolog("    command used: \n" + utilities.convert_list_to_cmd_str(cmd), False, False)

        stdErrFile = open(errfname, 'a')
        stdErrFile.seek(0, os.SEEK_END)
        stdOutFile = open(outfname, 'a')
        stdOutFile.seek(0, os.SEEK_END)

        p = subprocess.Popen(cmd, stderr=stdErrFile, stdout=stdOutFile)

        start_time = time.clock()
        CUTOFF_SECONDS = 300  #  5min

        while True:
            with open(outfname) as stdout_f:
                stdout = stdout_f.read()
                try:
                    listening = stdout.split("Listening on ")
                    url = listening[-1].strip()
                except:
                    pass

            with open(errfname) as stderr_f:
                stderr = stderr_f.read()
                if "Error" in stderr:
                    raise RuntimeError("Running Shiny App resulted in an Error:\n{}".format(stderr))

                else:
                    listening = stderr.split("Listening on ")
                    url = listening[-1].strip()

                    if url.startswith("http:"):
                        break

            time.sleep(5)
            if time.clock() - start_time > CUTOFF_SECONDS:
                msg = "Shiny App taking longer than expected to load!.\n"
                msg += "The R kernel generating the Shiny app will continue to run in the background.\n"
                msg += "Manually monitor the contents of {\n}\tfor the url which you can then open in a browser.".format(errfname)
                raise RuntimeError(msg)

        self.cellWidget = self.displayAndWait(responseCurveExplorerWidger, (p, url, None))


class responseCurveExplorerWidger(WebViewCellWidget):

    def updateContents(self, inputPorts):
        self.p, url, file = inputPorts
        WebViewCellWidget.updateContents(self, (url, file))

    def deleteLater(self):
        """ deleteLater() -> None
        Make sure to clear history and delete the widget

        """
        self.p.kill()
        WebViewCellWidget.deleteLater(self)

def port_occupied(url, port):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    result = sock.connect_ex((url, int(port)))
    return result == 0
