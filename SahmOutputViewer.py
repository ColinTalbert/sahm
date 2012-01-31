################################################################################
# ImageViewer widgets/toolbar implementation
################################################################################
from PyQt4 import QtCore, QtGui, QAxContainer
from core.modules.vistrails_module import Module
from packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
from packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
from packages.spreadsheet.spreadsheet_controller import spreadsheetController

from packages.sahm.sahm_picklists import ModelOutputType

import os
################################################################################

class SAHMModelOutputViewerCell(SpreadsheetCell):
    """
SAHMModelOutputViewerCell

Description:
        The SAHM Spatial Output Viewer Cell provides a convenient means for viewing the numerous
    spatial outputs produced by individual model runs as well as the input presence and absence
    points and background points if applicable.  The spatial viewer displays the outputs in an
    interactive Matplotlib chart which functions much like a full GIS.

        Attached to each cell is a toolbar that allows changing of the displayed layer and the
    overlaid points

Input Ports:
    row:  (optional)
        The spread sheet row that the output will be placed in. Counts start from 1 not 0.
        Default value = VisTrails will default to the next availible cell on the spreadsheet.

    column:  (optional)
        The spread sheet column that the output will be placed in. Counts start from 1 not 0.
        Default value = VisTrails will default to the next availible cell on the spreadsheet.

    ModelWorkspace:  (mandatory)
        The ModelWorkspace is a file titled modelWorkspace (no extension) that is saved in each
        model output folder.  All the individual outputs will be identified relative to the location
        of this file.

    InitialModelOutputDisplay:  (optional)
        The display tab to show initially.
        Options are:
            Text
            Response Curves
            AUC Curves
            Calibration
            Confusion
            Residuals
    """
    _input_ports = [("row", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ("column", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ('ModelWorkspace', '(edu.utah.sci.vistrails.basic:File)'),
                    ('InitialModelOutputDisplay', '(gov.usgs.sahm:ModelOutputType:Other)', {'defaults':str(['AUC'])})
                    ]
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out') 
     
    def compute(self):
        """ compute() -> None
        Dispatch the display event to the spreadsheet with images and labels
        
        """
        if self.hasInputFromPort("ModelWorkspace"):
            window = spreadsheetController.findSpreadsheetWindow()
            model_workspace = self.getInputFromPort("ModelWorkspace").name
            model_dir_full = os.path.normcase(os.path.split(model_workspace)[0])
            model_dir = os.path.split(model_dir_full)[1]
            model_name = model_dir[:model_dir.index('_')]
            
            
            auc_graph_path = os.path.join(model_dir_full, model_name + '_modelEvalPlot.jpg')
            auc_graph = window.file_pool.make_local_copy(auc_graph_path)
            
            text_output_path = os.path.join(model_dir_full, model_name + '_output.txt')
            text_output = window.file_pool.make_local_copy(text_output_path)
            
            response_path = os.path.join(model_dir_full, model_name + '_response_curves.pdf')
            response_curves = window.file_pool.make_local_copy(response_path)
            
            calibration_graph_path = os.path.join(model_dir_full, model_name + '_CalibrationPlot.jpg')
            calibration_graph = window.file_pool.make_local_copy(calibration_graph_path)
            
            confusion_graph_path = os.path.join(model_dir_full, model_name + '.confusion.matrix.jpg')
            confusion_graph = window.file_pool.make_local_copy(confusion_graph_path)
            
            residuals_graph_path = os.path.join(model_dir_full, model_name + '.resid.plot.jpg')
            residuals_graph = window.file_pool.make_local_copy(residuals_graph_path)
            
            model_label = model_dir.capitalize().replace('output', 'Output')
            
            
            if self.hasInputFromPort("row"):
                if not self.location:
                    self.location = CellLocation()
                self.location.row = self.getInputFromPort('row') - 1
            
            if self.hasInputFromPort("column"):
                if not self.location:
                    self.location = CellLocation()
                self.location.col = self.getInputFromPort('column') - 1
                
            if self.hasInputFromPort('InitialModelOutputDisplay'):
                initial_display = self.getInputFromPort('InitialModelOutputDisplay')
            else:
                initial_display = 'AUC'
            
        else:
            fileValue = None
            
            
        self.cellWidget = self.displayAndWait(SAHMOutputViewerCellWidget, (auc_graph, 
                                                                      text_output,
                                                                      response_curves,
                                                                      calibration_graph,
                                                                      confusion_graph,
                                                                      residuals_graph,
                                                                      model_label,
                                                                      initial_display))

class SAHMOutputViewerCellWidget(QCellWidget):
    """
    SAHMOutputViewerCellWidget is the widget that will display the various
    non spatial outputs from a model run
    """
    def __init__(self, parent=None):
        QCellWidget.__init__(self, parent)
        
        centralLayout = QtGui.QVBoxLayout()
        self.setLayout(centralLayout)
        centralLayout.setMargin(0)
        centralLayout.setSpacing(0)

        
#        self.setAnimationEnabled(True)
        
        self.Frame = QtGui.QFrame()
        self.ui = Ui_Frame()
        self.ui.setupUi(self.Frame)
        
#        #add scenes to our graphicViews
#        self.gs_prob_map = QtGui.QGraphicsScene()
#        self.ui.gv_prob_map.setScene(self.gs_prob_map)
#        self.gs_prob_map.wheelEvent = self.wheel_event_prob
        
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
        
        #add in ie browsers for the text and response
        self.text_browser = QAxContainer.QAxWidget(self)
        self.text_browser.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.text_browser.setControl("{8856F961-340A-11D0-A96B-00C04FD705A2}")
        self.ui.text_output_layout.addWidget(self.text_browser)
        self.text_urlSrc = None
        
        self.response_browser = QAxContainer.QAxWidget(self)
        self.response_browser.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.response_browser.setControl("{8856F961-340A-11D0-A96B-00C04FD705A2}")
        self.ui.response_curves_layout.addWidget(self.response_browser)
        self.response_urlSrc = None
        
        
        self.layout().addWidget(self.Frame)

    def updateContents(self, inputPorts):
        """ updateContents(inputPorts: tuple) -> None
        Update the widget contents based on the input data
        
        """
        (auc_graph, text_output, response_curves, calibration_graph, confusion_graph, 
         residuals_graph, model_label, inital_display) = inputPorts
        
        self.images = {}
#        if prob_map:
#            #Value = (full image, sized image, scene, view, max_height)
#            pixmap = QtGui.QPixmap(prob_map.name)
#            max_size = self.getMaxSize(self.ui.gv_prob_map)
#            scaled_pixmap = pixmap.scaled(max_size, max_size, 
#                                            QtCore.Qt.KeepAspectRatio, 
#                                            QtCore.Qt.FastTransformation)
#            
#            self.images['prob_map'] = [pixmap,
#                                       scaled_pixmap,
#                                       self.gs_prob_map,
#                                       self.ui.gv_prob_map,
#                                       max_size]
        
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
        

        self.text_urlSrc = QtCore.QUrl.fromLocalFile(text_output.name)
        if self.text_urlSrc!=None:
            self.text_browser.dynamicCall('Navigate(const QString&)', self.text_urlSrc.toString())
        else:
            self.text_browser.dynamicCall('Navigate(const QString&)', QtCore.QString('about:blank'))

        self.response_urlSrc = QtCore.QUrl.fromLocalFile(response_curves.name)
        if self.response_urlSrc!=None:
            self.response_browser.dynamicCall('Navigate(const QString&)', self.response_urlSrc.toString())
        else:
            self.response_browser.dynamicCall('Navigate(const QString&)', QtCore.QString('about:blank'))
        
        choices = ['Text', 'Response Curves', 'AUC', 'Calibration', 'Confusion', 'Residuals']
        selected_index = choices.index(inital_display)
        self.ui.tabWidget.setCurrentIndex(selected_index)

        self.view_current()

        #QCellWidget.updateContents(self, inputPorts)

    def getMaxSize(self, view):
        return self.Frame.size().width() - 10
#        if view.size().width()  <= view.size().height(): 
#            return view.size().width() * 0.95
#        else: 
#            return view.size().height() * 0.95
    
    def view_current(self):
        for k,v in self.images.iteritems():
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

    def wheel_event_calibration(self, event):
        self.wheel_event(event, 'calibration_graph', QtCore.Qt.SmoothTransformation)
    
    def wheel_event_confusion(self, event):
        self.wheel_event(event, 'confusion_graph', QtCore.Qt.SmoothTransformation)
        
    def wheel_event_residuals(self, event):
        self.wheel_event(event, 'residuals_graph', QtCore.Qt.SmoothTransformation)
       
    def wheel_event (self, event, id, transform):
        numDegrees = event.delta() / 8 
        numSteps = numDegrees / 15.0 
        self.zoom(numSteps, self.images[id], transform) 
        event.accept() 

    def zoom(self, step, images, transform):
        zoom_step = 0.06
        images[2].clear() 
        w = images[1].size().width() 
        h = images[1].size().height() 
        w, h = w * (1 + zoom_step*step), h * (1 + zoom_step*step) 
        images[1] = images[0].scaled(w, h, 
                                            QtCore.Qt.KeepAspectRatio, 
                                            transform) 
        self.view_current() 

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
#
#    def resizeEvent(self, e):
#        if self.originalPix!=None:
#            self.label.setPixmap(self.originalPix.scaled(self.label.size(),
#                                                         QtCore.Qt.KeepAspectRatio,
#                                                         QtCore.Qt.SmoothTransformation))
#                

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
        
        self.response_curves = QtGui.QWidget()
        self.response_curves.setObjectName(_fromUtf8("response_curves"))
        self.response_curves_layout = QtGui.QHBoxLayout(self.response_curves)
        self.response_curves_layout.setSpacing(0)
        self.response_curves_layout.setMargin(0)
        self.response_curves_layout.setObjectName(_fromUtf8("response_curves_layout"))
        self.tabWidget.addTab(self.response_curves, _fromUtf8(""))
        self.tabWidget.setTabToolTip(self.tabWidget.indexOf(self.response_curves), QtGui.QApplication.translate("Frame", "Response curves", None, QtGui.QApplication.UnicodeUTF8))
        
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
        self.tabWidget
        
        self.horizontalLayout.addWidget(self.tabWidget)
        self.retranslateUi(Frame)
        self.tabWidget.setCurrentIndex(2)
        QtCore.QMetaObject.connectSlotsByName(Frame)

    def retranslateUi(self, Frame):
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.text_output), QtGui.QApplication.translate("Frame", "Text Results", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.response_curves), QtGui.QApplication.translate("Frame", "Response", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.auc), QtGui.QApplication.translate("Frame", "AUC", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.calibration), QtGui.QApplication.translate("Frame", "Calibration", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.confusion), QtGui.QApplication.translate("Frame", "Confusion", None, QtGui.QApplication.UnicodeUTF8))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.residuals), QtGui.QApplication.translate("Frame", "Residuals", None, QtGui.QApplication.UnicodeUTF8))