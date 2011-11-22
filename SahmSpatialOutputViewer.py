################################################################################
# ImageViewer widgets/toolbar implementation
################################################################################
from PyQt4 import QtCore, QtGui, QAxContainer
from core.modules.vistrails_module import Module
from packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
from packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
from packages.spreadsheet.spreadsheet_controller import spreadsheetController
from SahmSpatialViewerCell import Ui_Frame
#import imageviewer_rc
import os

import csv

from packages.sahm.pySAHM.utilities import mds_to_shape
from utils import getrasterminmax
################################################################################

def setQGIS(qgis):
    globals()["qgis"] = qgis

class SAHMSpatialOutputViewerCell(SpreadsheetCell):
    """
    SAHMModelOutputViewerCell is a VisTrails Module that
    displays the various output from a SAHM Model run in a single cell

    """
    _input_ports = [("row", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ("column", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ('model_workspace', '(edu.utah.sci.vistrails.basic:File)')]
    #all inputs are determined relative to the model_workspace

    def __init__(self):
        SpreadsheetCell.__init__(self)


    def compute(self):
        inputs = {}
        inputs["model_workspace"] = self.forceGetInputFromPort('model_workspace').name
        inputs["model_dir"] = os.path.split(inputs["model_workspace"])[0]

        for model_output in ['prob', 'bin', 'resid', 'mess', 'MoD']:
            try:
                inputs[model_output +"_map"] = os.path.join(inputs["model_dir"],
                                self.find_file(inputs["model_dir"], "_" + model_output + "_map.tif"))
            except:
                inputs[model_output + "_map"] = ""
            
        mds = self.find_mds(inputs["model_dir"])
        shaperoot = self.gen_points_shp(mds)
        inputs["pres_points"] = shaperoot + "_pres.shp"
        inputs["abs_points"] = shaperoot + "_abs.shp"
        inputs["backs_points"] = shaperoot + "_backs.shp"

        inputs["model_tag"] = os.path.split(inputs["model_dir"])[1]

        if self.hasInputFromPort("row"):
            if not self.location:
                self.location = CellLocation()
            self.location.row = self.getInputFromPort('row') - 1
        
        if self.hasInputFromPort("column"):
            if not self.location:
                self.location = CellLocation()
            self.location.col = self.getInputFromPort('column') - 1

        self.displayAndWait(SAHMSpatialOutputViewerCellWidget,
                            inputs)


    def find_file(self, model_dir, suffix):
        try:
            return [file_name for file_name in os.listdir(model_dir)
                                     if file_name.endswith(suffix)][0]
        except IndexError:
            raise RuntimeError('The expected model output ' 
                               + suffix + ' was not found in the model output directory')

    def find_mds(self, model_dir):
        model_text = os.path.join(model_dir,
                            self.find_file(model_dir, "_output.txt"))

        f = open(model_text, 'rb')
        lines = f.read().splitlines()

        # grab the line after "Data:"
        result = [lines[i + 1] for i in range(len(lines))
                  if lines[i].startswith("Data:")][0].strip()

        if os.path.exists(result):
            return result
        else:
            raise RuntimeError('Valid input MDS file not found in Model text output.')

    def gen_points_shp(self, mds):
        """I couldn't figure out how to use a render on a shapefile
        instead I split the shapefile into three separate files for each
        class.
        This split only occurs once per mds though.
        """
        parentfolder, filename = os.path.split(mds)
        filenoext = os.path.splitext(filename)[0]
        shpfolder = os.path.join(parentfolder, "PointShapefiles")

        if not os.path.exists(shpfolder):
            os.mkdir(shpfolder)
        fileroot = os.path.join(shpfolder, filenoext)

        pregenerated = True
        for type in ["_pres", "_abs", "_backs"]:
            shape_file = fileroot +  type + ".shp"
            if not os.path.exists(shape_file):
                pregenerated = False

        if not pregenerated:
            mds_to_shape(mds, shpfolder)

        return fileroot


class SAHMSpatialOutputViewerCellWidget(QCellWidget):
    """

    """
    def __init__(self, parent=None):
        """ QGISCellWidget(parent: QWidget) -> QGISCellWidget
        Initialize the widget with its central layout

        """
        QCellWidget.__init__(self, parent)

        centralLayout = QtGui.QVBoxLayout()
        self.setLayout(centralLayout)
        centralLayout.setMargin(0)
        centralLayout.setSpacing(0)

#        self.setAnimationEnabled(True)

    def updateContents(self, inputs):
        """ updateContents(inputs: dictionary) -> None
        Update the widget contents based on the input data
        """
        
#        for item in self.children():
#            self.children().remove(item)
        
#        Cell_GUI = self.gen_viewer_frame()


        self.toolBarType = SAHMSpatialViewerToolBar
        self.controlBarType = SAHMSpatialViewerToolBar

#        self.legend_label.setText(QtCore.QString(inputs["model_tag"]))


        self.all_layers = {}
        self.legends = {}
#        self.toadd = []
        layers_to_add = [{"tag":"prob_map", "type":"raster", "categorical":False, "min":0, "max":1, "num_breaks":7, "displayorder":3},
                         {"tag":"bin_map", "type":"raster", "categorical":True, "categories":[0,1], "displayorder":-1},
                         {"tag":"resid_map", "type":"raster", "categorical":False, "min":0, "max":"pullfromdata", "num_breaks":6, "displayorder":-1},
                         {"tag":"mess_map", "type":"raster", "categorical":True, "categories":"pullfromdata", "displayorder":-1},
                         {"tag":"MoD_map", "type":"raster", "categorical":False, "min":0, "max":"pullfromdata", "num_breaks":6, "displayorder":-1},
                         {"tag":"pres_points", "type":"Vector", "color":"255,0,0", "displayorder":0},
                         {"tag":"abs_points", "type":"Vector", "color":"'0,255,0'", "displayorder":1},
                         {"tag":"backs_points", "type":"Vector", "color":"'0,0,0'", "displayorder":-1}]
        
        
        for layer_args in layers_to_add:
            self.add_layer(inputs, layer_args)
        
#        self.add_raster(inputs["prob_map"], "prob_map")
#        self.legends["prob_map"] =  self.create_continuous_legend("prob_map", None, (0, 1), 6)
#        self.add_raster(inputs["bin_map"], "bin_map")
#        self.legends["bin_map"] =  self.create_continuous_legend("bin_map", None, (0, 1), 1)
#        self.add_raster(inputs["resid_map"], "resid_map")
#        self.legends["resid_map"] =  self.create_continuous_legend("resid_map", None, (0, 1), 6)
#        self.add_raster(inputs["mess_map"], "mess_map")
#        self.legends["bin_map"] =  self.create_continuous_legend("bin_map", None, (0, 1), 1)
#        self.add_raster(inputs["MoD_map"], "MoD_map")
#        self.legends["bin_map"] =  self.create_continuous_legend("bin_map", None, (0, 1), 1)
#
#        self.add_vector(inputs['pres_points'], "pres_points", '255,0,0')
#        self.add_vector(inputs['abs_points'], "abs_points", '0,255,0')
#        self.add_vector(inputs['backs_points'], "backs_points", '0,0,0')

        
#        canvas.setLayerSet([self.all_layers["pres_points"],
#                                 self.all_layers["abs_points"],
#                           self.all_layers["prob_map"]])

#        self.ui.map_frame.layout().addWidget(self.map_canvas)
#        legend = self.create_continuous_legend("prob_map", None, (0, 1), 6)
#        self.switch_legend(self.legends["prob_map"])
        
#        self.update()
        initial_layers = []
#        initial_layers.append(self.all_layers["prob_map"])
        initial_layers.append(self.all_layers["pres_points"])
        initial_layers.append(self.all_layers["abs_points"])
#        for i in range(10):
#            tag = [v["tag"] for v in layers_to_add if v["displayorder"] == i ]
#            if tag:
#                initial_layers.append(self.all_layers[tag[0]])
        
#        self.map_canvas.setExtent(self.all_layers["prob_map"].layer().extent())    
#        self.map_canvas.setLayerSet(initial_layers)
        
#        raster_fname = r"K:\GIS_LIBRARY\Climate\DAYMET\SourceData\AvgMonthlyMaxTemp\tmax1.tif"
#        raster = qgis.core.QgsRasterLayer(raster_fname, os.path.splitext(os.path.basename(raster_fname))[0])
#        qgis.core.QgsMapLayerRegistry.instance().addMapLayer(raster)
        
        
        canvas = qgis.gui.QgsMapCanvas()
        canvas.show()

        canvas.setCanvasColor(QtCore.Qt.white)
        canvas.enableAntiAliasing(True)
        canvas.setExtent(self.all_layers["prob_map"].layer().extent())
        canvas.setLayerSet(initial_layers)
        self.layout().addWidget(canvas)
        self.update()


    def switch_legend(self, new_legend):
        #
        for item in self.legend.children():
            self.legend.children().pop()
        self.legend.layout().addWidget(new_legend)

    def gen_viewer_frame(self):
        '''returns the frame that contains our split window
        gui that fills the spreadsheet cell
#        '''
#        Frame = QtGui.QFrame(self)
#        Frame.resize(734, 706)
#        Frame.setFrameShape(QtGui.QFrame.StyledPanel)
#        Frame.setFrameShadow(QtGui.QFrame.Plain)
#        gridLayout = QtGui.QGridLayout(Frame)
#        gridLayout.setMargin(0)
#        gridLayout.setSpacing(0)
#        splitter = QtGui.QSplitter(Frame)
#        splitter.setOrientation(QtCore.Qt.Vertical)
#        legend_frame_2 = QtGui.QFrame(splitter)
#        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Preferred)
#        sizePolicy.setHorizontalStretch(0)
#        sizePolicy.setVerticalStretch(0)
#        sizePolicy.setHeightForWidth(legend_frame_2.sizePolicy().hasHeightForWidth())
#        legend_frame_2.setSizePolicy(sizePolicy)
#        legend_frame_2.setMinimumSize(QtCore.QSize(0, 0))
#        legend_frame_2.setBaseSize(QtCore.QSize(0, 0))
#        legend_frame_2.setFrameShape(QtGui.QFrame.StyledPanel)
#        legend_frame_2.setFrameShadow(QtGui.QFrame.Sunken)
#
#        horizontalLayout_2 = QtGui.QHBoxLayout(legend_frame_2)
#        horizontalLayout_2.setSpacing(5)
#        horizontalLayout_2.setContentsMargins(9, 0, 0, 0)
#
#        self.legend_label = QtGui.QLabel(legend_frame_2)
#        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Minimum)
#        sizePolicy.setHorizontalStretch(0)
#        sizePolicy.setVerticalStretch(0)
#        sizePolicy.setHeightForWidth(self.legend_label.sizePolicy().hasHeightForWidth())
#        self.legend_label.setSizePolicy(sizePolicy)
#        self.legend_label.setText(QtGui.QApplication.translate("Frame", "TextLabel", None, QtGui.QApplication.UnicodeUTF8))
#
#        horizontalLayout_2.addWidget(self.legend_label)
#        self.legend = QtGui.QFrame(legend_frame_2)
#        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
#        sizePolicy.setHorizontalStretch(0)
#        sizePolicy.setVerticalStretch(0)
#        sizePolicy.setHeightForWidth(self.legend.sizePolicy().hasHeightForWidth())
#        self.legend.setSizePolicy(sizePolicy)
#        self.legend.setFrameShape(QtGui.QFrame.NoFrame)
#        self.legend.setFrameShadow(QtGui.QFrame.Plain)
#        self.legend.setLineWidth(0)
#
#        self.legend_layout = QtGui.QHBoxLayout(self.legend)
#        self.legend_layout.setMargin(0)
#
#        horizontalLayout_2.addWidget(self.legend)
#        
#        self.map_frame = QtGui.QFrame(splitter)
#        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Expanding)
#        sizePolicy.setHorizontalStretch(0)
#        sizePolicy.setVerticalStretch(1)
#        sizePolicy.setHeightForWidth(self.map_frame.sizePolicy().hasHeightForWidth())
#        self.map_frame.setSizePolicy(sizePolicy)
#        self.map_frame.setFrameShape(QtGui.QFrame.StyledPanel)
#        self.map_frame.setFrameShadow(QtGui.QFrame.Sunken)
#
#        horizontalLayout = QtGui.QHBoxLayout(self.map_frame)
#        horizontalLayout.setSpacing(0)
#        horizontalLayout.setMargin(0)
#        
#        self.map_canvas = qgis.gui.QgsMapCanvas(self.map_frame)
#        self.toolPan = qgis.gui.QgsMapToolPan(self.map_canvas)
#        self.map_canvas.setMapTool(self.toolPan)
#        self.map_canvas.setWheelAction(2)
#        self.map_canvas.setCanvasColor(QtCore.Qt.white)
#        self.map_canvas.enableAntiAliasing(True)
#        
#        self.map_frame.layout().addWidget(self.map_canvas)
##        
#        gridLayout.addWidget(splitter, 0, 0, 1, 1)
##        
#        return Frame
        



        
#        splitter = QtGui.QSplitter(self)
#        splitter.setOrientation(QtCore.Qt.Vertical)
#
#        
#        self.map_canvas = qgis.gui.QgsMapCanvas(splitter)
#        legend = QtGui.QFrame(splitter)
#        legend.layout = QtGui.QHBoxLayout(legend)
#        legend.layout.setSpacing(5)
#        legend.layout.setContentsMargins(9, 0, 0, 0)
#        
#        self.legend_label = QtGui.QLabel()
#        legend.layout.addWidget(self.legend_label)
#
#        
#        self.legend_frame = QtGui.QFrame(legend)
#        self.legend_frame.layout = QtGui.QHBoxLayout(self.legend_frame)
#        self.legend_frame.layout.setMargin(0)
#        
#        legend.layout.addWidget(self.legend_frame)
#        
#
#        splitter.addWidget(legend)
#        splitter.addWidget(self.map_canvas)
#        return splitter
        
        
        Frame = QtGui.QFrame(self)
#        Frame.setObjectName(_fromUtf8("Frame"))
        Frame.resize(734, 706)
#        Frame.setWindowTitle(QtGui.QApplication.translate("Frame", "Frame", None, QtGui.QApplication.UnicodeUTF8))
        Frame.setFrameShape(QtGui.QFrame.StyledPanel)
        Frame.setFrameShadow(QtGui.QFrame.Plain)
        gridLayout = QtGui.QGridLayout(Frame)
        gridLayout.setMargin(0)
        gridLayout.setSpacing(0)
#        gridLayout.setObjectName(_fromUtf8("gridLayout"))
        splitter = QtGui.QSplitter(Frame)
        splitter.setOrientation(QtCore.Qt.Vertical)
#        splitter.setObjectName(_fromUtf8("splitter"))
        legend_frame = QtGui.QFrame(splitter)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(legend_frame.sizePolicy().hasHeightForWidth())
        legend_frame.setSizePolicy(sizePolicy)
        legend_frame.setMinimumSize(QtCore.QSize(0, 0))
        legend_frame.setBaseSize(QtCore.QSize(0, 0))
        legend_frame.setFrameShape(QtGui.QFrame.StyledPanel)
        legend_frame.setFrameShadow(QtGui.QFrame.Sunken)
#        legend_frame_2.setObjectName(_fromUtf8("legend_frame_2"))
        horizontalLayout = QtGui.QHBoxLayout(legend_frame)
        horizontalLayout.setSpacing(5)
        horizontalLayout.setContentsMargins(9, 0, 0, 0)
#        horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.legend_label = QtGui.QLabel(legend_frame)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Minimum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.legend_label.sizePolicy().hasHeightForWidth())
        self.legend_label.setSizePolicy(sizePolicy)
#        legend_label.setText(QtGui.QApplication.translate("Frame", "TextLabel", None, QtGui.QApplication.UnicodeUTF8))
#        legend_label.setObjectName(_fromUtf8("legend_label"))
        horizontalLayout.addWidget(self.legend_label)
        self.legend = QtGui.QFrame(legend_frame)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
#        legend.setSizePolicy(sizePolicy)
#        legend.setFrameShape(QtGui.QFrame.NoFrame)
        self.legend.setFrameShadow(QtGui.QFrame.Plain)
        self.legend.setLineWidth(0)
#        self.legend.setObjectName(_fromUtf8("legend"))
        legend_layout = QtGui.QHBoxLayout(self.legend)
        legend_layout.setMargin(0)
#        legend_layout.setObjectName(_fromUtf8("legend_layout"))
        horizontalLayout.addWidget(self.legend)
        map_frame = QtGui.QFrame(splitter)
#        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Expanding)
#        sizePolicy.setHorizontalStretch(0)
#        sizePolicy.setVerticalStretch(1)
#        sizePolicy.setHeightForWidth(map_frame.sizePolicy().hasHeightForWidth())
#        map_frame.setSizePolicy(sizePolicy)
#        map_frame.setFrameShape(QtGui.QFrame.StyledPanel)
#        map_frame.setFrameShadow(QtGui.QFrame.Sunken)
#        map_frame.setObjectName(_fromUtf8("map_frame"))
        horizontalLayout = QtGui.QHBoxLayout(map_frame)
        horizontalLayout.setSpacing(0)
        horizontalLayout.setMargin(0)
#        horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        gridLayout.addWidget(splitter, 0, 0, 1, 1)
        
        
        
        
        self.map_canvas = qgis.gui.QgsMapCanvas()
        map_frame.layout().addWidget(self.map_canvas)

        return splitter

    def dumpToFile(self, filename):
        pass

    def saveToPDF(self, filename):
        pass

    def add_layer(self, inputs, kwargs):
        if kwargs["type"] == "raster":
            self.add_raster(inputs[kwargs["tag"]], kwargs["tag"])
            self.add_legend(inputs[kwargs["tag"]], kwargs)                                         
        else:
            self.add_vector(inputs[kwargs["tag"]], kwargs["tag"], kwargs["color"])

    def add_raster(self, path, tag):
        if os.path.exists(path):
            fileInfo = QtCore.QFileInfo(path)
            baseName = fileInfo.baseName()
            raster = qgis.core.QgsRasterLayer(path, baseName)
    
            if tag=="resid_map":
                min_max = getrasterminmax(path)
            else:
                min_max = None
            self.set_color_ramp(tag, raster, min_max)
            self.all_layers[tag] = qgis.gui.QgsMapCanvasLayer(raster)
            qgis.core.QgsMapLayerRegistry.instance().addMapLayer(raster)
            return True
        else:
            return False          

    def add_vector(self, path, tag, strcolor):
        fileInfo = QtCore.QFileInfo(path)
        baseName = fileInfo.baseName()
        points_layer = qgis.core.QgsVectorLayer(path, baseName, "ogr")

#        props = {'color':strcolor, 'radius':'3' }
#        s = qgis.core.QgsMarkerSymbolV2.createSimple(props)
#
#        points_layer.setRendererV2( qgis.core.QgsSingleSymbolRendererV2( s ) )

        qgis.core.QgsMapLayerRegistry.instance().addMapLayer(points_layer)
        self.all_layers[tag] = qgis.gui.QgsMapCanvasLayer(points_layer)
        
    def add_legend(self, path, kwargs):
        '''Adds a legend to our legends dict based on the raster and 
        parameters passed
        '''
        if kwargs['categorical']:
            legend = self.create_categorical_legend(kwargs)
#            self.legends[kwargs["tag"]] =  legend
        else:
            self.legends[kwargs["tag"]] =  self.create_continuous_legend(kwargs["tag"], None, (0, 1), 6)

    def set_color_ramp(self, layer_type, raster, min_max=None):
        '''creates and applies a qgis color ramp to correctly 
        display the input raster layer according to the entries in the 
        csv
        '''
        QgsColorRampShader = qgis.core.QgsColorRampShader

        if layer_type == "MoD_map":
            MoD is categorical
            raster.setDrawingStyle(qgis.core.QgsRasterLayer.SingleBandPseudoColor)
            raster.setColorShadingAlgorithm(qgis.core.QgsRasterLayer.PseudoColorShader)
            return None
        else:
            raster.setDrawingStyle(qgis.core.QgsRasterLayer.SingleBandPseudoColor)

        csv_file = os.path.abspath(os.path.join(os.path.dirname(__file__), "ColorBreaks.csv"))
        csvfile = open(csv_file, "r")
        reader = csv.reader(csvfile)
        header = reader.next() #skip the header

        color_ramp_items = []
        for row in reader:
            if row[0] == layer_type:
                r, g, b = [int(val) for val in row[2:5]]
                cur_color = QtGui.QColor(r, g, b)
                cur_val = float(row[1])
                color_item = QgsColorRampShader.ColorRampItem(cur_val, cur_color)
                color_ramp_items.append(color_item)

        raster.setColorShadingAlgorithm(qgis.core.QgsRasterLayer.ColorRampShader)
        fcn = raster.rasterShader().rasterShaderFunction()
        fcn.setColorRampType(QgsColorRampShader.INTERPOLATED)
        fcn.setColorRampItemList(color_ramp_items)

        if hasattr(raster, "setCacheImage"): raster.setCacheImage(None)
        raster.triggerRepaint()

        qgis.core.QgsMapLayerRegistry.instance().addMapLayer(raster)

        csvfile.close()

        return None

    def create_continuous_legend(self, layer_type, raster, min_max, num_tags):
        '''Generates a qframe that contains the color ramp with labels
        that gets displayed in the legend frame
        '''
        legend = QtGui.QFrame()
        mainlayout = QtGui.QVBoxLayout(legend)
        mainlayout.setSpacing(0)
        mainlayout.setMargin(0)

        frame_colorbar = QtGui.QFrame(legend)
#        frame_colorbar.setObjectName(QtCore.QString.fromUtf8("frame_colorbar"))
        layout_colorbar = QtGui.QHBoxLayout(frame_colorbar)
        layout_colorbar.setSpacing(0)
        layout_colorbar.setContentsMargins(-1, 2, -1, 0)

        frame_ticks = QtGui.QFrame(legend)
        frame_ticks.setFrameShape(QtGui.QFrame.StyledPanel)
        frame_ticks.setFrameShadow(QtGui.QFrame.Raised)
#        frame_ticks.setObjectName(QtCore.QString.fromUtf8("frame_ticks"))
        layout_ticks = QtGui.QHBoxLayout(frame_ticks)
        layout_ticks.setSpacing(0)
        layout_ticks.setContentsMargins(-1, 0, -1, 0)

        frame_labels = QtGui.QFrame(legend)
#        frame_labels.setObjectName(QtCore.QString.fromUtf8("frame_labels"))
        layout_labels = QtGui.QHBoxLayout(frame_labels)
        layout_labels.setSpacing(0)
        layout_labels.setContentsMargins(4, 0, 4, 0)

        csv_file = os.path.abspath(os.path.join(os.path.dirname(__file__), "ColorBreaks.csv"))
        csvfile = open(csv_file, "r")
        reader = csv.reader(csvfile)
        header = reader.next() #skip the header

        color_ramp_items = []
        prev_color = None
        for row in reader:
            if row[0] == layer_type:
                cur_color = ", ".join(row[2:5])

                if prev_color:
                    color_label = QtGui.QLabel(frame_colorbar)
                    sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.MinimumExpanding)
                    sizePolicy.setHorizontalStretch(0)
                    sizePolicy.setVerticalStretch(0)
                    sizePolicy.setHeightForWidth(color_label.sizePolicy().hasHeightForWidth())
                    color_label.setSizePolicy(sizePolicy)
                    color_label.setMinimumSize(QtCore.QSize(0, 4))
                    stylesheet = "background-color: qlineargradient(spread:pad, x1:0, y1:0, x2:1, y2:0, stop:0 rgba("
                    stylesheet += prev_color
                    stylesheet += ", 255), stop:1 rgba("
                    stylesheet += cur_color
                    stylesheet += ", 255));"
#                    color_label.setStyleSheet(QtCore.QString.fromUtf8(stylesheet))
                    color_label.setText(QtCore.QString.fromUtf8(""))
                    layout_colorbar.addWidget(color_label)

                prev_color = cur_color

        min = min_max[0]
        max = min_max[1]
        step = float(max - min) / num_tags
        curStep = min

        while curStep <= max:
            line = QtGui.QFrame(frame_ticks)
            sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Minimum)
            sizePolicy.setHorizontalStretch(0)
            sizePolicy.setVerticalStretch(0)
            sizePolicy.setHeightForWidth(line.sizePolicy().hasHeightForWidth())
            line.setSizePolicy(sizePolicy)
            line.setMinimumSize(QtCore.QSize(0, 4))
            line.setSizeIncrement(QtCore.QSize(0, 0))
            line.setFrameShadow(QtGui.QFrame.Plain)
            line.setFrameShape(QtGui.QFrame.VLine)
            line.setFrameShadow(QtGui.QFrame.Sunken)
            line.setObjectName(QtCore.QString.fromUtf8("line"))
            layout_ticks.addWidget(line)
            spacerItem = QtGui.QSpacerItem(133, 2, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
            layout_ticks.addItem(spacerItem)

            lbl = QtGui.QLabel(frame_labels)
            lbl.setMinimumSize(QtCore.QSize(0, 10))
            txt = "%.1f" %curStep
            lbl.setText(QtCore.QString.fromUtf8(txt))
            lbl.setAlignment(QtCore.Qt.AlignCenter)
            #lbl.setObjectName(QtCore.QString.fromUtf8("lbl"))
            layout_labels.addWidget(lbl)
            spacerItem2 = QtGui.QSpacerItem(5, 5, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
            layout_labels.addItem(spacerItem2)

            curStep += step

        layout_ticks.removeItem(spacerItem)
        layout_labels.removeItem(spacerItem2)

        mainlayout.addWidget(frame_colorbar)
        mainlayout.addWidget(frame_ticks)
        mainlayout.addWidget(frame_labels)
        return legend




#
#def update_displayed_layers(cellWidget):
#    pass
#
#
##        [qgis.gui.QgsMapCanvasLayer(layer) for layer in layers]

#    layer = cellWidget.cur_layers[layer]
#    layers = cellWidget.canvas.layers()
#
#    if visible:
#        layers.insert(0, layer.layer())
#    else:
#        layers.remove(layer.layer())
#
#    cellWidget.canvas.setLayerSet(layerset)
#    cellWidget.canvas.repaint()

    def create_categorical_legend(self, kwargs):
        pass

class ViewLayerAction(QtGui.QAction):
    def __init__(self, action_dict, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "Images", action_dict["icon"]))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               action_dict["label"],
                               parent)
        self.setCheckable(True)
        self.setChecked(action_dict["checked"])
        self.tag = action_dict["tag"]
        self.group = action_dict["group"]

    def triggeredSlot(self, checked=False):
#        cellWidget = self.toolBar.getSnappedWidget()
        self.toggleOthers()
        self.displayLayer()

    def toggleOthers(self):
        '''Unselect the other raster layers
        '''
        for action in self.toolBar.actions():
            if "group" in dir(action) and \
                action.group == self.group and \
                action.tag <> self.tag:
                action.setChecked(False)

    def displayLayer(self):
        '''Display all the layers that have their
        actions selected in the toolbar
        '''
        cellWidget = self.toolBar.getSnappedWidget()

        all_layers = cellWidget.all_layers
        layerset = []
        for action in self.toolBar.actions():
            if action.isChecked() and all_layers.has_key(action.tag):
                layer = all_layers[action.tag]
                layerset.append(qgis.gui.QgsMapCanvasLayer(layer))

        cellWidget.map_canvas.setLayerSet(layerset)
#        cellWidget.canvas.repaint()
        cellWidget.update()

class SAHMSpatialViewerToolBar(QCellToolBar):
    """
    The toolbar that allows users to toggle layers on and off
    in the widget

    """
    def createToolBar(self):
        """ createToolBar() -> None
        This will get call initiallly to add customizable widgets

        """
        sw = self.getSnappedWidget()
        
        actions = [{"tag":"pres_points", "icon":"RedPoints.png",
                     "checked":True, "label":"Display presence points",
                     "group":"pres_points"},
                   {"tag":"abs_points", "icon":"GreenPoints.png",
                     "checked":True, "label":"Display absence points",
                     "group":"abs_points"},
                   {"tag":"backs_points", "icon":"BlackPoints.png",
                     "checked":False, "label":"Display background points",
                     "group":"backs_points"},
                   {"tag":"prob_map", "icon":"ProbMap.png",
                     "checked":True, "label":"Display probability map",
                     "group":"Grids"},
                   {"tag":"bin_map", "icon":"BinMap.png",
                     "checked":False, "label":"Display binary map",
                     "group":"Grids"},
                   {"tag":"res_map", "icon":"ResMap.png",
                     "checked":False, "label":"Display residuals map",
                     "group":"Grids"},
                   {"tag":"mess_map", "icon":"MesMap.png",
                     "checked":False, "label":"Display Multivariate Environmental Similarity Surface (Mess) map",
                     "group":"Grids"},
                    {"tag":"MoD_map", "icon":"ModMap.png",
                     "checked":False, "label":"Display Most Dissimilar Variable (MoD) map",
                     "group":"Grids"}]


        for action_dict in actions:
            self.appendAction(ViewLayerAction(action_dict, self))
            
            
    def updateToolBar(self):
        QCellToolBar.updateToolBar(self)
        sw = self.getSnappedWidget()
        for action in self.actions():
            if type(action) == ViewLayerAction:
                #disenable all action refering to data we don't have
                action.setEnabled(action.tag in sw.all_layers)

        