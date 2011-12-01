################################################################################
# ImageViewer widgets/toolbar implementation
################################################################################
import os
import csv

from PyQt4 import QtCore, QtGui, QAxContainer
from core.modules.vistrails_module import Module
from packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
from packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
from packages.spreadsheet.spreadsheet_controller import spreadsheetController

import matplotlib
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QTAgg as NavigationToolbar
from matplotlib.figure import Figure
from matplotlib.offsetbox import AnchoredOffsetbox, TextArea
import matplotlib.colors as colors

import numpy as np

from osgeo import gdal, gdalconst


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
        inputs["model_dir"] = os.path.split(os.path.normcase(inputs["model_workspace"]))[0]

        for model_output in ['prob', 'bin', 'resid', 'mess', 'MoD']:
            try:
                inputs[model_output +"_map"] = os.path.join(inputs["model_dir"],
                                self.find_file(inputs["model_dir"], "_" + model_output + "_map.tif"))
            except:
                inputs[model_output + "_map"] = ""
            
        inputs["mds"] = self.find_mds(inputs["model_dir"])

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
        self.create_main_frame()
        
        self.setAnimationEnabled(True)

    def updateContents(self, inputs):
        """ updateContents(inputs: dictionary) -> None
        Update the widget contents based on the input data
        """
        self.toolBarType = SAHMSpatialViewerToolBar
        self.controlBarType = SAHMSpatialViewerToolBar
        self.inputs = inputs
        
        self.load_layers()
        self.on_draw()
        self.update()
        
    def create_main_frame(self):
#        self.main_frame = QtGui.QWidget(self)
#        
#        splitter = QtGui.QSplitter(self)
#        splitter.setOrientation(QtCore.Qt.Vertical)

        self.dpi = 100
        self.fig = Figure((5.0, 4.0), dpi=self.dpi)
        self.fig.subplots_adjust(left = 0, right=1, top=1, bottom=0)
        self.map_canvas = MyDiagram(self.fig)
        
        self.add_axis()
        
        
##        self.map_canvas.setParent(self)
#        legend = QtGui.QFrame(splitter)
#        legend.setFrameShape(QtGui.QFrame.StyledPanel)
#        legend.layout = QtGui.QHBoxLayout(legend)
#        legend.layout.setSpacing(5)
#        legend.layout.setContentsMargins(9, 0, 0, 0)
#        
#        self.legend_label = QtGui.QLabel()
#        legend.layout.addWidget(self.legend_label)
#        
##        colorbar_frame = QtGui.QFrame(legend)
##        colorbar_frame.setSizePolicy(QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum))
##        colorbar_frame.layout(QtGui.QHBoxLayout(colorbar_frame))
#        
#
#        self.legend_fig = Figure((5.0, 0.4), dpi=self.dpi)
#        self.legend_fig.set_facecolor('w')
#        self.legend_fig.subplots_adjust(left = 0, right=1, top=1, bottom=0)
#        
##        self.legend_frame = QtGui.QFrame(legend)
##        self.legend_frame.layout = QtGui.QHBoxLayout(self.legend_frame)
##        self.legend_frame.layout.setMargin(0)
##        self.legend_axes = self.legend_fig.add_subplot(1, 1, 1)
##        self.legend_axes.axes('off')
#        self.legend_canvas = MyDiagram(self.legend_fig)
#        self.legend_canvas.setSizePolicy(QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Expanding))
#        self.legend_axes = self.legend_fig.add_subplot(111)
#        self.legend_axes.spines['right'].set_color('none')
#        self.legend_axes.spines['top'].set_color('none')
#        self.legend_axes.spines['bottom'].set_color('none')
#        self.legend_axes.spines['left'].set_color('none')
#        self.legend_axes.get_xaxis().set_visible(False)
#        self.legend_axes.get_yaxis().set_visible(False)
#        
##        colorbar_frame.layout.addWidget(self.legend_canvas)
#        legend.layout.addWidget(self.legend_canvas)
#        
#
#        splitter.addWidget(legend)
#        splitter.addWidget(self.map_canvas)
        
        
        
#        # Create the mpl Figure and FigCanvas objects. 
#        # 5x4 inches, 100 dots-per-inch
#        #
#        self.dpi = 100
#        self.fig = Figure((5.0, 4.0), dpi=self.dpi)
#        self.canvas = FigureCanvas(self.fig)
#        self.canvas.setParent(self)

        
        self.mpl_toolbar = NavigationToolbar(self.map_canvas, None)
        #Strip out the unused actions
        keep_actions = ['Home', 'Back', 'Forward', 'Pan', 'Zoom', 'Save']
        for action in self.mpl_toolbar.actions():
            if not action.text() in keep_actions and action.text():
                self.mpl_toolbar.removeAction(action)
        
        self.layout().addWidget(self.map_canvas)    
    
    def load_layers(self):
        self.displayTL = True
        self.all_layers = {"prob_map":{"type":"raster", "title":"Probability" ,"categorical":False, "min":0, "max":1, 'cmap':matplotlib.cm.jet, "displayorder":9999, "displayed":True, "enabled":False, "file":""},
                         "bin_map":{"type":"raster", "title":"Binary probability" , "categorical":True, "categories":[0,1], 'cmap':matplotlib.cm.Greys, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "resid_map":{"type":"raster", "title":"Residuals" , "categorical":False, "min":0, "max":"pullfromdata", 'cmap':matplotlib.cm.Accent, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "mess_map":{"type":"raster", "title":"Mess" , "categorical":True, "categories":"pullfromdata", 'cmap':matplotlib.cm.BrBG, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "MoD_map":{"type":"raster", "title":"MoD" , "categorical":False, "min":0, "max":"pullfromdata", 'cmap':matplotlib.cm.prism, "displayorder":9999, "num_breaks":7, "displayed":False, "enabled":False, "file":""},
                         "pres_points":{"type":"Vector", "color":(1,0,0), "displayorder":3, "num_breaks":7, "displayed":True, "enabled":False, "file":""},
                         "abs_points":{"type":"Vector", "color":(0,1,0), "displayorder":2, "num_breaks":7, "displayed":True, "enabled":False, "file":""},
                         "backs_points":{"type":"Vector", "color":(0,0,0), "displayorder":1, "num_breaks":7, "displayed":False, "enabled":False, "file":""}}
        
        for k,v in self.all_layers.items():
            if k in self.inputs.keys():
                if os.path.exists(self.inputs[k]):
                    self.all_layers[k]["file"] = self.inputs[k]
                    self.all_layers[k]["enabled"] = True
                 
        #make our specialty colormaps
        self.all_layers["resid_map"]["cmap"] = self.make_resid_cmap(self.all_layers["resid_map"])
                 
        pointfile = self.inputs["mds"]
        points = np.genfromtxt(pointfile, delimiter=",", skip_header=3)
                    
        for name, val in {"abs_points":0, "pres_points":1, "backs_points":-9999}.items():
            #parse out the x, y s for the points in each of our categoreis
            self.all_layers[name]['x'] = np.delete(points, np.argwhere(points[:,2]<>val), 0)[:,0]
            self.all_layers[name]['y'] = np.delete(points, np.argwhere(points[:,2]<>val), 0)[:,1]
            if len(self.all_layers[name]['x']) == 0:
                self.all_layers[name]["enabled"] = False
            else:
                self.all_layers[name]["enabled"] = True
               
    def add_axis(self):
        self.axes = self.fig.add_subplot(111, aspect='equal', adjustable='datalim')
        self.axes.spines['right'].set_color('none')
        self.axes.spines['top'].set_color('none')
        self.axes.spines['bottom'].set_color('none')
        self.axes.spines['left'].set_color('none')
        self.axes.get_xaxis().set_visible(False)
        self.axes.get_yaxis().set_visible(False)
        
    def add_title(self, title):
        at = AnchoredText(title,
                          loc=2, frameon=True, pad=.05, borderpad=0.2)
        at.patch.set_boxstyle("round,rounding_size=0.2")
        at.set_alpha(0.1)
        self.axes.add_artist(at)

    def make_resid_cmap(self, kwargs):

        vals = self.get_array_from_raster(kwargs['file'])
        vals_min = np.amin(vals)
        vals_max = np.amax(vals)
        diff = vals_max - vals_min
        zero_ratio = abs(vals_min) / diff

        cdict = {'red': ((0.0, 1.0, 0.0),
                         (zero_ratio, 0.53, 1.0),
                         (1.0, 1.0, 1.0)),
                 'green': ((0.0, 1.0, 0.0),
                           (zero_ratio, 1, 1.0),
                           (1.0, 0.0, 1.0)),
                 'blue': ((0.0, 0.0, 1.0),
                          (zero_ratio, 1, 0.0),
                          (1.0, 0.0, 1.0))}
        
        return matplotlib.colors.LinearSegmentedColormap('my_colormap',cdict,256)

        
    def on_draw(self):
        """ Redraws the figure
        """
        #clear map plot
        self.fig.clear()
        self.add_axis()
#        self.axes.cla()
#        self.legend_axes.cla()
#        self.legend_fig.clear()
        
        displayed_keys = [key for key in self.all_layers.keys() if self.all_layers[key]['displayed']]
        displayed_keys = sorted(displayed_keys, key=lambda disp_key: self.all_layers[disp_key]['displayorder'])
        #loop through all_layers and display the enabled layers
        #only displayed layers sorted by display order
        title = self.inputs["model_tag"] + "\n"
        for k in displayed_keys:
            v = self.all_layers[k]
            if v['enabled']:
                if v['type'] == 'Vector':
                    self.add_vector(v)
                else:
                    self.add_raster(v)
                    title += self.all_layers[k]['title']
                #if raster then clear and display the color ramp.
               
               
        if self.displayTL:
            self.add_title(title)
         
    def add_vector(self, kwargs):
        self.axes.scatter(kwargs['x'], kwargs['y'], s=10, c=kwargs['color'], linewidth=0.5, antialiased=True)
    
    def add_raster(self, kwargs):
        rasterfile = kwargs['file']
        raster_array = self.get_array_from_raster(rasterfile)
        rasterparams = self.getRasterParams(rasterfile)
        rmin = np.amin(raster_array)
        rmax = np.amax(raster_array)
        norm = colors.normalize(rmin, rmax)
        map_extent = [rasterparams["ulx"],rasterparams["lrx"],rasterparams["lry"],rasterparams["uly"]]
        raster_plot = self.axes.imshow(raster_array,interpolation="nearest", cmap=kwargs['cmap'], norm=norm, origin='upper', extent=map_extent)
        
        if self.displayTL:
            cb = self.fig.colorbar(raster_plot, orientation='horizontal', pad=0.01, fraction=.1, shrink=.9)
            for t in cb.ax.get_xticklabels():
                t.set_fontsize(7)
        

    def get_array_from_raster(self, raster_file):
        '''return a numpy array with the values from the raster_file
        if there are more than 10,000 rows or cols the data will be 
        subsampled and self.map_ratio will be set.
        All nodata values will be removed f
        '''
        ds = gdal.Open(raster_file, gdal.GA_ReadOnly)
        rasterparams = self.getRasterParams(raster_file)
        factor = 1
        nrows = rasterparams["height"] / factor
        ncols = rasterparams["width"] / factor
        ary = ds.GetRasterBand(1).ReadAsArray(buf_ysize=nrows, buf_xsize=ncols)
        ndval = ds.GetRasterBand(1).GetNoDataValue()
        ndval = -3.39999995214e+038
        return np.ma.masked_array(ary, mask=(ary==ndval))
        
        
    def getRasterParams(self, rasterFile):
        """
        Extracts a series of bits of information from a passed raster
        All values are stored in a dictionary which is returned.
        If errors are encountered along the way the error messages will
        be returned as a list in the Error element.
        """
        try:
            #initialize our params dictionary to have None for all parma
            params = {}
            allRasterParams = ["Error", "xScale", "yScale", "width", "height",
                            "ulx", "uly", "lrx", "lry", "Wkt", 
                            "tUlx", "tUly", "tLrx", "tLry", 
                            "srs", "gt", "prj", "NoData", "PixelType"]
            
            for param in allRasterParams:
                params[param] = None
            params["Error"] = []
            
            # Get the PARC parameters from the rasterFile.
            dataset = gdal.Open(rasterFile, gdalconst.GA_ReadOnly)
            if dataset is None:
                params["Error"].append("Unable to open file")
                #print "Unable to open " + rasterFile
                #raise Exception, "Unable to open specifed file " + rasterFile
                
            
            xform  = dataset.GetGeoTransform()
            params["xScale"] = xform[1]
            params["yScale"] = xform[5]
    
            params["width"]  = dataset.RasterXSize
            params["height"] = dataset.RasterYSize
    
            params["ulx"] = xform[0]
            params["uly"] = xform[3]
            params["lrx"] = params["ulx"] + params["width"]  * params["xScale"]
            params["lry"] = params["uly"] + params["height"] * params["yScale"]
                
            
        except:
            #print "We ran into problems extracting raster parameters from " + rasterFile
            params["Error"].append("Some untrapped error was encountered")
        finally:
            del dataset
            return params
        
    def dumpToFile(self, filename):
        pass

    def saveToPDF(self, filename):
        pass



class MyDiagram(FigureCanvas):
    def __init__(self, fig):
        FigureCanvas.__init__(self, fig)
        
    def resizeEvent(self, event):
        if not event.size().height() == 0:
            FigureCanvas.resizeEvent(self, event)

    def leaveEvent(self, event):
        FigureCanvas.leaveEvent(self, event)
        QtGui.QApplication.restoreOverrideCursor()
#        self.emit(QtCore.SIGNAL('axes_leave_event'), event)

class viewTitleLegend(QtGui.QAction):
    def __init__(self, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "Images", "titlelegend.png"))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               "Show Title and Legend",
                               parent)
        self.setCheckable(True)
        self.setChecked(True)
        
    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()
        cellWidget.displayTL = self.isChecked()
        cellWidget.on_draw()
        cellWidget.fig.canvas.draw()
        cellWidget.update()
            

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
        #set all to not displayed
        for k, v in cellWidget.all_layers.items():
            v['displayed'] = False
        #turn on the selected layers
        for action in self.toolBar.actions():
            try:
                if action.isChecked() and cellWidget.all_layers.has_key(action.tag):
                    cellWidget.all_layers[action.tag]['displayed'] = True
            except AttributeError:
                pass #ignore buttons that don't have a tag set
        cellWidget.on_draw()
        cellWidget.fig.canvas.draw()
        cellWidget.update()
#        all_layers = cellWidget.all_layers
#        layerset = []
#        for action in self.toolBar.actions():
#            if action.isChecked() and all_layers.has_key(action.tag):
#                layer = all_layers[action.tag]
#                layerset.append(qgis.gui.QgsMapCanvasLayer(layer))
#
#        cellWidget.map_canvas.setLayerSet(layerset)
##        cellWidget.canvas.repaint()
#        cellWidget.update()

class SAHMSpatialViewerToolBar(QCellToolBar):
    """
    The toolbar that allows users to toggle layers on and off
    in the widget

    """
    def createToolBar(self):
        """ createToolBar() -> None
        This will get call initially to add customizable widgets

        """
        QCellToolBar.updateToolBar(self)
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
                   {"tag":"resid_map", "icon":"ResMap.png",
                     "checked":False, "label":"Display residuals map",
                     "group":"Grids"},
                   {"tag":"mess_map", "icon":"MesMap.png",
                     "checked":False, "label":"Display Multivariate Environmental Similarity Surface (Mess) map",
                     "group":"Grids"},
                    {"tag":"MoD_map", "icon":"ModMap.png",
                     "checked":False, "label":"Display Most Dissimilar Variable (MoD) map",
                     "group":"Grids"}]
        
        lyrs_label = QtGui.QLabel()
        lyrs_label.setText("Layers:")
        self.appendWidget(lyrs_label)

        for action_dict in actions:
            self.appendAction(ViewLayerAction(action_dict, self))
        
        nav_label = QtGui.QLabel()
        nav_label.setText("  Navigation:")
        self.appendWidget(nav_label)
        self.appendAction(viewTitleLegend(self))
#        self.appendWidget(sw.mpl_toolbar)
        
    def updateToolBar(self):
        QCellToolBar.updateToolBar(self)
        sw = self.getSnappedWidget()
                
        for action in self.actions():
            if type(action) == ViewLayerAction:
                #disenable all action refering to data we don't have
                action.setEnabled(sw.all_layers[action.tag]['enabled'])
        
        self.appendWidget(sw.mpl_toolbar)
        
#        for action in self.actions():
#            if type(action) == ViewLayerAction:
#                #disenable all action refering to data we don't have
#                action.setEnabled(action.tag in sw.all_layers)

class AnchoredText(AnchoredOffsetbox):
    def __init__(self, s, loc, pad=0.4, borderpad=0.5, prop=None, frameon=True):

        self.txt = TextArea(s,
                            minimumdescent=False)


        super(AnchoredText, self).__init__(loc, pad=pad, borderpad=borderpad,
                                           child=self.txt,
                                           prop=prop,
                                           frameon=frameon)

