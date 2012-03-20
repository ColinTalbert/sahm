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
from utils import dbfreader

import matplotlib
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QTAgg as NavigationToolbar
from matplotlib.figure import Figure
from matplotlib.offsetbox import AnchoredOffsetbox, TextArea
import matplotlib.colors as colors

import numpy as np
import gc

from osgeo import gdal, gdalconst


class SAHMSpatialOutputViewerCell(SpreadsheetCell):
    """
    SAHMModelOutputViewerCell is a VisTrails Module that
    displays the various output from a SAHM Model run in a single cell

    """
    _input_ports = [("row", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ("column", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ('model_workspace', '(edu.utah.sci.vistrails.basic:File)'),
                    ("max_cells_dimension", "(edu.utah.sci.vistrails.basic:Integer)", {'defaults':str(['5000']), 'optional':True})]
    #all inputs are determined relative to the model_workspace

    def __init__(self):
        SpreadsheetCell.__init__(self)

    def compute(self):
        
        model_workspace = self.forceGetInputFromPort('model_workspace').name
        
        if self.hasInputFromPort("row"):
            if not self.location:
                self.location = CellLocation()
            self.location.row = self.getInputFromPort('row') - 1
        
        if self.hasInputFromPort("column"):
            if not self.location:
                self.location = CellLocation()
            self.location.col = self.getInputFromPort('column') - 1
            
        if self.hasInputFromPort("max_cells_dimension"):
            max_cell_dimension = self.getInputFromPort('max_cells_dimension')
        else:
            max_cell_dimension = [item for item in self._input_ports if item[0] == 'max_cells_dimension'][0][2]['defaults']

        args = [model_workspace, max_cell_dimension]
        self.displayAndWait(SAHMSpatialOutputViewerCellWidget,
                            args)





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
        
        self.setAnimationEnabled(False)

    def updateContents(self, args):
        """ updateContents(inputs: dictionary) -> None
        Update the widget contents based on the input data
        """
        model_workspace, max_cell_dimension = args
        self.toolBarType = SAHMSpatialViewerToolBar
        self.controlBarType = SAHMSpatialViewerToolBar
        self.inputs = self.updateInputs(model_workspace, max_cell_dimension)
        
        self.load_layers()
        self.update_layers()
        self.update()
        
    def updateInputs(self, model_workspace, max_cell_dimension):    
        inputs = {}
        inputs["model_workspace"] = model_workspace
        inputs["model_dir"] = os.path.split(os.path.normcase(inputs["model_workspace"]))[0]

        for model_output in ['prob', 'bin', 'resid', 'mess', 'MoD']:
            try:
                inputs[model_output +"_map"] = os.path.join(inputs["model_dir"],
                                self.find_file(inputs["model_dir"], "_" + model_output + "_map.tif"))
            except:
                inputs[model_output + "_map"] = ""
            
        inputs["mds"] = self.find_mds(inputs["model_dir"])

        inputs["model_tag"] = os.path.split(inputs["model_dir"])[1]




        return inputs
        
    def find_mds(self, model_dir):
        """returns the path to the mds that was used to generate this
        model output.  While the text file that the R model produces
        has an absolute path to the data this function assumes that
        the mds file is in the session folder that this model output
        folder is in.  That is it looks for an mds with the same 
        file name in the parent folder of the model folder.
        """
        model_text = os.path.join(model_dir,
                            self.find_file(model_dir, "_output.txt"))
        #assumed to be one level up from model folder.
        session_folder = os.path.split(model_dir)[0]

        f = open(model_text, 'rb')
        lines = f.read().splitlines()

        # grab the line after "Data:"
        originalMDS = [lines[i + 1] for i in range(len(lines))
                  if lines[i].startswith("Data:")][0].strip()

        fname = os.path.split(originalMDS)[1]
        result = os.path.join(session_folder, fname)
        if os.path.exists(result):
            return result
        else:
            raise RuntimeError('Valid input MDS file not found in Model text output.')
    
    
    def find_file(self, model_dir, suffix):
        try:
            return [file_name for file_name in os.listdir(model_dir)
                                     if file_name.endswith(suffix)][0]
        except IndexError:
            raise RuntimeError('The expected model output ' 
                               + suffix + ' was not found in the model output directory')
    
    def create_main_frame(self):
        ''' Set up our matplot lib figure
        self.ax1 is the chart that contains our GIS Data'''
        self.dpi = 100
        self.fig = Figure((5.0, 4.0), dpi=self.dpi)
        
#        self.fig.subplots_adjust(left = 0.01, right=0.99, top=0.99, bottom=0.001)
        self.fig.subplots_adjust(left = 0, right=1, top=1, bottom=0)
        self.map_canvas = MyMapCanvas(self.fig)
        self.map_canvas.mpl_connect('scroll_event', self.wheel_zoom)
        
        self.ax1 = self.fig.add_subplot(111, aspect='equal', adjustable='datalim')
        self.ax1.spines['right'].set_color('none')
        self.ax1.spines['top'].set_color('none')
        self.ax1.spines['bottom'].set_color('none')
        self.ax1.spines['left'].set_color('none')
        self.ax1.get_xaxis().set_visible(False)
        self.ax1.get_yaxis().set_visible(False)
           

           
        self.mpl_toolbar = NavigationToolbar(self.map_canvas, None)
        #Strip out the unused actions
        keep_actions = ['Pan', 'Zoom', 'Save']
        for action in self.mpl_toolbar.actions():
            if not action.text() in keep_actions and action.text():
                self.mpl_toolbar.removeAction(action)
            if action.text() == 'Zoom':
                icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "Images", "zoom.png"))
                action.setIcon(QtGui.QIcon(icon))

        self.layout().addWidget(self.map_canvas)


          
    def wheel_zoom(self, event):
        #zoom in or out centered on the current cursor position

        inv = self.ax1.transData.inverted()
        curX, curY = inv.transform((event.x, event.y))

        curL, curR = self.ax1.get_xlim()
        curB, curT = self.ax1.get_ylim()
        width = curR - curL
        height = curT - curB
        steps = -1 * event.step / 0.25
        factor = steps / 35
        #sanity check
        if factor > 0.5:
            factor = 0.5
        if factor < -0.5:
            factor = -0.5

        newWidth = width * (1.0 - factor)
        newHeight = height * (1.0 - factor)
        dWidth = width - newWidth
        dHeight = height -newHeight
        
        pcntLofX =  1 - (width - (curX - curL)) / width
        pcntUnderTop = (height - (curT - curY)) / height
        
        newL = curL + (dWidth * pcntLofX)
        newR = curR - (dWidth*(1-pcntLofX))
        newB = curB + (dHeight * pcntUnderTop)
        newT = curT - (dWidth * (1 - pcntUnderTop))
        self.ax1.set_xlim((newL, newR))
        self.ax1.set_ylim((newB, newT))
        
#        self.on_draw()
        
        self.map_canvas.draw()
    
    def deleteLater(self):
        """ deleteLater() -> None        
        Overriding PyQt deleteLater to free up resources
        
        """
        self.fig.clf()
        self.map_canvas.close()
        gc.collect()
        
        QCellWidget.deleteLater(self)
    
    def load_layers(self):
        '''Sets up a dictionary with entries for each of our layers which contains info like the file location,
        the colormap to use for display, and the order to display.
        '''
        self.displayTL = True
        self.all_layers = {"prob_map":{"type":"raster", "title":"Probability" ,"categorical":False, "min":0, "max":1, 'cmap':matplotlib.cm.jet, "displayorder":9999, "displayed":True, "enabled":False, "file":""},
                         "bin_map":{"type":"raster", "title":"Binary probability" , "categorical":False, "categories":[0,1], 'cmap':matplotlib.cm.Greys, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "resid_map":{"type":"raster", "title":"Residuals" , "categorical":False, "min":0, "max":"pullfromdata", 'cmap':matplotlib.cm.Accent, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "mess_map":{"type":"raster", "title":"Mess" , "categorical":False, "categories":"pullfromdata", 'cmap':matplotlib.cm.jet, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "MoD_map":{"type":"raster", "title":"MoD" , "categorical":True, "min":0, "max":"pullfromdata", 'cmap':matplotlib.cm.prism, "displayorder":9999, "num_breaks":7, "displayed":False, "enabled":False, "file":""},
                         "pres_points":{"type":"Vector", "color":(1,0,0), "displayorder":3, "num_breaks":7, "displayed":True, "enabled":False, "file":""},
                         "abs_points":{"type":"Vector", "color":(0,1,0), "displayorder":2, "num_breaks":7, "displayed":True, "enabled":False, "file":""},
                         "backs_points":{"type":"Vector", "color":(0,0,0), "displayorder":1, "num_breaks":7, "displayed":False, "enabled":False, "file":""}}
        
        #assign the file and enable all provided inputs
        for k,v in self.all_layers.items():
            if k in self.inputs.keys():
                if os.path.exists(self.inputs[k]):
                    self.all_layers[k]["file"] = self.inputs[k]
                    self.all_layers[k]["enabled"] = True
                 
        #make our specialty colormaps
        if self.all_layers["resid_map"]['enabled']:
            self.all_layers["resid_map"]["cmap"] = self.make_resid_cmap(self.all_layers["resid_map"])
        
        if self.all_layers["MoD_map"]['enabled']:
            self.all_layers["MoD_map"]["cmap"] = self.make_categorical_cmap(self.all_layers["MoD_map"])       
                 
        pointfile = self.inputs["mds"]
        points = np.genfromtxt(pointfile, delimiter=",", skip_header=3)   
               
                    
        for name, val in {"abs_points":0, "pres_points":1, "backs_points":-9999}.items():
            #parse out the x, y s for the points in each of our categorys
            self.all_layers[name]['x'] = np.delete(points, np.argwhere(points[:,2]<>val), 0)[:,0]
            self.all_layers[name]['y'] = np.delete(points, np.argwhere(points[:,2]<>val), 0)[:,1]
            if len(self.all_layers[name]['x']) == 0:
                self.all_layers[name]["enabled"] = False
            else:
                self.all_layers[name]["enabled"] = True
        
        del points
        
    def update_layers(self):
        """
        """
        #clear map plot
#        self.fig.clear()
#        self.add_axis()
#        self.ax1.clf()
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
#                    self.rasterlayer.rasterfile = v['file']
#                    self.rasterlayer.switch_raster(v['file'])
#                    self.rasterlayer.kwargs = v
                    title += self.all_layers[k]['title']
         
               
        if self.displayTL:
            self.add_title(title)
            self.add_colorbar()
        
        
    def add_title(self, title):
        ''' Adds a title label element to the matplotlib chart'''
        at = AnchoredText(title,
                          loc=2, frameon=True, pad=.05, borderpad=0.2)
        at.patch.set_boxstyle("round,rounding_size=0.2")
        at.set_alpha(0.1)
        self.ax1.add_artist(at)

    def make_resid_cmap(self, kwargs):
        '''    returns the special colormap for a residual surface'''
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
        
        del vals
        return matplotlib.colors.LinearSegmentedColormap('my_colormap',cdict,256)

    def make_categorical_cmap(self, kwargs):
        '''    returns the special colormap for a residual surface'''
        vals = self.get_array_from_raster(kwargs['file'])
        uniques = np.unique(vals)
        vatdbf = kwargs['file'] + ".vat.dbf"
        if os.path.exists(vatdbf):
            #we'll pull labels from this file
            f = open(vatdbf, 'rb')
            db = list(dbfreader(f))
            f.close()
            labels = []
            for record in db[2:]:
                labels.append(record[1])

        kwargs['cbar_ticks'] = uniques
        kwargs['cbar_labels'] = labels
        return matplotlib.cm.get_cmap('Accent', len(uniques))
        
 
    def add_vector(self, kwargs):
        self.test = self.ax1.scatter(kwargs['x'], kwargs['y'], s=10, c=kwargs['color'], linewidth=0.5, antialiased=True)
        del self.test
    
    def add_colorbar(self):
        if self.rasterlayer.kwargs['categorical']:
            cb = self.fig.colorbar(raster_plot, ticks=self.rasterlayer.kwargs['cbar_ticks'], orientation='vertical', pad=0.01, shrink=.9, fraction=.3, aspect=15)
            cb.ax.set_yticklabels(kwargs['cbar_labels'])
        else:
            cb = self.fig.colorbar(raster_plot, orientation='horizontal', pad=0.01, fraction=.1, shrink=.9, aspect=30)
            
        for t in cb.ax.get_xticklabels():
            if self.rasterlayer.kwargs['categorical']:
                t.set_fontsize(5)
                t.set_rotation(90)
            else:
                t.set_fontsize(7)
    
    def add_raster(self, kwargs):
        rasterfile = kwargs['file']
        self.rasterlayer = RasterDisplay()
        self.ax1.callbacks.connect('xlim_changed', self.rasterlayer.ax_update)
        self.ax1.callbacks.connect('ylim_changed', self.rasterlayer.ax_update)
        self.rasterlayer.switch_raster(rasterfile)
        self.rasterlayer.kwargs = kwargs
        
        
        raster_array = self.get_array_from_raster(rasterfile)
        rasterparams = self.getRasterParams(rasterfile)
        rmin = np.amin(raster_array)
        rmax = np.amax(raster_array)
        norm = colors.normalize(rmin, rmax)
#        cur_extent = self.ax1.viewLim.extents
#        cur_extent = [cur_extent[0], cur_extent[2], cur_extent[1], cur_extent[3]]
        map_extent = [rasterparams["ulx"],rasterparams["lrx"],rasterparams["lry"],rasterparams["uly"]]

        #Get the range for the new area
        xstart,ystart,xdelta,ydelta = self.ax1.viewLim.bounds
        xend = xstart + xdelta
        yend = ystart + ydelta
        cur_extent = [xstart, xend, ystart, yend]

        raster_array = self.rasterlayer(rasterparams["ulx"], rasterparams["lrx"], rasterparams["lry"], rasterparams["uly"])

        self.raster_plot = self.ax1.imshow(raster_array, interpolation="nearest", cmap=kwargs['cmap'], norm=norm, origin='upper', extent=map_extent)
        self.rasterlayer.ax_update(self.ax1)
       
        

    def get_array_from_raster(self, raster_file):
        '''return a numpy array with the values from the raster_file
        if there are more than 10,000 rows or cols the data will be 
        subsampled and self.map_ratio will be set.
        All nodata values will be removed
        '''
        ds = gdal.Open(raster_file, gdal.GA_ReadOnly)
        rasterparams = self.getRasterParams(raster_file)
        nrows = rasterparams["height"]
        ncols = rasterparams["width"]
        
        if nrows > 500:
            nrows = 500
        if ncols > 500:
            ncols = 500
        
#        max_dimension = max([nrows, ncols])
#        extent=self.map_canvas.figure.get_window_extent().transformed(self.map_canvas.figure.dpi_scale_trans.inverted())
#        max_pixel_dimension = max(extent.max) * 100
##        max_dimension = self.fig.
##        if max_dimension > self.inputs["max_cells_dimension"]:
#        ratio = max_pixel_dimension / max_dimension
#        nrows = int(ratio * nrows)
#        ncols = int(ratio * ncols)
                
        try:
            ary = ds.GetRasterBand(1).ReadAsArray(buf_ysize=nrows, buf_xsize=ncols)
            ndval = ds.GetRasterBand(1).GetNoDataValue()
        except MemoryError:
            msgbox = QtGui.QMessageBox(self)
            msgbox.setText("This viewer cannot handle datasets this large.\nTry setting the max_cells_dimension to a smaller value.")
            msgbox.exec_()
            raise MemoryError
            
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


class MyMapCanvas(FigureCanvas):
    def __init__(self, fig):
        FigureCanvas.__init__(self, fig)
        self._cursor = None

    def resizeEvent(self, event):
        if not event.size().height() == 0:
            FigureCanvas.resizeEvent(self, event)

    def enterEvent(self, event):
        if (self._cursor is not None and             
            QtGui.QApplication.overrideCursor() is None):             
            QtGui.QApplication.setOverrideCursor(self._cursor)         
        FigureCanvas.enterEvent(self, event) 

    def leaveEvent(self, event):
        self._cursor = QtGui.QCursor(QtGui.QApplication.overrideCursor())         
        QtGui.QApplication.restoreOverrideCursor()         
        FigureCanvas.leaveEvent(self, event)
        
#    def scrollEvent(self, event):
#        print event
#        
#    def scroll_event(self, x, y, steps):
#        print steps
#
#    def wheelEvent(self, event):
#        x, y = event.x(), event.y()
#        
#        print event.x()
#        print "y", event.y()
#        print "delta", event.delta()
#        self.set


class RasterDisplay(object):
    def __init__(self, width=300, height=300):
        self.height = height
        self.width = width
        
    def switch_raster(self, rasterfile):
        self.rasterfile = rasterfile
        self.rasterparams = self.getRasterParams(rasterfile)

    def __call__(self, xstart, xend, ystart, yend):
        self.x = np.linspace(xstart, xend, self.width)
        self.y = np.linspace(ystart, yend, self.height).reshape(-1,1)

        #pull the pixels we need, no more
        ds = gdal.Open(self.rasterfile, gdal.GA_ReadOnly)

        ncols = int((xend - xstart) / self.rasterparams["xScale"])
        nrows = int((yend - ystart) / abs(self.rasterparams["yScale"]))
        
            
        xOffset = int((xstart - self.rasterparams["ulx"]) / self.rasterparams["xScale"])
        yOffset = int((yend - self.rasterparams["uly"]) / self.rasterparams["yScale"])   
                
        try:
            ary = ds.GetRasterBand(1).ReadAsArray(xoff=xOffset, yoff=yOffset, 
                                                  win_xsize=ncols, win_ysize=nrows, 
                                                  buf_ysize=self.height, buf_xsize=self.width)
            ndval = ds.GetRasterBand(1).GetNoDataValue()
        except MemoryError:
            msgbox = QtGui.QMessageBox(self)
            msgbox.setText("This viewer cannot handle datasets this large.\nTry setting the max_cells_dimension to a smaller value.")
            msgbox.exec_()
            raise MemoryError
            
        return np.ma.masked_array(ary, mask=(ary==ndval))   

    def ax_update(self, ax):
        ax.set_autoscale_on(False) # Otherwise, infinite loop

        #Get the number of points from the number of pixels in the window
        dims = ax.axesPatch.get_window_extent().bounds
        self.width = int(dims[2] + 0.5)
        self.height = int(dims[2] + 0.5)

        #Get the range for the new area
        xstart,ystart,xdelta,ydelta = ax.viewLim.bounds
        xend = xstart + xdelta
        yend = ystart + ydelta

        # Update the image object with our new data and extent
        im = ax.images[-1]
        im.set_data(self.__call__(xstart, xend, ystart, yend))
        im.set_extent((xstart, xend, ystart, yend))
        ax.figure.canvas.draw_idle()

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


class fullExtent(QtGui.QAction):
    def __init__(self, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "Images", "world.png"))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               "Full Extent",
                               parent)
        self.setCheckable(False)

    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()
        xlim = cellWidget.xlim
        ylim = cellWidget.ylim
        cellWidget.axes.set_xlim(xlim)
        cellWidget.axes.set_ylim(ylim)
        cellWidget.fig.canvas.draw()
        cellWidget.update()

class viewTitleLegend(QtGui.QAction):
    def __init__(self, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "Images", "titlelegend.png"))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               "Show/Hide Title and Legend",
                               parent)
        self.setCheckable(True)
        self.setChecked(True)
        
    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()
        
#        xlim = cellWidget.axes.get_xlim()
#        ylim = cellWidget.axes.get_ylim()
        cellWidget.displayTL = self.isChecked()
#        cellWidget.on_draw()
        cellWidget.fig.canvas.draw()
        cellWidget.update()
#        cellWidget.axes.set_xlim(xlim)
#        cellWidget.axes.set_ylim(ylim)            

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
        cellWidget = self.toolBar.getSnappedWidget()
        xlim = cellWidget.ax1.get_xlim()
        ylim = cellWidget.ax1.get_ylim()
        self.toggleOthers()
        self.displayLayer()
        self.toolBar.updateToolBar()
        cellWidget.axes.set_xlim(xlim)
        cellWidget.axes.set_ylim(ylim)

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
        cellWidget.update_layers()
        try:
            cellWidget.fig.canvas.draw()
        except MemoryError:
            msgbox = QtGui.QMessageBox(self)
            msgbox.setText("This viewer cannot handle datasets this large.\nTry setting the max_cells_dimension to a smaller value.")
            msgbox.exec_()
            raise MemoryError
            
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
        self.appendAction(fullExtent(self))
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

