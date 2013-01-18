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

################################################################################
# ImageViewer widgets/toolbar implementation
################################################################################
import os
import csv
import gc
import itertools

import utils
import math

from PyQt4 import QtCore, QtGui
from core.system import systemType
if systemType in ['Microsoft', 'Windows']:
    from PyQt4 import QAxContainer
from core.modules.vistrails_module import Module
from packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
from packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
from packages.spreadsheet.spreadsheet_controller import spreadsheetController

from sahm_picklists import OutputRaster
from utils import map_ports

from utils import dbfreader, getRasterParams
from utils import print_timing

import matplotlib
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QTAgg as NavigationToolbar
#from matplotlib.backends.backend_qt4 import FigureCanvasQT as FigureCanvas
#from matplotlib.backends.backend_qt4 import NavigationToolbar2QT as NavigationToolbar
from matplotlib.figure import Figure
from matplotlib.offsetbox import AnchoredOffsetbox, TextArea
import matplotlib.colors as colors

import numpy as np

from osgeo import gdal, gdalconst

from core.packagemanager import get_package_manager

import utils

class SAHMSpatialOutputViewerCell(SpreadsheetCell):
    """
    SAHMModelOutputViewerCell is a VisTrails Module that
    displays the various output from a SAHM Model run in a single cell

    """
    _input_ports = [("row", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ("column", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ('display_presense_points', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'[False]', 'optional':False}),
                    ('display_absense_points', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'[False]', 'optional':False}),
                    ('display_background_points', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'[False]', 'optional':False}),
                    ('initial_raster_display', '(gov.usgs.sahm:OutputRaster:Other)', {'defaults':'["Probability"]'}),
                    ('model_workspace', '(edu.utah.sci.vistrails.basic:Directory)')]
    #all inputs are determined relative to the model_workspace

    def __init__(self):
        SpreadsheetCell.__init__(self)

        self.port_map = {'display_presense_points': ("display_pres_points", None, True),
            'display_absense_points': ("display_abs_points", None, True),
            'display_background_points': ("display_backs_points", None, True),
            'initial_raster_display': ("initial_raster", None, True),
            "model_workspace": ("model_workspace", None, True)}

    @print_timing
    def compute(self):
        inputs = {}

        inputs = map_ports(self, self.port_map)

#        inputs["model_workspace"] = self.forceGetInputFromPort('model_workspace').name
#
#        for port in ['display_presense_points', 'display_absense_points', 'display_background_points', 'initial-raster_display']:
#            inputs[port] = self.forceGetInputFromPort(port)

#        pm = get_package_manager()
#        hasVisWall = pm.has_package('gov.usgs.VisWallServer')
#        if hasVisWall:
#            row = self.forceGetInputFromPort("row", -1)
#            col = self.forceGetInputFromPort("column", -1)
#            from packages.VisWallServer import display_sahm_output
#            display_sahm_output(row, col, {"model_workspace": inputs["model_workspace"]}, 'SpatialOutput')
#            return

        inputs["model_dir"] = os.path.normcase(inputs["model_workspace"])

        for model_output in ['prob', 'bin', 'resid', 'mess', 'MoD']:
            try:
                inputs[model_output +"_map"] = os.path.join(inputs["model_dir"],
                                utils.find_file(inputs["model_dir"], "_" + model_output + "_map.tif"))
            except:
                inputs[model_output + "_map"] = ""

        try:
            inputs["mds"] = self.find_mds(inputs["model_dir"])
        except RuntimeError:
            inputs["mds"] = ""

        inputs["model_tag"] = os.path.split(inputs["model_dir"])[1]

        if self.hasInputFromPort("row"):
            if not self.location:
                self.location = CellLocation()
            self.location.row = self.getInputFromPort('row') - 1

        if self.hasInputFromPort("column"):
            if not self.location:
                self.location = CellLocation()
            self.location.col = self.getInputFromPort('column') - 1

        if self.inputPorts.has_key('Location'):
            self.location =  self.inputPorts['Location'][0].obj

#        if self.hasInputFromPort("max_cells_dimension"):
#            inputs["max_cells_dimension"] = self.getInputFromPort('max_cells_dimension')
#        else:
#            inputs["max_cells_dimension"] = [item for item in self._input_ports if item[0] == 'max_cells_dimension'][0][2]['defaults']

        if utils.checkIfModelFinished(inputs["model_dir"]):
            self.local_displayAndWait(inputs)

    @print_timing
    def local_displayAndWait(self, inputs):
        self.displayAndWait(SAHMSpatialOutputViewerCellWidget,
                            inputs)       
    


    def find_mds(self, model_dir):
        """returns the path to the mds that was used to generate this
        model output.  While the text file that the R model produces
        has an absolute path to the data this function assumes that
        the mds file is in the session folder that this model output
        folder is in.  That is it looks for an mds with the same
        file name in the parent folder of the model folder.
        """
        model_text = os.path.join(model_dir,
                            utils.find_file(model_dir, "_output.txt"))
        #assumed to be one level up from model folder.
        session_folder = os.path.split(model_dir)[0]

        f = open(model_text, 'rb')
        lines = f.read().splitlines()

        # grab the line after "Data:"
        try:
            originalMDS = [lines[i + 1] for i in range(len(lines))
                  if lines[i].startswith("Data:")][0].strip()
                

            fname = os.path.split(originalMDS)[1]
            result = os.path.join(session_folder, fname)
            if os.path.exists(result):
                return result
            elif os.path.exists(originalMDS):
                return originalMDS
            else:
                raise RuntimeError('Valid input MDS file not found in Model text output.')
        except IndexError:
            raise RuntimeError('Valid input MDS file not found in Model text output.')


class SAHMSpatialOutputViewerCellWidget(QCellWidget):
    """

    """
    def __init__(self, parent=None):
        """ QGISCellWidget(parent: QWidget) -> QGISCellWidget
        Initialize the widget with its central layout

        """
        QCellWidget.__init__(self, parent)

        self.displaylegend = True

        centralLayout = QtGui.QVBoxLayout()
        self.setLayout(centralLayout)
        centralLayout.setMargin(0)
        centralLayout.setSpacing(0)
        self.create_main_frame()
        self.fig.canvas.draw()
        self.sync_changes = "all"
        self.setAnimationEnabled(False)
        self.displayTL = True
        self.pointsLoaded = False

    @print_timing
    def updateContents(self, inputs):
        """ updateContents(inputs: dictionary) -> None
        Update the widget contents based on the input data
        """
#        print "SAHMSpatialOutputViewerCellWidget updateContents"
        self.toolBarType = SAHMSpatialViewerToolBar
        self.controlBarType = SAHMSpatialViewerToolBar
        self.inputs = inputs

        self.load_layers()
        self.on_draw(UseMaxExt=True)

        self.maxXlim, self.maxYlim = self.getMaxDisplayExtent()
    
    
        self.axes.set_ylim(self.maxYlim, emit=False)
        self.axes.set_xlim(self.maxXlim, emit=False)
        self.fig.canvas.draw()
        self.update()

    @print_timing
    def create_main_frame(self):
        self.setFocusPolicy(QtCore.Qt.ClickFocus)
        self.dpi = 100
        self.fig = Figure((5.0, 4.0), dpi=self.dpi)

#        self.fig.subplots_adjust(left = 0.01, right=0.99, top=0.99, bottom=0.001)
        self.fig.subplots_adjust(left = 0, right=1, top=1, bottom=0)
        self.map_canvas = MyMapCanvas(self.fig)
        self.map_canvas.mpl_connect('scroll_event', self.wheel_zoom)
#        self.connect(self, QtCore.SIGNAL('keyPressEvent(QString)'),
#             self.key_press)
        self.map_canvas.mpl_connect('button_release_event', self.button_up)
        self.map_canvas.mpl_connect('resize_event', self.resize)
        self.add_axis()

        self.mpl_toolbar = NavigationToolbar(self.map_canvas, None)



        self.mpl_toolbar.pan()

#        self.popMenu = popup_menu(self, self.mpl_toolbar)
        self.popMenu = None

        self.map_canvas.setContextMenuPolicy( QtCore.Qt.CustomContextMenu )
        self.connect(self.map_canvas, QtCore.SIGNAL('customContextMenuRequested(const QPoint&)'), self.on_context_menu)

        self.layout().addWidget(self.map_canvas)

    def on_context_menu(self, point):
        if self.popMenu is None:
            self.popMenu = self.createPopupMenu()
        self.popMenu.exec_(self.map_canvas.mapToGlobal(point))

    def createPopupMenu(self):
        sheet = self.findSheetTabWidget()
        toolbar = SAHMSpatialViewerToolBar(sheet)
        row, col = self.findCurrentCell()
        toolbar.snapTo(row, col)
        return toolbar.gen_popup_menu()

    def getRasterParams(self, rasterfile):
        return getRasterParams(rasterfile)

    @print_timing
    def wheel_zoom(self, event):
        #zoom in or out centered on the current cursor position

        inv = self.axes.transData.inverted()
        curX, curY = inv.transform((event.x, event.y))

        curL, curR = self.axes.get_xlim()
        curB, curT = self.axes.get_ylim()
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
        self.axes.set_xlim((newL, newR))
        self.axes.set_ylim((newB, newT))

        self.sync_extents()


    def button_up(self, event):
        if event.button == 1:
#            self.pull_pixels()
            self.sync_extents()

    @print_timing
    def resize(self):
        self.pull_pixels()

    @print_timing
    def pull_pixels(self):
#        print "SAHMSpatialOutputViewerCellWidget _pull_pixels"
        self.rasterlayer.ax_update(self.axes)

    @print_timing
    def keyPressEvent(self, event):
        if type(event) == QtGui.QKeyEvent and event.key() == QtCore.Qt.Key_T:
            active_cells = self.getSelectedCellWidgets()

            displayed = not self.displayTL
            for cell in active_cells:
                cell.displayTL = displayed
                cell.on_draw()
                cell.fig.canvas.draw()
                cell.update()

    def deleteLater(self):
        """ deleteLater() -> None
        Overriding PyQt deleteLater to free up resources

        """
        self.fig.clf()
        self.map_canvas.close()
        gc.collect()

        QCellWidget.deleteLater(self)

    @print_timing
    def load_layers(self):
#        print "SAHMSpatialOutputViewerCellWidget load_layers"
        self.all_layers = {"prob_map":{"type":"raster", "title":"Probability" ,"categorical":False, "min":0, "max":1, 'cmap':matplotlib.cm.jet, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "bin_map":{"type":"raster", "title":"Binary probability" , "categorical":False, "min":0, "max":1, 'cmap':matplotlib.cm.Greys, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "resid_map":{"type":"raster", "title":"Residuals" , "categorical":False, "min":"pullfromdata", "max":"pullfromdata", 'cmap':matplotlib.cm.Accent, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "mess_map":{"type":"raster", "title":"Mess" , "categorical":False, "min":0, "max":"pullfromdata", "categories":"pullfromdata", 'cmap':matplotlib.cm.jet, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "MoD_map":{"type":"raster", "title":"MoD" , "categorical":True, "min":0, "max":"pullfromdata", 'cmap':matplotlib.cm.prism, "displayorder":9999, "num_breaks":7, "displayed":False, "enabled":False, "file":""},
                         "pres_points":{"type":"Vector", "title":"Presence", "color":(1,0,0), "displayorder":3, "num_breaks":7, "displayed":False, "enabled":True, "file":""},
                         "abs_points":{"type":"Vector", "title":"Absence", "color":(0,1,0), "displayorder":2, "num_breaks":7, "displayed":False, "enabled":True, "file":""},
                         "backs_points":{"type":"Vector", "title":"Background", "color":(0,0,0), "displayorder":1, "num_breaks":7, "displayed":False, "enabled":True, "file":""}}

        #set the inital layers to display passed on passed in inputs
        for layer in ["display_pres_points", "display_abs_points", "display_backs_points"]:
            self.all_layers[layer.replace("display_", "")]["displayed"] = self.inputs[layer]

        initial_map_dict = {'Probability':"prob_map", 'Binary Probability':"bin_map",
                                       'Residuals':"resid_map", 'Mess':"mess_map", 'MoD':"MoD_map"}
        self.all_layers[initial_map_dict[self.inputs['initial_raster']]]["displayed"] = True

        for k,v in self.all_layers.items():
            if k in self.inputs:
                if os.path.exists(self.inputs[k]):
                    self.all_layers[k]["file"] = self.inputs[k]
                    self.all_layers[k]["enabled"] = True

        rasterparams = getRasterParams(self.all_layers[initial_map_dict[self.inputs['initial_raster']]]["file"])
        self.maxExtent = [rasterparams["ulx"],rasterparams["lrx"],rasterparams["lry"],rasterparams["uly"]]

        #make our specialty colormaps
        if self.all_layers["resid_map"]['enabled']:
            self.all_layers["resid_map"]["cmap"] = self.make_resid_cmap(self.all_layers["resid_map"])

        if self.all_layers["MoD_map"]['enabled']:
            self.all_layers["MoD_map"]["cmap"] = self.make_categorical_cmap(self.all_layers["MoD_map"])



#    def set_maxExtent(self, width=1, height=1, portionRow=1, portionCol=1):
#        rasterparams = getRasterParams(self.all_layers[self.initial_map_dict[self.inputs['initial_raster']]]["file"])
#        cellWidth = (rasterparams["lrx"] - rasterparams["ulx"]) / width
#        cellHeight = (rasterparams["uly"] - rasterparams["lry"]) / height 
#                
#        ulx = rasterparams["ulx"] + ((portionCol) * cellWidth)
#        lrx = rasterparams["ulx"] + ((portionCol + 1) * cellWidth)
#        uly = rasterparams["uly"] - ((portionRow) * cellHeight)
#        lry = rasterparams["uly"] - ((portionRow + 1) * cellHeight)
#        
#        self.maxExtent = [ulx, lrx, lry, uly]

    @print_timing
    def loadPoints(self):
        #initialize our arrays
        for pointType in ['abs_points', 'pres_points', 'backs_points']:
            self.all_layers[pointType]['x'] = []
            self.all_layers[pointType]['y'] = []
        
        mdsReader = csv.reader(open(self.inputs["mds"], 'r'))
        header = mdsReader.next()
        header2 = mdsReader.next()
        header3 = mdsReader.next()
        
        for row in mdsReader:
            if int(row[2]) > 0:
                self.all_layers['pres_points']['x'].append(float(row[0]))
                self.all_layers['pres_points']['y'].append(float(row[1]))
            elif int(row[2]) == 0:
                self.all_layers['abs_points']['x'].append(float(row[0]))
                self.all_layers['abs_points']['y'].append(float(row[1]))
            else:
                self.all_layers['backs_points']['x'].append(float(row[0]))
                self.all_layers['backs_points']['y'].append(float(row[1]))
                
        for pointType in ['abs_points', 'pres_points', 'backs_points']:
            self.all_layers[pointType]["enabled"] = len(self.all_layers[pointType]['x']) > 0

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

#        vals = self.get_array_from_raster(kwargs['file'])
#        vals = self.rasterlayer()
#        vals_min = np.amin(vals)
#        vals_max = np.amax(vals)
        vals_min, vals_max = utils.getrasterminmax(kwargs['file'])
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

    def make_categorical_cmap(self, kwargs):
#        vals = self.get_array_from_raster(kwargs['file'])
#        vals = self.rasterlayer()
#        uniques = np.unique(vals)
        uniques = []
        vatdbf = kwargs['file'] + ".vat.dbf"
        if os.path.exists(vatdbf):
            #we'll pull labels from this file
            f = open(vatdbf, 'rb')
            db = list(dbfreader(f))
            f.close()
            labels = []
            for record in db[2:]:
                uniques.append(record[0])
                labels.append(record[1])

        kwargs['cbar_ticks'] = uniques
        kwargs['cbar_labels'] = labels
        return matplotlib.cm.get_cmap('Accent', len(uniques))
    
    @print_timing
    def on_draw(self, UseMaxExt=False, passedExtent=None):
        """ Completely clears then redraws the figure
        There's probably a more efficient way to do this.
        """
#        print "SAHMSpatialOutputViewerCellWidget _on_draw"
        if passedExtent is not None:
            curExtent = passedExtent
        elif UseMaxExt:
            curExtents = self.getMaxExtent()
        else:
            curExtents = self.get_extent()
            
#        print "curExtents: ", curExtents

        self.fig.clear()
        self.add_axis()

        displayed_keys = [key for key in self.all_layers.keys() if self.all_layers[key]['displayed']]
        displayed_keys = sorted(displayed_keys, key=lambda disp_key: self.all_layers[disp_key]['displayorder'])
        #loop through all_layers and display the enabled layers
        #only displayed layers sorted by display order
        title = self.inputs["model_tag"] + "\n"
        for k in displayed_keys:
            v = self.all_layers[k]
            if v['enabled']:
                if v['type'] == 'Vector':
                    self.add_vector(k)
                else:
                    self.add_raster(v, curExtents)
                    title += self.all_layers[k]['title']

        if self.displayTL:
            self.add_title(title)
    
    @print_timing
    def add_vector(self, layername):
        kwargs = self.all_layers[layername]
        if not self.pointsLoaded or not kwargs.has_key("x"):
            self.loadPoints()
            self.pointsLoaded = True
        
        if self.all_layers[layername]['enabled']:
            self.axes.scatter(kwargs['x'], kwargs['y'], s=10, c=kwargs['color'], linewidth=0.5, antialiased=True)
    
    @print_timing
    def add_raster(self, kwargs, curExtents):
        rasterfile = kwargs['file']

        self.rasterlayer = RasterDisplay()
        self.rasterlayer.setDims(self.axes)
        self.rasterlayer.switch_raster(rasterfile)

#        rasterparams = getRasterParams(rasterfile)
#        map_extent = [rasterparams["ulx"],rasterparams["lrx"],rasterparams["lry"],rasterparams["uly"]]
#
#
##        raster_array = self.get_array_from_raster(rasterfile)
#        curXLim = self.axes.get_xlim()
#        curYLim = self.axes.get_ylim()
#        raster_array = self.rasterlayer(rasterparams["ulx"], rasterparams["lrx"], rasterparams["lry"], rasterparams["uly"])
#        print "\n_addraster"
        raster_array = self.rasterlayer(*curExtents)
#
#        rmin = np.amin(raster_array)
#        rmax = np.amax(raster_array)

        if kwargs['categorical']:
            raster_plot = self.axes.imshow(raster_array, interpolation="nearest", cmap=kwargs['cmap'], origin='upper', extent=self.getDataExtent())
        else:
            rmin = kwargs['min']
            rmax = kwargs['max']
            if kwargs["max"] == "pullfromdata" or \
                kwargs["min"] == "pullfromdata":
                min, max = utils.getrasterminmax(kwargs['file'])
                if kwargs["max"] == "pullfromdata":
                    rmax = max
                if kwargs["min"] == "pullfromdata":
                    rmin = min

            norm = colors.normalize(rmin, rmax)
#            print "raster_array is None:", raster_array is None
#            print "raster_array.size:", raster_array.size
#            print "type(raster_array):", type(raster_array)
#            print raster_array
            if raster_array.size == 1:
                print "raster_array is None!!!/n/n"
                raster_array = np.empty([1960, 1080])
#                print raster_array
            raster_plot = self.axes.imshow(raster_array,interpolation="nearest", cmap=kwargs['cmap'], norm=norm, origin='upper', extent=self.getDataExtent())
            

        if self.displayTL:

            if kwargs['categorical']:
                cb = self.fig.colorbar(raster_plot, ticks=kwargs['cbar_ticks'], orientation='vertical', pad=0.01, shrink=.9, fraction=.3, aspect=15)
                cb.ax.set_yticklabels(kwargs['cbar_labels'])
            else:
                cb = self.fig.colorbar(raster_plot, orientation='horizontal', pad=0.01, fraction=.1, shrink=.9, aspect=30)

            for t in cb.ax.get_xticklabels():
                if kwargs['categorical']:
                    t.set_fontsize(5)
                    t.set_rotation(90)
                else:
                    t.set_fontsize(7)

            

#
#    def get_array_from_raster(self, raster_file):
#        '''return a numpy array with the values from the raster_file
#        if there are more than 10,000 rows or cols the data will be
#        subsampled and self.map_ratio will be set.
#        All nodata values will be removed
#        '''
#        ds = gdal.Open(raster_file, gdal.GA_ReadOnly)
#        rasterparams = getRasterParams(raster_file)
#        nrows = rasterparams["height"]
#        ncols = rasterparams["width"]
#        max_dimension = max([nrows, ncols])
#        if max_dimension > self.inputs["max_cells_dimension"]:
#            ratio = float(self.inputs["max_cells_dimension"]) / max_dimension
#            nrows = int(ratio * nrows)
#            ncols = int(ratio * ncols)
#
#        try:
#            ary = ds.GetRasterBand(1).ReadAsArray(buf_ysize=nrows, buf_xsize=ncols)
#            ndval = ds.GetRasterBand(1).GetNoDataValue()
#        except MemoryError:
#            msgbox = QtGui.QMessageBox(self)
#            msgbox.setText("This viewer cannot handle datasets this large.\nTry setting the max_cells_dimension to a smaller value.")
#            msgbox.exec_()
#            raise MemoryError
#
#        return np.ma.masked_array(ary, mask=(ary==ndval))


    def dumpToFile(self, filename):
        pass

    def saveToPDF(self, filename):
        pass

    def findSheetTabWidget(self):
        """ findSheetTabWidget() -> QTabWidget
        Find and return the sheet tab widget

        """
        p = self.parent()
        while p:
            if hasattr(p, 'isSheetTabWidget'):
                if p.isSheetTabWidget()==True:
                    return p
            p = p.parent()
        return None

    def findCurrentCell(self):
        sheet = self.findSheetTabWidget()
        all_cells = list(itertools.product(range(sheet.getDimension()[0]), range(sheet.getDimension()[1])))
        for row, col in all_cells:
            cell = sheet.getCell(row, col)
            if cell is self:
                return row, col
        return -1, -1

    def getSAHMSpatialsInCellList(self, sheet, cells):
        """  Get the list of SAHM spatial outputviewers
         inside a list of (row, column) cells.
        """
        SAHMspatials = []
        for (row, col) in cells:
            cell = sheet.getCell(row, col)
            if hasattr(cell, 'make_resid_cmap'):
                SAHMspatials.append(cell)
        return SAHMspatials

    def getSelectedCellWidgets(self):
        sheet = self.findSheetTabWidget()
        if sheet:
            selected_cells = sheet.getSelectedLocations()
            return self.getSAHMSpatialsInCellList(sheet, selected_cells)
        return []

    def get_allCellWidgets(self):
        sheet = self.findSheetTabWidget()
        if sheet:
            all_cells = list(itertools.product(range(sheet.getDimension()[0]), range(sheet.getDimension()[1])))
            return self.getSAHMSpatialsInCellList(sheet, all_cells)
        return []

    def get_active_cells(self):
        if self.sync_changes == "all":
            return self.get_allCellWidgets()
        elif self.sync_changes == "sel":
            return self.getSelectedCellWidgets()
        else:
            return [self]

    def getActionByTag(self, tag):
        if self.popMenu is None:
            self.popMenu = self.createPopupMenu()

        for action in self.popMenu.actions():
            try:
                if action.tag == tag:
                    return action
            except AttributeError:
                pass
        return None
    
    #Functions dealing with managing extents
    @print_timing
    def set_extent(self, ylim, xlim):
#        print "_set_extent"
        self.axes.set_ylim(ylim, emit=False)
        self.axes.set_xlim(xlim, emit=False)

        self.pull_pixels()
        self.fig.canvas.draw()
        self.update()
        
    @print_timing        
    def get_extent(self):
#        print "_get_extent"
        return list(self.axes.get_xlim()) + list(self.axes.get_ylim())

    def getMaxExtent(self):
        return self.maxExtent

    def getDataExtent(self):
        return self.maxExtent

    def getMaxDisplayExtent(self):
        return self.axes.get_xlim(), self.axes.get_ylim()
    
    def zoomFull(self):
        self.axes.set_xlim(self.maxXlim)
        self.axes.set_ylim(self.maxYlim)
        self.sync_extents()
        
    def sync_extents(self):
#        print "_sync_extents"
        for spatialViewer in self.get_active_cells():
            spatialViewer.set_extent(self.axes.get_ylim(), self.axes.get_xlim())
            spatialViewer.pull_pixels()
            spatialViewer.map_canvas.draw()


class MyMapCanvas(FigureCanvas):
    def __init__(self, fig):
        FigureCanvas.__init__(self, fig)
        self._cursor = None

    @print_timing
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

class RasterDisplay(object):
    '''The idea behind this is from
    http://matplotlib.sourceforge.net/examples/event_handling/viewlims.py
    basically we want to only query as much data as we have screen pixels for.
    When the user zooms, pans, resizes we'll go back to the original display
    and get another set of pixels.

    This object has a pointer to the original raster and functions
    for switching the input file or getting an array of pixel values
    '''
    def __init__(self, threeBand=False, NoDataValue="ExtractFromFile", width=300, height=300):
        self.height = height
        self.width = width
        self.threeBand = threeBand
        self.NoDataValue = NoDataValue

    @print_timing
    def switch_raster(self, rasterfile):
        self.rasterfile = rasterfile
        self.rasterparams = getRasterParams(rasterfile)

    @print_timing
    def __call__(self, xstart, xend, ystart, yend):
#        print "RasterDisplay __call__"
        self.x = np.linspace(xstart, xend, self.width)
        self.y = np.linspace(ystart, yend, self.height).reshape(-1,1)

         #pull the pixels we need, no more
        ds = gdal.Open(self.rasterfile, gdal.GA_ReadOnly)

        xform  = ds.GetGeoTransform()

        if xstart < xform[0]:
            xstart = xform[0]
        if xend > (xform[0] + (ds.RasterXSize * xform[1])):
            xend = (xform[0] + (ds.RasterXSize * xform[1]))
        if ystart < (xform[3] + (ds.RasterYSize * xform[5])):
            ystart = (xform[3] + (ds.RasterYSize * xform[5]))
        if yend > xform[3]:
            yend = xform[3]
        

        ncols = int((xend - xstart) / xform[1])
        nrows = int((yend - ystart) / abs(xform[5]))

        xOffset = int((xstart - xform[0]) / xform[1])
        yOffset = int((yend- xform[3]) / xform[5])

        if xOffset + ncols > ds.RasterXSize:
            xOffset = max(0, ds.RasterXSize - xOffset)
        if xOffset < 0:
            xOffset = 0

        if yOffset + nrows > ds.RasterYSize:
            yOffset = 0
        if yOffset < 0:
            yOffset = 0

        if ncols + xOffset > ds.RasterXSize:
            ncols = ds.RasterXSize - xOffset
        if ncols < 0:
            ncols = ds.RasterXSize

        if nrows + yOffset > ds.RasterYSize:
            nrows = ds.RasterYSize - yOffset
        if nrows < 0:
            nrows = ds.RasterYSize

#        print "rows, cols:  ", ncols, nrows
#        print "pixelspulled: ", self.height, self.width

        ary = ds.GetRasterBand(1).ReadAsArray(xoff=xOffset, yoff=yOffset,
                                              win_xsize=ncols, win_ysize=nrows,
                                              buf_ysize=self.height, buf_xsize=self.width)
        
        if self.NoDataValue == "ExtractFromFile":
            ndval = utils.getNDVal(self.rasterfile)
        else:
            ndval = float(self.NoDataValue)

        if ary is None or ary.size == 1:
                print "raster_array is None!!!/n/n"
                return np.empty([1960, 1080])
        else:
                return np.ma.masked_array(ary, mask=(ary==ndval))
            
    @print_timing
    def setDims(self, ax):
        #Get the number of points from the number of pixels in the window
        dims = ax.axesPatch.get_window_extent().bounds
#        print "RasterDisplay setDims:   ", dims
        self.width = int(dims[2] + 0.5)
        self.height = int(dims[3] + 0.5)

    @print_timing
    def ax_update(self, ax):
        ax.set_autoscale_on(False) # Otherwise, infinite loop
        self.setDims(ax)
#        print "RasterDisplay ax_update"

        #Get the range for the new area
        xstart,ystart,xdelta,ydelta = ax.axes.viewLim.bounds
        xend = xstart + xdelta
        yend = ystart + ydelta

        factor = 0.1 #we want to pull more pixels than we absolutely need
        #so that we don't get white edges
        xBuff = (xend - xstart) * factor
        yBuff = (yend - ystart) * factor
        xstart = xstart - xBuff
        ystart = ystart - yBuff
        xend = xend + xBuff
        yend = yend + yBuff

#        print "   Before: "
#        print "     xlim: ", int(xstart), int(xend)
#        print "     ylim: ", int(ystart), int(yend)

        #reel these values in if they are outside our bounds
        for bound in ['xstart', 'xend']:
            if eval(bound) < self.rasterparams["ulx"]:
                exec(bound + " = " + str(self.rasterparams["ulx"]))
            if eval(bound) > self.rasterparams["lrx"]:
                exec(bound + " = " + str(self.rasterparams["lrx"]))
            
        for bound in ['ystart', 'yend']:
            if eval(bound) < self.rasterparams["lry"]:
                exec(bound + " = " + str(self.rasterparams["lry"]))
            if eval(bound) > self.rasterparams["uly"]:
                exec(bound + " = " + str(self.rasterparams["uly"]))

        # Update the image object with our new data and extent
        im = ax.images[-1]
#        print "   After: "
#        print "     xlim: ", int(xstart), int(xend)
#        print "     ylim: ", int(ystart), int(yend)
        im.set_data(self.__call__(xstart, xend, ystart, yend))
        im.set_extent((xstart, xend, ystart, yend))
        ax.figure.canvas.draw_idle()

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
        cellWidget.zoomFull()


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

        active_cells = cellWidget.get_active_cells()
        for cell in active_cells:

            xlim = cell.axes.get_xlim()
            ylim = cell.axes.get_ylim()
            cell.displayTL = self.isChecked()
            cell.on_draw()
            cell.fig.canvas.draw()
            cell.update()
            cell.axes.set_xlim(xlim)
            cell.axes.set_ylim(ylim)

class sync_changes(QtGui.QAction):

    def __init__(self, parent=None):
        self.sync_options = itertools.cycle(["all", "sel", "one"])

        QtGui.QAction.__init__(self,
                               self.getIcon(self.sync_options.next()),
                               r"Apply changes to all / selected / single cell",
                               parent)
        self.setCheckable(True)
        self.setChecked(True)


    def getIcon(self, tag):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "Images", tag + ".png"))
        return QtGui.QIcon(icon)

    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()
        next_option = self.sync_options.next()
        self.setIcon(self.getIcon(next_option))
        cellWidget.sync_changes = next_option

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

        self.toggleOthers()
        self.displayLayer()
        self.toolBar.updateToolBar()

    def toggleOthers(self):
        '''Unselect the other raster or vector layers
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
        active_cells = cellWidget.get_active_cells()

        for cell in active_cells:

            #set all to not displayed
            for k, v in cell.all_layers.items():
                v['displayed'] = False
            #turn on the selected layers
            for action in self.toolBar.actions():
                try:
                    if action.isChecked() and cell.all_layers.has_key(action.tag):
                        cell.all_layers[action.tag]['displayed'] = True
                except AttributeError:
                    pass #ignore buttons that don't have a tag set
            cell.on_draw()
            try:
                cell.fig.canvas.draw()
            except MemoryError:
                msgbox = QtGui.QMessageBox(self)
                msgbox.setText("This viewer cannot handle datasets this large.\nTry setting the max_cells_dimension to a smaller value.")
                msgbox.exec_()
                raise MemoryError

            cell.update()

class MPL_action(QtGui.QAction):

    def __init__(self, action_dict, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "Images", action_dict["icon"]))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               action_dict["label"],
                               parent)
        self.setToolTip(action_dict["tooltip"])
        self.setCheckable(action_dict["checkable"])
        self.setChecked(action_dict["checked"])
        self.actionfunc = action_dict["actionfunc"]
        self.tag = action_dict["label"]

    def triggeredSlot(self, checked=False):

        cellWidget = self.toolBar.getSnappedWidget()

        if self.tag in self.tag in ["Pan", "Zoom"]:
            if self.isChecked():
                cursor = self.tag
            elif self.tag == 'Pan':
                cursor = "Zoom"
            elif self.tag == 'Zoom':
                cursor = "Pan"

        active_cells = cellWidget.get_active_cells()
        for cell in active_cells:

            if self.tag in ["Pan", "Zoom"]:
                zoomaction = cell.getActionByTag("Zoom")
                panaction = cell.getActionByTag("Pan")

                if cursor == "Zoom" and \
                    (not zoomaction.isChecked() or cellWidget is cell):
                    cell.mpl_toolbar.zoom()
                elif cursor == "Pan" and \
                    (not panaction.isChecked() or cellWidget is cell):
                    cell.mpl_toolbar.pan()

                zoomaction.setChecked(cursor=="Zoom")
                panaction.setChecked(cursor=="Pan")

            else:
                eval("cell.mpl_toolbar." + self.actionfunc + "()")

    def getAction(self, name):
        for action in self.parent().actions():
            if hasattr(action, "actionfunc") and \
                action.actionfunc == name:
                return action
        return None


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
                     "checked":False, "label":"Display presence points",
                     "group":"pres_points"},
                   {"tag":"abs_points", "icon":"GreenPoints.png",
                     "checked":False, "label":"Display absence points",
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

        self.appendAction(sync_changes(self))
        lyrs_label = QtGui.QLabel()
        lyrs_label.setText("Layers:")
        self.appendWidget(lyrs_label)

        for action_dict in actions:
            self.appendAction(ViewLayerAction(action_dict, self))

        nav_label = QtGui.QLabel()

        nav_label.setText("  Navigation:")
        self.addSeparator()
        self.addSeparator()
        self.appendWidget(nav_label)


        self.appendAction(viewTitleLegend(self))
        self.appendAction(fullExtent(self))

        mplActions = [{"icon":"move.png", "checked":True, "label":"Pan",
                     "tooltip":"Pan axes with left mouse, zoom with right",
                     "checkable":True, "actionfunc":"pan"},
                     {"icon":"zoom.png", "checked":False,
                     "label":"Zoom", "tooltip":"Zoom to rectangle",
                     "checkable":True, "actionfunc":"zoom"},
                      {"icon":"back.png", "checked":False, "label":"Last Extent",
                     "tooltip":"Back to previous view",
                     "checkable":False, "actionfunc":"back"}, {"icon":"forward.png",
                     "checked":False, "label":"Next Extent",
                     "tooltip":"Forward to next extent",
                     "checkable":False, "actionfunc":"forward"},
                      {"icon":"filesave.png", "checked":False, "label":"Save",
                     "tooltip":"Save the figure", "checkable":False,
                     "actionfunc":"save_figure"},]
        for action_dict in mplActions:
            self.appendAction(MPL_action(action_dict, self))


    def updateToolBar(self):
        QCellToolBar.updateToolBar(self)
        sw = self.getSnappedWidget()

        for action in self.actions():
            if type(action) == ViewLayerAction:
                #disenable all action refering to data we don't have
                action.setEnabled(sw.all_layers[action.tag]['enabled'])
                action.setChecked(sw.all_layers[action.tag]['displayed'])

        sw.popMenu = self.gen_popup_menu()

    def gen_popup_menu(self):
        sw = self.getSnappedWidget()
        popmenu = QtGui.QMenu(sw)
        toolbar = QtGui.QToolBar()
        for action in self.actions():
            if action.isSeparator():
                popmenu.addSeparator()
            elif not isinstance(action, QtGui.QWidgetAction):
                action.setIconVisibleInMenu(True)
                popmenu.addAction(action)
            else:
                popmenu.addSeparator()

#        for action in sw.mpl_toolbar.actions():
#            action.setIconVisibleInMenu(True)
#            popmenu.addAction(action)

        return popmenu


class AnchoredText(AnchoredOffsetbox):
    def __init__(self, s, loc, pad=0.4, borderpad=0.5, prop=None, frameon=True):

        self.txt = TextArea(s, minimumdescent=False)

        super(AnchoredText, self).__init__(loc, pad=pad, borderpad=borderpad,
                                           child=self.txt,
                                           prop=prop,
                                           frameon=frameon)
