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
try:
    from vistrails.core.modules.vistrails_module import Module
    from vistrails.packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
    from vistrails.packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
    from vistrails.packages.spreadsheet.spreadsheet_controller import spreadsheetController
    from vistrails.core.packagemanager import get_package_manager
except ImportError:
    from core.modules.vistrails_module import Module
    from packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
    from packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
    from packages.spreadsheet.spreadsheet_controller import spreadsheetController
    from core.packagemanager import get_package_manager
    
from sahm_picklists import OutputRaster
from utils import map_ports
from SahmSpatialOutputViewer import AnchoredText, MyMapCanvas, \
    RasterDisplay, fullExtent, viewTitleLegend, sync_changes, MPL_action 
from utils import getRasterParams
from pySAHM.utilities import dbfreader as dbfreader

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

from sahm_picklists import mpl_colormap

import GenerateModuleDoc as GenModDoc
doc_file = os.path.abspath(os.path.join(os.path.dirname(__file__),  "documentation.xml"))
GenModDoc.load_documentation(doc_file)

class GeneralSpatialViewer(SpreadsheetCell):
    """
    SAHMModelOutputViewerCell is a VisTrails Module that
    displays the various output from a SAHM Model run in a single cell

    colorRamp values can be found at:

    """
    __doc__ = GenModDoc.construct_module_doc('GeneralSpatialViewer')
    _input_ports = [("row", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ("column", "(edu.utah.sci.vistrails.basic:Integer)"),
                    ("rasterFile", '(edu.utah.sci.vistrails.basic:Path)'),
                    ("colorRamp", '(gov.usgs.sahm:mpl_colormap:Other)', {'defaults':'["jet"]'}),
                    ('categorical', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'["False"]', 'optional':False}),
                    ('threeBand', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'["False"]', 'optional':False}),
                    ('dataMin', '(edu.utah.sci.vistrails.basic:Float)'),
                    ('dataMax', '(edu.utah.sci.vistrails.basic:Float)'),
                    ('NoDataValue', '(edu.utah.sci.vistrails.basic:Float)'),]

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'out') 

    def __init__(self):
        SpreadsheetCell.__init__(self)

        self.port_map = {'rasterFile': ("rasterFile", None, True),
            'colorRamp': ("colorRamp", None, True),
            'categorical': ("categorical", None, True),
            'threeBand': ("threeBand", None, True),
            'dataMin': ("dataMin", None, False),
            'dataMax': ("dataMax", None, False),
            'NoDataValue': ("NoDataValue", None, False),
            }

    def compute(self):
        inputs = {}

        inputs = map_ports(self, self.port_map)

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

        self.local_displayAndWait(inputs)       
    
    def local_displayAndWait(self, inputs):
        self.displayAndWait(SpatialViewerCellWidget, inputs)

class SpatialViewerCellWidget(QCellWidget):
    """

    """
    def __init__(self, parent=None):
        """ QGISCellWidget(parent: QWidget) -> QGISCellWidget
        Initialize the widget with its central layout

        """
        QCellWidget.__init__(self, parent)

        self.displaylegend = False

        centralLayout = QtGui.QVBoxLayout()
        self.setLayout(centralLayout)
        centralLayout.setMargin(0)
        centralLayout.setSpacing(0)
        self.create_main_frame()
        self.fig.canvas.draw()
        self.sync_changes = "all"
        self.setAnimationEnabled(False)
        self.displayTL = False
        self.cmap = matplotlib.cm.jet
        self.categorical = False
        self.threeBand = False
        self.dataMin = "ExtractFromFile"
        self.dataMax = "ExtractFromFile"
        self.NoDataValue = "ExtractFromFile"

    def updateContents(self, inputs):
        """ updateContents(inputs: dictionary) -> None
        Update the widget contents based on the input data
        """
        self.toolBarType = GeneralSpatialViewerToolBar
        self.controlBarType = GeneralSpatialViewerToolBar
        self.rasterFile = inputs["rasterFile"]
        self.cmap = eval("matplotlib.cm." + inputs["colorRamp"])

        for input in ['categorical' , 'threeBand']:
            if type(inputs[input]) is str:
                self.__dict__[input] = eval(inputs[input])
            else:
                self.__dict__[input] = inputs[input]

        for input in ['dataMin' , 'dataMax', 'NoDataValue']:
            if inputs.has_key(input) and \
            type(inputs[input]) is str and \
            inputs[input] != "ExtractFromFile":
                self.__dict__[input] = eval(inputs[input])
            elif inputs.has_key(input):
                self.__dict__[input] = inputs[input]
            else:
                self.__dict__[input] = "ExtractFromFile"  

        self.load_layers()
        self.on_draw(UseMaxExt=True)

        self.maxXlim, self.maxYlim = self.getMaxDisplayExtent()
        self.axes.set_ylim(self.maxYlim, emit=False)
        self.axes.set_xlim(self.maxXlim, emit=False)
        self.fig.canvas.draw()
        self.update()


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
        self.map_canvas.mpl_connect('resize_event', self._resize)
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
        toolbar = GeneralSpatialViewerToolBar(sheet)
        row, col = self.findCurrentCell()
        toolbar.snapTo(row, col)
        return toolbar.gen_popup_menu()

    def getRasterParams(self, rasterfile):
        return getRasterParams(rasterfile)

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

    def _resize(self, event):
        self.pull_pixels()

    def pull_pixels(self):
        try:
            self.rasterlayer.ax_update(self.axes)
        except:
            pass

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

    def load_layers(self):
        rasterparams = getRasterParams(self.rasterFile)
        self.maxExtent = [rasterparams["ulx"],rasterparams["lrx"],rasterparams["lry"],rasterparams["uly"]]

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

    def on_draw(self, UseMaxExt=False, passedExtent=None):
        """ Completely clears then redraws the figure
        There's probably a more efficient way to do this.
        """
        if passedExtent is not None:
            curExtent = passedExtent
        elif UseMaxExt:
            curExtents = self.getMaxExtent()
        else:
            curExtents = self.get_extent()
            

        self.fig.clear()
        self.add_axis()
        
        self.add_raster(curExtents)
        
        title = os.path.splitext(os.path.split(self.rasterFile)[1])[0]
        
#        if self.displayTL:
#            self.add_title(title)
            

    def add_raster(self, curExtents):
        self.rasterlayer = RasterDisplay(self.threeBand, self.NoDataValue)
        self.rasterlayer.setDims(self.axes)
        self.rasterlayer.switch_raster(self.rasterFile)

        raster_array = self.rasterlayer(*curExtents)

        if self.categorical and not self.threeBand:
            raster_plot = self.axes.imshow(raster_array, interpolation="nearest", cmap=self.cmap, origin='upper', extent=self.getDataExtent())
        elif self.threeBand:
            raster_plot = self.axes.imshow(raster_array, origin='lower', extent=self.getDataExtent())
        else:
            min, max = utils.getrasterminmax(self.rasterFile)
            if not self.dataMin == "ExtractFromFile":
                min = float(self.dataMin)
            if not self.dataMax == "ExtractFromFile":
                max = float(self.dataMax)
                
            rmax = max
            rmin = min

            norm = colors.Normalize(rmin, rmax)
            raster_plot = self.axes.imshow(raster_array,interpolation="nearest", cmap=self.cmap, norm=norm, origin='upper', extent=self.getDataExtent())
            

#        if self.displayTL:
#            if self.categorical:
#                cb = self.fig.colorbar(raster_plot, orientation='vertical', pad=0.01, shrink=.9, fraction=.3, aspect=15)
##                cb.ax.set_yticklabels(kwargs['cbar_labels'])
#            else:
#                cb = self.fig.colorbar(raster_plot, orientation='vertical', pad=0.01, fraction=.1, shrink=.9, aspect=30)
#
#            for t in cb.ax.get_xticklabels():
#                if self.categorical:
#                    t.set_fontsize(5)
#                    t.set_rotation(90)
#                else:
#                    t.set_fontsize(7)

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
            if type(cell) == SpatialViewerCellWidget:
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
    def set_extent(self, ylim, xlim):
        self.axes.set_ylim(ylim, emit=False)
        self.axes.set_xlim(xlim, emit=False)

        self.pull_pixels()
        self.fig.canvas.draw()
        self.update()
        
    def get_extent(self):
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
        for spatialViewer in self.get_active_cells():
            spatialViewer.set_extent(self.axes.get_ylim(), self.axes.get_xlim())
            spatialViewer.pull_pixels()
            spatialViewer.map_canvas.draw()

#class AnchoredText(AnchoredOffsetbox):
#    def __init__(self, s, loc, pad=0.4, borderpad=0.5, prop=None, frameon=True):
#
#        self.txt = TextArea(s, minimumdescent=False)
#
#        super(AnchoredText, self).__init__(loc, pad=pad, borderpad=borderpad,
#                                           child=self.txt,
#                                           prop=prop,
#                                           frameon=frameon)

#class MyMapCanvas(FigureCanvas):
#    def __init__(self, fig):
#        FigureCanvas.__init__(self, fig)
#        self._cursor = None
#
#    def resizeEvent(self, event):
#        if not event.size().height() == 0:
#            FigureCanvas.resizeEvent(self, event)
#
#    def enterEvent(self, event):
#        if (self._cursor is not None and
#            QtGui.QApplication.overrideCursor() is None):
#            QtGui.QApplication.setOverrideCursor(self._cursor)
#        FigureCanvas.enterEvent(self, event)
#
#    def leaveEvent(self, event):
#        self._cursor = QtGui.QCursor(QtGui.QApplication.overrideCursor())
#        QtGui.QApplication.restoreOverrideCursor()
#        FigureCanvas.leaveEvent(self, event)

#class RasterDisplay(object):
#    '''The idea behind this is from
#    http://matplotlib.sourceforge.net/examples/event_handling/viewlims.py
#    basically we want to only query as much data as we have screen pixels for.
#    When the user zooms, pans, resizes we'll go back to the original display
#    and get another set of pixels.
#
#    This object has a pointer to the original raster and functions
#    for switching the input file or getting an array of pixel values
#    '''
#    def __init__(self, threeBand=False, width=300, height=300):
#        self.height = height
#        self.width = width
#        self.threeBand = threeBand
#
#    def switch_raster(self, rasterfile):
#        self.rasterfile = rasterfile
#        self.rasterparams = getRasterParams(rasterfile)
#
#
#    def __call__(self, xstart, xend, ystart, yend):
#        print "RasterDisplay __call__"
#        self.x = np.linspace(xstart, xend, self.width)
#        self.y = np.linspace(ystart, yend, self.height).reshape(-1,1)
#
#         #pull the pixels we need, no more
#        ds = gdal.Open(self.rasterfile, gdal.GA_ReadOnly)
#
#        xform  = ds.GetGeoTransform()
#
#        if xstart < xform[0]:
#            xstart = xform[0]
#        if xend > (xform[0] + (ds.RasterXSize * xform[1])):
#            xend = (xform[0] + (ds.RasterXSize * xform[1]))
#        if ystart < (xform[3] + (ds.RasterYSize * xform[5])):
#            ystart = xform[3]
#        if yend > xform[3]:
#            xend = xform[3]
#        
#
#        ncols = int((xend - xstart) / xform[1])
#        nrows = int((yend - ystart) / abs(xform[5]))
#
#        xOffset = int((xstart - xform[0]) / xform[1])
#        yOffset = int((yend- xform[3]) / xform[5])
#
#        if xOffset + ncols > ds.RasterXSize:
#            xOffset = max(0, ds.RasterXSize - xOffset)
#        if xOffset < 0:
#            xOffset = 0
#
#        if yOffset + nrows > ds.RasterYSize:
#            yOffset = 0
#        if yOffset < 0:
#            yOffset = 0
#
#        if ncols + xOffset > ds.RasterXSize:
#            ncols = ds.RasterXSize - xOffset
#        if ncols < 0:
#            ncols = ds.RasterXSize
#
#        if nrows + yOffset > ds.RasterYSize:
#            nrows = ds.RasterYSize - yOffset
#        if nrows < 0:
#            nrows = ds.RasterYSize
#
#        print "rows, cols:  ", ncols, nrows
#        print "pixelspulled: ", self.height, self.width
#
#        ary = ds.GetRasterBand(1).ReadAsArray(xoff=xOffset, yoff=yOffset,
#                                              win_xsize=ncols, win_ysize=nrows,
#                                              buf_ysize=self.height, buf_xsize=self.width)
#        
#        ndval = ds.GetRasterBand(1).GetNoDataValue()
#
#        return np.ma.masked_array(ary, mask=(ary==ndval))
#
#    def setDims(self, ax):
#        #Get the number of points from the number of pixels in the window
#        dims = ax.axesPatch.get_window_extent().bounds
#        print "RasterDisplay setDims:   ", dims
#        self.width = int(dims[2] + 0.5)
#        self.height = int(dims[3] + 0.5)
#
#    def ax_update(self, ax):
#        ax.set_autoscale_on(False) # Otherwise, infinite loop
#        self.setDims(ax)
#        print "RasterDisplay ax_update"
#
#        #Get the range for the new area
#        xstart,ystart,xdelta,ydelta = ax.axes.viewLim.bounds
#        xend = xstart + xdelta
#        yend = ystart + ydelta
#
#        factor = 0.1 #we want to pull more pixels than we absolutely need
#        #so that we don't get white edges
#        xBuff = (xend - xstart) * factor
#        yBuff = (yend - ystart) * factor
#        xstart = xstart - xBuff
#        ystart = ystart - yBuff
#        xend = xend + xBuff
#        yend = yend + yBuff
#
#        print "   Before: "
#        print "     xlim: ", int(xstart), int(xend)
#        print "     ylim: ", int(ystart), int(yend)
#
#        #reel these values in if they are outside our bounds
#        for bound in ['xstart', 'xend']:
#            if eval(bound) < self.rasterparams["ulx"]:
#                exec(bound + " = " + str(self.rasterparams["ulx"]))
#            if eval(bound) > self.rasterparams["lrx"]:
#                exec(bound + " = " + str(self.rasterparams["lrx"]))
#            
#        for bound in ['ystart', 'yend']:
#            if eval(bound) < self.rasterparams["lry"]:
#                exec(bound + " = " + str(self.rasterparams["lry"]))
#            if eval(bound) > self.rasterparams["uly"]:
#                exec(bound + " = " + str(self.rasterparams["uly"]))
#
#        # Update the image object with our new data and extent
#        im = ax.images[-1]
#        print "   After: "
#        print "     xlim: ", int(xstart), int(xend)
#        print "     ylim: ", int(ystart), int(yend)
#        im.set_data(self.__call__(xstart, xend, ystart, yend))
#        im.set_extent((xstart, xend, ystart, yend))
#        ax.figure.canvas.draw_idle()
#
##class fullExtent(QtGui.QAction):
##    def __init__(self, parent=None):
##        icon = os.path.abspath(os.path.join(
##                    os.path.dirname(__file__), "Images", "world.png"))
##        QtGui.QAction.__init__(self,
##                               QtGui.QIcon(icon),
##                               "Full Extent",
##                               parent)
##        self.setCheckable(False)
##
##    def triggeredSlot(self):
##        cellWidget = self.toolBar.getSnappedWidget()
##        cellWidget.zoomFull()
#
#class viewTitleLegend(QtGui.QAction):
#    def __init__(self, parent=None):
#        icon = os.path.abspath(os.path.join(
#                    os.path.dirname(__file__), "Images", "titlelegend.png"))
#        QtGui.QAction.__init__(self,
#                               QtGui.QIcon(icon),
#                               "Show/Hide Title and Legend",
#                               parent)
#        self.setCheckable(True)
#        self.setChecked(True)
#
#    def triggeredSlot(self):
#        cellWidget = self.toolBar.getSnappedWidget()
#
#        active_cells = cellWidget.get_active_cells()
#        for cell in active_cells:
#
#            xlim = cell.axes.get_xlim()
#            ylim = cell.axes.get_ylim()
#            cell.displayTL = self.isChecked()
#            cell.on_draw()
#            cell.fig.canvas.draw()
#            cell.update()
#            cell.axes.set_xlim(xlim)
#            cell.axes.set_ylim(ylim)
#
#class sync_changes(QtGui.QAction):
#
#    def __init__(self, parent=None):
#        self.sync_options = itertools.cycle(["all", "sel", "one"])
#
#        QtGui.QAction.__init__(self,
#                               self.getIcon(self.sync_options.next()),
#                               r"Apply changes to all / selected / single cell",
#                               parent)
#        self.setCheckable(True)
#        self.setChecked(True)
#
#
#    def getIcon(self, tag):
#        icon = os.path.abspath(os.path.join(
#                    os.path.dirname(__file__), "Images", tag + ".png"))
#        return QtGui.QIcon(icon)
#
#    def triggeredSlot(self):
#        cellWidget = self.toolBar.getSnappedWidget()
#        next_option = self.sync_options.next()
#        self.setIcon(self.getIcon(next_option))
#        cellWidget.sync_changes = next_option

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

#class MPL_action(QtGui.QAction):
#
#    def __init__(self, action_dict, parent=None):
#        icon = os.path.abspath(os.path.join(
#                    os.path.dirname(__file__), "Images", action_dict["icon"]))
#        QtGui.QAction.__init__(self,
#                               QtGui.QIcon(icon),
#                               action_dict["label"],
#                               parent)
#        self.setToolTip(action_dict["tooltip"])
#        self.setCheckable(action_dict["checkable"])
#        self.setChecked(action_dict["checked"])
#        self.actionfunc = action_dict["actionfunc"]
#        self.tag = action_dict["label"]
#
#    def triggeredSlot(self, checked=False):
#
#        cellWidget = self.toolBar.getSnappedWidget()
#
#        if self.tag in self.tag in ["Pan", "Zoom"]:
#            if self.isChecked():
#                cursor = self.tag
#            elif self.tag == 'Pan':
#                cursor = "Zoom"
#            elif self.tag == 'Zoom':
#                cursor = "Pan"
#
#        active_cells = cellWidget.get_active_cells()
#        for cell in active_cells:
#
#            if self.tag in ["Pan", "Zoom"]:
#                zoomaction = cell.getActionByTag("Zoom")
#                panaction = cell.getActionByTag("Pan")
#
#                if cursor == "Zoom" and \
#                    (not zoomaction.isChecked() or cellWidget is cell):
#                    cell.mpl_toolbar.zoom()
#                elif cursor == "Pan" and \
#                    (not panaction.isChecked() or cellWidget is cell):
#                    cell.mpl_toolbar.pan()
#
#                zoomaction.setChecked(cursor=="Zoom")
#                panaction.setChecked(cursor=="Pan")
#
#            else:
#                eval("cell.mpl_toolbar." + self.actionfunc + "()")
#                cell.pull_pixels()
#
#    def getAction(self, name):
#        for action in self.parent().actions():
#            if hasattr(action, "actionfunc") and \
#                action.actionfunc == name:
#                return action
#        return None


class GeneralSpatialViewerToolBar(QCellToolBar):
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

        actions = []

        self.appendAction(sync_changes(self))
        lyrs_label = QtGui.QLabel()
#        lyrs_label.setText("Layers:")
#        self.appendWidget(lyrs_label)

#        for action_dict in actions:
#            self.appendAction(ViewLayerAction(action_dict, self))

        nav_label = QtGui.QLabel()

        nav_label.setText("  Navigation:")
        self.addSeparator()
        self.addSeparator()
        self.appendWidget(nav_label)


#        self.appendAction(viewTitleLegend(self))
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

#        for action in self.actions():
#            if type(action) == ViewLayerAction:
#                #disenable all action refering to data we don't have
#                action.setEnabled(sw.all_layers[action.tag]['enabled'])

        #Strip out the unused actions
        keep_actions = ['Zoom', 'Save', 'Back', 'Forward', 'Pan']
        keep_actions = []
        for action in sw.mpl_toolbar.actions():
            if not action.text() in keep_actions and action.text():
                continue
            if action.text() == 'Zoom':
                icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "Images", "zoom.png"))
                action.setIcon(QtGui.QIcon(icon))
            if action.text()  == 'Pan':
                action.setChecked(True)
            self.appendAction(action)

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


#class AnchoredText(AnchoredOffsetbox):
#    def __init__(self, s, loc, pad=0.4, borderpad=0.5, prop=None, frameon=True):
#
#        self.txt = TextArea(s, minimumdescent=False)
#
#        super(AnchoredText, self).__init__(loc, pad=pad, borderpad=borderpad,
#                                           child=self.txt,
#                                           prop=prop,
#                                           frameon=frameon)

#class popup_menu(QtGui.QMenu):
#    def __init__(self, parent=None, mpl_toolbar=None):
#
#        QtGui.QMenu.__init__(self, parent)
#
#        toolbar = QtGui.QToolBar()
#        for action in mpl_toolbar.actions():
#            action.setIconVisibleInMenu(True)
#            self.addAction(action)
#
#        # Actions
#        icon = os.path.abspath(os.path.join(os.path.dirname(__file__), "Images", "RedPoints.png"))
#        self.actionPresence = QtGui.QAction(QtGui.QIcon(icon), "Presence points", self)
#        self.actionPresence.setStatusTip("Display/hide presence points")
#        self.connect(self.actionPresence, QtCore.SIGNAL("triggered()"), self.on_pointsclick)
#        self.actionPresence.setIconVisibleInMenu(True)
#
#        self.actionAbsence = QtGui.QAction(QtGui.QIcon("GreenPoints.png"), "Presence points", self)
#        self.actionAbsence.setStatusTip("Display/hide absence points")
#
#
#        # create context menu
#        self.addAction(self.actionPresence)
#        self.addAction(self.actionAbsence)
#        self.addSeparator()
#
#
#    def on_pointsclick(self):
#        QtGui.QMessageBox.about(self, "I do nothing", "no really")