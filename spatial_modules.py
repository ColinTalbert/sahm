###############################################################################
#  #
#  # Copyright (C) 2010-2012, USGS Fort Collins Science Center.
#  # All rights reserved.
#  # Contact: talbertc@usgs.gov
#  #
#  # This file is part of the Software for Assisted Habitat Modeling package
#  # for VisTrails.
#  #
#  # "Redistribution and use in source and binary forms, with or without
#  # modification, are permitted provided that the following conditions are met:
#  #
#  #  - Redistributions of source code must retain the above copyright notice,
#  #    this list of conditions and the following disclaimer.
#  #  - Redistributions in binary form must reproduce the above copyright
#  #    notice, this list of conditions and the following disclaimer in the
#  #    documentation and/or other materials provided with the distribution.
#  #  - Neither the name of the University of Utah nor the names of its
#  #    contributors may be used to endorse or promote products derived from
#  #    this software without specific prior written permission.
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
import os
import gc
import itertools
import re
import copy
import math

import pylab

import utils

try:
    import fiona
    import shapely
    from pyproj import Proj, transform
    from shapely.geometry import shape
    from matplotlib.patches import Polygon
    from matplotlib.collections import PatchCollection
except ImportError:
    fiona = None

from PyQt4 import QtCore, QtGui
from vistrails.core.modules.vistrails_module import Module, ModuleError
from vistrails.packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
from vistrails.packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
from vistrails.packages.spreadsheet.spreadsheet_controller import spreadsheetController
from vistrails.core.packagemanager import get_package_manager

from utils import map_ports

import pySAHM.utilities as utilities
from pySAHM.utilities import dbfreader as dbfreader

import matplotlib
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QTAgg as NavigationToolbar

from matplotlib.figure import Figure
from matplotlib.offsetbox import AnchoredOffsetbox, TextArea
import matplotlib.colors as colors

import numpy as np

from osgeo import gdal, gdalconst
from osgeo import ogr, osr

import pySAHM.SpatialUtilities as SpatialUtilities

import GenerateModuleDoc as GenModDoc
doc_file = os.path.abspath(os.path.join(os.path.dirname(__file__), "documentation.xml"))
GenModDoc.load_documentation(doc_file)

class BaseGeoViewerCell(SpreadsheetCell):
    _input_ports = [("row", "(edu.utah.sci.vistrails.basic:Integer)", {'optional': True}),
                    ("column", "(edu.utah.sci.vistrails.basic:Integer)", {'optional': True}),
                    ("vector_layers", "(edu.utah.sci.vistrails.basic:Dictionary)", {'optional': False}),
                    ('display_states', '(edu.utah.sci.vistrails.basic:Boolean)',
                                    {'defaults':'["True"]', 'optional':True}),
                    ('display_colorbar', '(edu.utah.sci.vistrails.basic:Boolean)',
                                    {'defaults':'["True"]', 'optional':True}),
                    ('Location', '(org.vistrails.vistrails.spreadsheet:CellLocation)',
                                    {'optional':True})]


    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'out')

    def __init__(self, parent=None):
        SpreadsheetCell.__init__(self)

        self.port_map = {}

    def compute(self):
        self.inputs = self.parse_inputs()
        self.inputs.update(map_ports(self, self.port_map))

        self.local_displayAndWait(self.inputs)

    def parse_inputs(self):
        inputs = {}

        self.location = utils.get_sheet_location(self)

        inputs['vector_layers'] = self.force_get_input_list('vector_layers')
        #  ugly hack to get the viswall to work.  Serial/unserialize nests
        #  the list of vector_layers in an additional list
        if inputs['vector_layers'] != [] and type(inputs['vector_layers'][0]) == list:
            inputs['vector_layers'] = inputs['vector_layers'][0]

        inputs['raster_layers'] = self.force_get_input_list('raster_layers')
        #  ugly hack to get the viswall to work.  Serial/unserialize nests
        #  the list of vector_layers in an additional list
        if inputs['raster_layers'] != [] and type(inputs['raster_layers'][0]) == list:
            inputs['raster_layers'] = inputs['raster_layers'][0]

        inputs["display_states"] = self.force_get_input("display_states", True)
        inputs["display_colorbar"] = self.force_get_input("display_colorbar", True)
        return inputs

    def local_displayAndWait(self, inputs):
        self.displayAndWait(SpatialViewerCellWidget, inputs)

class GeoSpatialViewerCell(BaseGeoViewerCell):
    """
    SAHMModelOutputViewerCell is a VisTrails Module that
    displays the various output from a SAHM Model run in a single cell

    cmap values can be found at:

    """
    __doc__ = GenModDoc.construct_module_doc('GeoSpatialViewerCell')
    _input_ports = copy.deepcopy(BaseGeoViewerCell._input_ports)
    _input_ports.extend([ ("raster_layers",
                           "(edu.utah.sci.vistrails.basic:Dictionary)")])

class SpatialViewerCellWidgetBase(QCellWidget):
    """

    """
    def __init__(self, parent=None):
        """ QGISCellWidget(parent: QWidget) -> QGISCellWidget
        Initialize the widget with its central layout

        """
        QCellWidget.__init__(self, parent)

        self.SyncChangesButton = "all"
        self.setAnimationEnabled(False)
        self.display_states = True
        self.display_colorbar = True
        self.cursor_mode = 'zoom'

        centralLayout = QtGui.QVBoxLayout()
        self.setLayout(centralLayout)
        centralLayout.setMargin(0)
        centralLayout.setSpacing(0)
        self.create_main_frame()

    def updateContents(self, inputs):
        """ updateContents(inputs: dictionary) -> None
        Update the widget contents based on the input data
        """
        self.set_toolbars()
        for raster_kwargs in inputs['raster_layers']:
            if isinstance(raster_kwargs['cmap'], basestring):
                raster_kwargs['cmap'] = eval("matplotlib.cm." +
                                                  raster_kwargs['cmap'])
            if raster_kwargs['categorical']:
                uniques, labels, cmap = make_categorical_cmap(raster_kwargs)
                raster_kwargs['cmap'] = cmap
                raster_kwargs['unique_vals'] = uniques
                raster_kwargs['unique_labels'] = labels
        self.inputs = inputs
        self.load_layers()

        self.display_states = inputs["display_states"]
        self.display_colorbar = inputs["display_colorbar"]

        self.on_draw(view_extent=self.get_max_extent())

        self.fig.canvas.draw()
        self.update()

#          self.cid = self.axes.callbacks.connect('ylim_changed', self.lim_changed)
#          self.cid = self.axes.callbacks.connect('xlim_changed', self.lim_changed)
#          self.axes.end_pan = self.end_pan

    def adj_aspect(self, extent):

        #  total limits of all the data
        (d_x0, d_x1), (d_y0, d_y1) = self.get_max_extent()
        d_width = d_x1 - d_x0
        d_height = d_y1 - d_y0
        xr = 1.05 * d_width
        yr = 1.05 * d_height

        d_aspect = float(d_height) / d_width


        #  limits of the data in the zoom box
        (z_x0, z_x1), (z_y0, z_y1) = extent
        z_width = max(math.fabs(z_x1 - z_x0), 1e-30)
        z_height = max(math.fabs(z_y1 - z_y0), 1e-30)

        z_aspect = float(z_height) / z_width

        ax_bounds = self.axes.axesPatch.get_window_extent().bounds
        ax_width = int(ax_bounds[2] + 0.5)
        ax_height = int(ax_bounds[3] + 0.5)

        ax_aspect = float(ax_height) / ax_width

        xmarg = d_width - xr
        ymarg = d_height - yr
        Ysize = ax_aspect * z_width
        Xsize = z_height / ax_aspect
        Xmarg = Xsize - xr
        Ymarg = Ysize - yr
        xm = 0  #  Setting these targets to, e.g., 0.05*xr does not seem to
                #  help.
        ym = 0

        y_expander = (d_aspect * z_width / z_height - 1.0)

        if abs(y_expander) < 0.005:
    #         print 'good enough already'
            return

        if xmarg > xm and ymarg > ym:
            adjy = ((Ymarg > 0 and y_expander < 0)
                    or (Xmarg < 0 and y_expander > 0))
        else:
            adjy = y_expander > 0

        if adjy:
            yc = 0.5 * (z_y0 + z_y1)
            y0 = yc - Ysize / 2.0
            y1 = yc + Ysize / 2.0

            adj_extent = ((z_x0, z_x1), (y0, y1))
            #  print 'New y0, y1:', y0, y1
            #  print 'New ysize, ysize/xsize', y1-y0, (y1-y0)/xsize
        else:
            xc = 0.5 * (z_x0 + z_x1)
            x0 = xc - Xsize / 2.0
            x1 = xc + Xsize / 2.0
            adj_extent = ((x0, x1), (z_y0, z_y1))

        return adj_extent

#      def lim_changed(self, event):
#          print '!!!!!!!!!!!!lim_changed', os.path.split(self.inputs['model_workspace'])[1]
#  #          if self.cursor_mode == "zoom":
#  #              print '\tin zoom'
#  #              extent = self.adj_aspect(self.get_extent())
#  #              self.sync_extents(extent=extent)

    def on_draw_base(self, view_extent=None):
        """ Completely clears then redraws the figure
        There's probably a more efficient way to do this.
        """
        if view_extent:
            xlim, ylim = view_extent
        else:
            xlim, ylim = self.get_extent()

        self.fig.clear()
        self.add_axis()

        self.display_raster(xlim, ylim)

        self.set_axis_extent(xlim, ylim)
        if self.display_states:
            self.add_states()

        for vector_layer in self.vector_layers:
            if vector_layer:
                kwargs = dict(vector_layer)
                del(kwargs['input_file'])
                if kwargs['shapetype'] == 'polygon':
                    del(kwargs['shapetype'])
                    self.add_poly_layer(vector_layer['input_file'], **kwargs)
                elif kwargs['shapetype'] == 'point':
                    del(kwargs['shapetype'])
                    self.add_point_layer(vector_layer['input_file'], **kwargs)


        if self.display_colorbar:
            if self.rasterdisplay_layer.kwargs['categorical']:
                ticks = self.rasterdisplay_layer.kwargs['cbar_ticks']
                cb = self.fig.colorbar(self.raster_plot, ticks=ticks,
                                       orientation='vertical', pad=0.01,
                                       shrink=.9, fraction=.3, aspect=15)
                cb.ax.set_yticklabels(self.rasterdisplay_layer.kwargs['cbar_labels'])
                for t in cb.ax.get_yticklabels():
                    t.set_fontsize(7)
            else:
                cb = self.fig.colorbar(self.raster_plot, orientation='horizontal',
                                   pad=0.01, fraction=.1, shrink=.9, aspect=30)
                for t in cb.ax.get_xticklabels():
                    t.set_fontsize(7)

        self.set_axis_extent(xlim, ylim)

#          self.cid = self.axes.callbacks.connect('ylim_changed', self.lim_changed)

        self.axes.figure.canvas.draw_idle()
        self.update()

    def set_raster_base(self, raster_kwargs):
        '''The raster being displayed sets the data_max extent and projection for our
        map.  There can be only one and switching it requires recreating the
        whole shooting match. Set it up here
        '''
        self.rasterdisplay_layer = RasterDisplay()
        self.rasterdisplay_layer.setDims(self.axes)
        self.rasterdisplay_layer.switch_raster(raster_kwargs)

    def display_raster(self, xlim, ylim):
        '''refresh the given raster
        '''

        xlim_clip = list(np.clip(xlim, self.rasterdisplay_layer.raster.west, self.rasterdisplay_layer.raster.east))
        ylim_clip = list(np.clip(ylim, self.rasterdisplay_layer.raster.south, self.rasterdisplay_layer.raster.north))

        raster_array = self.rasterdisplay_layer(xlim, ylim)

        if self.rasterdisplay_layer.kwargs['categorical']:
            self.raster_plot = self.axes.imshow(raster_array,
                                            interpolation="nearest",
                                            cmap=self.rasterdisplay_layer.cmap,
                                            origin='upper',
                                            extent=xlim_clip + ylim_clip)

        else:
            rmax = self.rasterdisplay_layer.display_max
            rmin = self.rasterdisplay_layer.display_min

            norm = colors.Normalize(rmin, rmax)
            self.raster_plot = self.axes.imshow(raster_array,
                                            interpolation="nearest",
                                            cmap=self.rasterdisplay_layer.cmap,
                                            norm=norm, origin='upper',
                                            extent=xlim_clip + ylim_clip)

    def create_main_frame(self):
        self.setFocusPolicy(QtCore.Qt.ClickFocus)
        self.dpi = 100
        self.fig = Figure((5.0, 4.0), dpi=self.dpi)

        self.fig.subplots_adjust(left=0, right=1, top=1, bottom=0)
        self.map_canvas = MyMapCanvas(self.fig)
        self.map_canvas.mpl_connect('scroll_event', self.wheel_zoom)

#          self.map_canvas.mpl_connect('button_press_event', self.button_down)
        self.map_canvas.mpl_connect('button_release_event', self.button_up)
        self.map_canvas.mpl_connect('resize_event', self._resize)

        self.add_axis()

        self.mpl_toolbar = NavigationToolbar(self.map_canvas, None)

        self.mpl_toolbar.zoom()

        self.popMenu = None

        self.map_canvas.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
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

    def wheel_zoom(self, event):
        #  zoom in or out centered on the current cursor position

        inv = self.axes.transData.inverted()
        curX, curY = inv.transform((event.x, event.y))

        curL, curR = self.axes.get_xlim()
        curB, curT = self.axes.get_ylim()
        width = curR - curL
        height = curT - curB
        steps = -1 * event.step / 0.25
        factor = steps / 35
        #  sanity check
        if factor > 0.5:
            factor = 0.5
        if factor < -0.5:
            factor = -0.5

        newWidth = width * (1.0 - factor)
        newHeight = height * (1.0 - factor)
        dWidth = width - newWidth
        dHeight = height - newHeight

        pcntLofX = 1 - (width - (curX - curL)) / width
        pcntUnderTop = (height - (curT - curY)) / height

        newL = curL + (dWidth * pcntLofX)
        newR = curR - (dWidth * (1 - pcntLofX))
        newB = curB + (dHeight * pcntUnderTop)
        newT = curT - (dWidth * (1 - pcntUnderTop))
        self.axes.set_xlim((newL, newR))
        self.axes.set_ylim((newB, newT))

        extent = self.adj_aspect(self.get_extent())
        self.set_extent(*extent)
        self.sync_extents(extent)


#      def button_down(self, event):
#          print 'button_down', os.path.split(self.inputs['model_workspace'])[1]
#
    def button_up(self, event):
#          print 'button_up', os.path.split(self.inputs['model_workspace'])[1]
        if event.button == 1 and self.cursor_mode == 'pan':
            #  this is a left click which signifies a zoom or pan end
#              print "\tbutton1 and pan"
            self.pull_pixels()
            self.sync_extents()
        elif self.cursor_mode == 'zoom':
#              print "\tbutton1 and zoom"
            extent = self.adj_aspect(self.get_extent())
            self.set_extent(*extent)
            self.sync_extents(extent)

    def _resize(self, event):
#          try:
#              print '_resize', os.path.split(self.inputs['model_workspace'])[1]
#          except:
#              print "no inputs..."
        self.pull_pixels()

    def pull_pixels(self):
        try:
            self.rasterdisplay_layer.ax_update(self.axes)
        except:
            pass

    def keyPressEvent(self, event):
        if type(event) == QtGui.QKeyEvent and event.key() == QtCore.Qt.Key_T:
            active_cells = self.getSelectedCellWidgets()

            displayed = not self.displayTL
            for cell in active_cells:
                cell.displayTL = displayed
                cell.pull_pixels()
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

    def add_axis(self):
        self.axes = self.fig.add_subplot(111, aspect='equal', adjustable='datalim')
        self.axes.spines['right'].set_color('none')
        self.axes.spines['top'].set_color('none')
        self.axes.spines['bottom'].set_color('none')
        self.axes.spines['left'].set_color('none')
        self.axes.get_xaxis().set_visible(False)
        self.axes.get_yaxis().set_visible(False)

    def add_states(self):
        '''Add simplified state boundaries to the output map
        The file that we're displaying is stored in the sahm/data/
        '''
        sahm_dir = os.path.dirname(os.path.abspath(__file__))
        shp_fname = os.path.join(sahm_dir, "data", "states_110m", "ne_110m_admin_1_states_provinces_lakes.shp")

        self.add_poly_layer(shp_fname, edgecolor=".3", facecolor='none',
                            alpha=0.8, display_all=True)

    def add_poly_layer(self, layer_fname, display_all=False, **kwargs):
        '''add a polygon layer file to our map
        '''
        raster_crs = osr.SpatialReference()
        raster_crs.ImportFromWkt(self.rasterdisplay_layer.raster.ds.GetProjection())

        if kwargs.has_key('query'):
            query_function = self.parse_query(kwargs['query'])
            del(kwargs['query'])
        else:
            query_function = lambda rec: True


        with fiona.open(layer_fname) as fiona_shp:
            shape_proj = Proj(fiona_shp.crs)
            raster_proj = Proj(raster_crs.ExportToProj4())

            ((minx, maxx), (miny, maxy)) = self.get_extent()
            shift360 = False
            if minx > 180 and minx < 360:
                minx -= 360
                shift360 = True
            if maxx > 180 and maxx < 360:
                maxx -= 360
                shift360 = True

            (minx, maxx), (miny, maxy) = transform(raster_proj, shape_proj, (minx, maxx), (miny, maxy))
            patches = []

            if display_all:
                recs = fiona_shp
            else:
                recs = fiona_shp.filter(bbox=(minx, miny, maxx, maxy))

            for rec in recs:
                if query_function(rec):
                    patches += self.get_patches(rec['geometry'],
                                   shape_proj, raster_proj, shift360)

            pc = PatchCollection(patches, **kwargs)
            self.axes.add_collection(pc)

    def parse_query(self, query_string):
        '''given a string in standard ESRI query format returns a function that
        returns true or false if a fiona record matches that query string
        '''
        try:
            for typo in ["> =", "< =", "< >"]:
                query_string = query_string.replace(typo, typo.replace(" ", ""))

            for typo in ["=", ">", "<", "<=", ">=", "<>"]:
                query_string = query_string.replace(typo, typo.replace(typo, " " + typo + " "))

            query_string = query_string.replace(" = ", " == ")

            for s in [' and ', ' or ', ' not ']:
                try:
                    insensitive_s = re.findall(s, query_string, re.IGNORECASE)[0]
                    query_string = query_string.replace(insensitive_s, s)
                except IndexError:
                    pass

            for col_name in re.findall('"([^"]*)"', query_string):
                query_string = query_string.replace('"' + col_name + '"', "rec['properties']['" + col_name + "']")

            def query_function(rec):
                '''the function we will be returning
                '''
                try:
                    return eval(query_string)
                except KeyError, e:
                    msg = "The field name used, " + e.message + " does not appear to be in this shapefile."
                    msg += "\n columns are: " + ", ".join(rec['properties'])
                    msg += "\n\nNote that these are case sensitive!"
                    raise ModuleError(self, msg)

            return query_function
        except:
            #  if anything goes wrong raise a module exception
            msg = "There was something wrong with your query string.\n"
            msg += "Check that it matches the ESRI format for shapefiles, such as:\n"
            msg += '"field_name" = ' + "'value'"
            msg += "\n\n acceptable opperands are =, > , <, >=, <=, And, Or, Not"
            raise ModuleError(self, msg)

    def get_patches(self, geom, shape_proj, raster_proj, shift360=False):
        '''returns a list of all the patches in the passed geometry
        '''
        patches = []
        if geom['type'] == 'Polygon':
            trans_ring = np.asarray(zip(*transform(shape_proj, raster_proj, *zip(*geom['coordinates'][0]))))
            if shift360:
                trans_ring[:, 0] = trans_ring[:, 0] + 360
                patches.append(Polygon(trans_ring))
            else:
                patches.append(Polygon(trans_ring))
#              self.plot_poly(trans_ring, **kwargs)
        elif geom['type'] == 'MultiPolygon':
            for ring in geom['coordinates']:
                trans_ring = np.asarray(zip(*transform(shape_proj, raster_proj, *zip(*ring[0]))))
                if shift360:
                    trans_ring[:, 0] = trans_ring[:, 0] + 360
                    patches.append(Polygon(trans_ring))
                else:
                    patches.append(Polygon(trans_ring))
#                  self.plot_poly(trans_ring, **kwargs)
        return patches

    def add_point_layer(self, layer_fname, **kwargs):
        '''add a polygon layer file to our map
        '''
        raster_crs = osr.SpatialReference()
        raster_crs.ImportFromWkt(self.rasterdisplay_layer.raster.ds.GetProjection())

        if kwargs.has_key('query'):
            query_function = self.parse_query(kwargs['query'])
            del(kwargs['query'])
        else:
            query_function = lambda rec: True


        with fiona.open(layer_fname) as fiona_shp:
            shape_proj = Proj(fiona_shp.crs)
            raster_proj = Proj(raster_crs.ExportToProj4())

            ((minx, maxx), (miny, maxy)) = self.get_extent()
            (minx, maxx), (miny, maxy) = transform(raster_proj, shape_proj, (minx, maxx), (miny, maxy))
            x = []
            y = []
            for rec in fiona_shp.filter(bbox=(minx, miny, maxx, maxy)):
                if query_function(rec):
                    if rec['geometry']['type'] == 'Point':
                        x1, y1 = transform(shape_proj, raster_proj,
                                                rec['geometry']['coordinates'][0],
                                                rec['geometry']['coordinates'][1])
                        x.append(x1)
                        y.append(y1)
                    elif rec['geometry']['type'] == 'MultiPoint':
                        for point in rec['geometry']:
                            x1, y1 = transform(shape_proj, raster_proj,
                                                point['coordinates'][0],
                                                point['coordinates'][1])
                            x.append(x1)
                            y.append(y1)
            self.axes.scatter(x, y, **kwargs)

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
                if p.isSheetTabWidget() == True:
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
            if isinstance(cell, SpatialViewerCellWidgetBase):
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
        if self.SyncChangesButton == "all":
            return self.get_allCellWidgets()
        elif self.SyncChangesButton == "sel":
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

    #  Functions dealing with managing extents
    def set_extent(self, xlim, ylim):
#          print '\t\t\tset_extent', os.path.split(self.inputs['model_workspace'])[1]
        self.set_axis_extent(xlim, ylim)
        self.pull_pixels()
        self.fig.canvas.draw()
        self.set_axis_extent(xlim, ylim)
        self.update()

    def set_axis_extent(self, xlim, ylim):
#          print 'set_axis_extent', os.path.split(self.inputs['model_workspace'])[1]
        self.axes.set_ylim(ylim, emit=False)
        self.axes.set_xlim(xlim, emit=False)

    def get_extent(self):
#          print 'get_extent', os.path.split(self.inputs['model_workspace'])[1]
        return self.axes.get_xlim(), self.axes.get_ylim()

    def get_max_extent(self):
#          print 'get_max_extent', os.path.split(self.inputs['model_workspace'])[1]
        return (self.rasterdisplay_layer.raster.west,
                self.rasterdisplay_layer.raster.east) , \
                (self.rasterdisplay_layer.raster.south,
                self.rasterdisplay_layer.raster.north)

    def zoomFull(self):
#          print 'zoomFull', os.path.split(self.inputs['model_workspace'])[1]
        max_xlim, max_ylim = self.get_max_extent()
        self.set_extent(max_xlim, max_ylim)
        self.sync_extents((max_xlim, max_ylim))

    def sync_extents(self, extent=''):
#          print "sync_extents", os.path.split(self.inputs['model_workspace'])[1]
        for spatialViewer in self.get_active_cells():
            if not spatialViewer is self:
                self.set_single_extent(spatialViewer, extent)
            
    def set_single_extent(self, cell, extent=''):
#          print "\t\tset_single_extent", os.path.split(self.inputs['model_workspace'])[1]
        if extent:
            x0, x1 = extent[0]
            y0, y1 = extent[1]
        else:
            x0, x1 = self.axes.get_xlim()
            y0, y1 = self.axes.get_ylim()

        try:
            x0t, y0t = SpatialUtilities.transformPoint(x0, y0, self.rasterdisplay_layer.raster.srs, cell.rasterdisplay_layer.raster.srs)
            x1t, y1t = SpatialUtilities.transformPoint(x1, y1, self.rasterdisplay_layer.raster.srs, cell.rasterdisplay_layer.raster.srs)
        except:
            x0t, y0t = x0, y0
            x1t, y1t = x1, x1

        cell.set_extent((x0t, x1t), (y0t, y1t))

class SpatialViewerCellWidget(SpatialViewerCellWidgetBase):
    '''
    '''
    def set_toolbars(self):
        self.toolBarType = GeneralSpatialViewerToolBar
        self.controlBarType = GeneralSpatialViewerToolBar

    def load_layers(self):
        self.vector_layers = self.inputs['vector_layers']

        self.set_raster_base(self.inputs['raster_layers'][0])

    def on_draw(self, view_extent=None):
        SpatialViewerCellWidgetBase.on_draw_base(self, view_extent)

class ViewLayerAction(QtGui.QAction):
    def __init__(self, action_dict, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", action_dict["icon"]))
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

            #  set all to not displayed
            for k, v in cell.all_layers.items():
                v['displayed'] = False
            #  turn on the selected layers
            for action in self.toolBar.actions():
                try:
                    if action.isChecked() and cell.all_layers.has_key(action.tag):
                        cell.all_layers[action.tag]['displayed'] = True
                except AttributeError:
                    pass  #  ignore buttons that don't have a tag set
            cell.on_draw()
            try:
                cell.fig.canvas.draw()
            except MemoryError:
                msgbox = QtGui.QMessageBox(self)
                msgbox.setText("This viewer cannot handle datasets this large.\nTry setting the max_cells_dimension to a smaller value.")
                msgbox.exec_()
                raise MemoryError

            cell.update()

class ViewStateBoundariesButton(QtGui.QAction):
    def __init__(self, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", "states.png"))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               "Show/Hide State Boundaries",
                               parent)
        self.setCheckable(True)
        self.setChecked(True)
        self.setEnabled(not fiona is None)

    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()

        active_cells = cellWidget.get_active_cells()
        for cell in active_cells:
            cell.display_states = self.isChecked()
            cell.on_draw()

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
        self.add_layers_actions()
        self.add_nav_actions()
        self.add_other_actions()

    def add_layers_actions(self):
        '''add the actions (buttons) associated with turning layers on and off
        '''
        lyrs_label = QtGui.QLabel()
        lyrs_label.setText("Layers:")
        self.appendWidget(lyrs_label)
        self.appendAction(ViewStateBoundariesButton(self))

    def add_nav_actions(self):
        '''Add the actions(buttons) associated with map navigation
        i.e. zoom, pan, extents
        '''
        self.addSeparator()
        nav_label = QtGui.QLabel()
        nav_label.setText("  Navigation:")
        self.appendWidget(nav_label)
        self.appendAction(FullExtentButton(self))

        mplActions = [{"icon":"move.png", "checked":False, "label":"Pan",
                 "tooltip":"Pan axes with left mouse, zoom with right",
                 "checkable":True, "actionfunc":"pan"},
                 {"icon":"zoom.png", "checked":True,
                 "label":"Zoom", "tooltip":"Zoom to rectangle",
                 "checkable":True, "actionfunc":"zoom"},
                  {"icon":"back.png", "checked":False, "label":"Last Extent",
                 "tooltip":"Back to previous view",
                 "checkable":False, "actionfunc":"back"}, {"icon":"forward.png",
                 "checked":False, "label":"Next Extent",
                 "tooltip":"Forward to next extent",
                 "checkable":False, "actionfunc":"forward"}]
        for action_dict in mplActions:
            self.appendAction(MPLButton(action_dict, self))

    def add_other_actions(self):
        '''Add the actions(buttons) associated with map navigation
        i.e. zoom, pan, extents
        '''
        self.addSeparator()
        other_label = QtGui.QLabel()
        other_label.setText("  Other:")
        self.appendWidget(other_label)
        self.appendAction(SyncChangesButton(self))
        self.appendAction(showColorbarButton(self))
        mpl_save = {"icon":"filesave.png", "checked":False, "label":"Save",
                     "tooltip":"Save the figure", "checkable":False,
                     "actionfunc":"save_figure"}
        self.appendAction(MPLButton(mpl_save, self))

    def updateToolBar(self):
        QCellToolBar.updateToolBar(self)
        sw = self.getSnappedWidget()

        #  Strip out the unused actions
        keep_actions = ['Zoom', 'Save', 'Back', 'Forward', 'Pan']
        keep_actions = []
        for action in sw.mpl_toolbar.actions():
            if not action.text() in keep_actions and action.text():
                continue
            if action.text() == 'Zoom':
                icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", "zoom.png"))
                action.setIcon(QtGui.QIcon(icon))
            if action.text() == 'Pan':
                action.setChecked(True)
            elif type(action) == ViewStateBoundariesButton:
                action.setEnabled(not fiona is None)
                action.setChecked(sw.display_states)
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

        return popmenu

class SyncChangesButton(QtGui.QAction):
    '''A toolbar button that allows users to choose which other cells will
    follow the extent, and other changes made to this cell
    options all = all spatial cells will be updated
            sel = only the selected cells will be updated
            one = only this cell will be updated
    '''

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
                    os.path.dirname(__file__), "data", "Images", tag + ".png"))
        return QtGui.QIcon(icon)

    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()
        next_option = self.sync_options.next()
        self.setIcon(self.getIcon(next_option))
        cellWidget.SyncChangesButton = next_option

class showColorbarButton(QtGui.QAction):
    '''A toolbar button that allows users to choose which other cells will
    follow the extent, and other changes made to this cell
    options all = all spatial cells will be updated
            sel = only the selected cells will be updated
            one = only this cell will be updated
    '''

    def __init__(self, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", "colorbar.png"))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               "Show colorbar",
                               parent)
        self.setCheckable(True)
        self.setChecked(True)
        self.setEnabled(not fiona is None)

    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()

        active_cells = cellWidget.get_active_cells()
        for cell in active_cells:
            cell.display_colorbar = self.isChecked()
            cell.on_draw()

class FullExtentButton(QtGui.QAction):
    '''The button used to go the the full extent (of the raster being displayed)
    '''
    def __init__(self, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", "world.png"))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               "Full Extent",
                               parent)
        self.setCheckable(False)

    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()
        cellWidget.zoomFull()

class MPLButton(QtGui.QAction):
    '''A button that is used to replace the built in matplotlib chart buttons
    This allows us to change the icons, tooltips, etc. as well as get them
    to interact with our particular charts as we need
    '''

    def __init__(self, action_dict, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", action_dict["icon"]))
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
                    cell.cursor_mode = 'zoom'
                elif cursor == "Pan" and \
                    (not panaction.isChecked() or cellWidget is cell):
                    cell.mpl_toolbar.pan()
                    cell.cursor_mode = 'pan'
                else:
                    cell.cursor_mode = 'none'

                zoomaction.setChecked(cursor == "Zoom")
                panaction.setChecked(cursor == "Pan")

            else:
                eval("cell.mpl_toolbar." + self.actionfunc + "()")

class RasterLayer(Module):
    __doc__ = GenModDoc.construct_module_doc('RasterLayer')

    maps = [m for m in pylab.cm.datad]
    maps.sort()

    _input_ports = [("raster_file", '(edu.utah.sci.vistrails.basic:Path)', {'optional': True}),
                    ("cmap", '(edu.utah.sci.vistrails.basic:String)',
                     {'entry_types': "['enum']",
                      'values': "[{}]".format(str(maps)), 'optional': True,
                      'defaults':'["jet"]', 'optional': True}),
                    ('categorical', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'["False"]', 'optional':True}),
                    ('three_band', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'["False"]', 'optional':True}),
                    ('display_min', '(edu.utah.sci.vistrails.basic:Float)', {'optional': True}),
                    ('display_max', '(edu.utah.sci.vistrails.basic:Float)', {'optional': True}),
                    ('NoDataValue', '(edu.utah.sci.vistrails.basic:Float)', {'optional': True}) ]

    _output_ports = [('display_dict', '(edu.utah.sci.vistrails.basic:Dictionary)')]

    port_map = {'raster_file':('raster_file', utils.get_relative_path, True),
                    'cmap': ("cmap", None, True),
                    'categorical': ("categorical", None, True),
                    'three_band': ("three_band", None, True),
                    'display_min': ("display_min", None, False),
                    'display_max': ("display_max", None, False),
                    'NoDataValue': ("NoDataValue", None, False),
                    }

    def __init__(self):
        Module.__init__(self)
        self.args_dict = {}

    def compute(self):
        self.args_dict.update(utils.map_ports(self, self.port_map))
        self.setResult('display_dict', self.args_dict)

class VectorLayer(Module):
    '''Base class for VisTrails modules that represent a geospatial layer
    that can be added to a GeneralSpatialViewerCell as an overlay to a RasterFile
    '''
    _input_ports = [('input_file', '(edu.utah.sci.vistrails.basic:File)', {'optional':True}),
                    ('line_color', '(edu.utah.sci.vistrails.basic:Color)', {'optional':True}),
                    ('fill_color', '(edu.utah.sci.vistrails.basic:Color)', {'optional':True}),
                    ('line_width', '(edu.utah.sci.vistrails.basic:Float)', {'defaults':'["1"]', 'optional':True}),
                    ('alpha', '(edu.utah.sci.vistrails.basic:Float)', {'defaults':'["1.0"]', 'optional':True}),
                    ('query', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'[""]', 'optional':True}),
                    ('draw_order', '(edu.utah.sci.vistrails.basic:Integer)', {'defaults':'["1"]', 'optional':True}), ]
    _output_ports = [('display_dict', '(edu.utah.sci.vistrails.basic:Dictionary)')]

    def __init__(self):
        Module.__init__(self)
        self.port_map = {'input_file':('input_file', utils.get_relative_path, True),  #  These ports are for all Models
                     'line_color':('edgecolor', utils.vt_color_to_tuple, False),
                     'fill_color':('facecolor', utils.vt_color_to_tuple, False),
                     'line_width':('linewidth', None, True),
                     'alpha':('alpha', None, True),
                     'query':('query', None, False),
                     'draw_order':('zorder', None, True),
                    }
        self.args_dict = {}

    def compute(self):
        self.args_dict.update(utils.map_ports(self, self.port_map))
        if self.args_dict.has_key('facecolor'):
            if utilities.approx_equal(self.args_dict['facecolor'][0], 1 / 255.0) and \
                utilities.approx_equal(self.args_dict['facecolor'][1], 2 / 255.0) and \
                utilities.approx_equal(self.args_dict['facecolor'][2], 3 / 255.0):
                self.args_dict['facecolor'] = 'none'
        self.args_dict['zorder'] += 1
        if self.args_dict['zorder'] < 2:
            self.args_dict == 2
        self.setResult('display_dict', self.args_dict)

class PointLayer(VectorLayer):
    __doc__ = GenModDoc.construct_module_doc('PointLayer')

    _input_ports = list(VectorLayer._input_ports)
    _input_ports.extend([('marker', '(edu.utah.sci.vistrails.basic:String)',
                          {'defaults':'["o"]', 'optional':True}),
                         ('markersize', '(edu.utah.sci.vistrails.basic:Float)',
                          {'defaults':'["50.0"]', 'optional':True}), ])

    def __init__(self):
        VectorLayer.__init__(self)
        self.port_map.update({'marker':('marker', None, True),
                              'markersize':('s', None, True)
                         })
        self.port_map['line_color'] = ('edgecolor', utils.vt_color_to_tuple, False)
        self.port_map['line_width'] = ('lw', None, True)

    def compute(self):
        self.args_dict['shapetype'] = "point"
        VectorLayer.compute(self)

class PolyLayer(VectorLayer):
    __doc__ = GenModDoc.construct_module_doc('PolyLayer')

    def compute(self):
        self.args_dict['shapetype'] = "polygon"
        VectorLayer.compute(self)

class LineLayer(VectorLayer):
    '''A vector file with polygon or multi-polygon geometry
    '''
    _input_ports = list(VectorLayer._input_ports)
    _input_ports.remove(('fill_color', '(edu.utah.sci.vistrails.basic:Color)', {'optional':True}))

    def compute(self):
        self.args_dict['shapetype'] = "line"
        VectorLayer.compute(self)

class MyMapCanvas(FigureCanvas):
    '''a hack to get a FigureCanvas not to resize when it's size is 0
    '''
    def __init__(self, fig):
        FigureCanvas.__init__(self, fig)

#    @print_timing
    def resizeEvent(self, event):
        if not event.size().height() == 0:
            FigureCanvas.resizeEvent(self, event)

class RasterDisplay(object):
    '''The idea behind this is from
    http://matplotlib.sourceforge.net/examples/event_handling/viewlims.py
    basically we want to only query as much data as we have screen pixels for.
    When the user zooms, pans, resizes we'll go back to the original display
    and get another set of pixels.

    This object has a pointer to the original raster and functions
    for switching the input file or getting an array of pixel values
    '''
    def __init__(self, width=300, height=300):
        self.height = height
        self.width = width
        self.display_min = None
        self.display_max = None
        self.cmap = None


    def switch_raster(self, raster_kwargs):
        '''A new raster fname was provided
        update the instance variables accordingly
        '''
        self.kwargs = raster_kwargs
        self.raster = SpatialUtilities.SAHMRaster(raster_kwargs['raster_file'])

        self.data_min, self.data_max = SpatialUtilities.get_raster_minmax(raster_kwargs['raster_file'])
        self.display_min, self.display_max = self.data_min, self.data_max

        if not raster_kwargs.get("display_max", 'pullfromraster') == 'pullfromraster':
            self.display_max = raster_kwargs['display_max']

        if not raster_kwargs.get("display_min", 'pullfromraster') == 'pullfromraster':
            self.display_min = raster_kwargs['display_min']

        self.three_band = raster_kwargs.get('three_band', False)

        self.cmap = raster_kwargs.get("cmap", matplotlib.cm.jet)

    def __call__(self, xlim, ylim):
        '''this function is called whenever the view updates
        We pull new pixels from our raster as needed
        '''

        #  reel these values in if they are outside our bounds
        xstart, xend = np.clip(xlim, self.raster.west, self.raster.east)
        ystart, yend = np.clip(ylim, self.raster.south, self.raster.north)

        if (xend - xstart) / self.raster.xScale > self.width:
            x_pixels = self.width
        else:
            x_pixels = None
        if (yend - ystart) / abs(self.raster.yScale) > self.height:
            y_pixels = self.height
        else:
            y_pixels = None

        if self.three_band:
            display_band = [1, 2, 3]
        else:
            display_band = 1

#          print 'get block', xstart, ystart, xend, yend
        ary = self.raster.get_block_bbox([xstart, ystart, xend, yend], x_pixels, y_pixels, display_band)
        if ary is None or ary.size == 1:
#                  print "raster_array is None!!!/n/n"
                return np.empty([1960, 1080])
        else:
                return ary

#    @print_timing
    def setDims(self, ax):
        #  Get the number of points from the number of pixels in the window
        dims = ax.axesPatch.get_window_extent().bounds
        self.width = int(dims[2] + 0.5)
        self.height = int(dims[3] + 0.5)

#    @print_timing
    def ax_update(self, ax):
#          print "\t\t\t\tpp", os.path.split(self.raster.source)[1]
        ax.set_autoscale_on(False)  #  Otherwise, infinite loop
        self.setDims(ax)

        #  Get the range for the new area
        xstart, ystart, xdelta, ydelta = ax.axes.viewLim.bounds
        xend = xstart + xdelta
        yend = ystart + ydelta

        factor = 0.1  #  we want to pull more pixels than we absolutely need
        #  so that we don't get white edges
        xBuff = (xend - xstart) * factor
        yBuff = (yend - ystart) * factor
        xstart = xstart - xBuff
        ystart = ystart - yBuff
        xend = xend + xBuff
        yend = yend + yBuff

        #  reel these values in if they are outside our bounds
        xstart, xend = np.clip([xstart, xend], self.raster.west, self.raster.east)
        ystart, yend = np.clip([ystart, yend], self.raster.south, self.raster.north)

        #  Update the image object with our new data and extent
        im = ax.images[-1]
        im.set_data(self.__call__((xstart, xend), (ystart, yend)))

        xstart, xend, ystart, yend = self.raster.get_bbox_data_bounds([xstart, ystart, xend, yend])

        im.set_extent((xstart, xend, ystart, yend))
        ax.figure.canvas.draw_idle()

def make_categorical_cmap(kwargs):
    uniques = []
    labels = []
    vatdbf = kwargs['raster_file'] + ".vat.dbf"
    if os.path.exists(vatdbf):
        #  we'll pull labels from this file
        f = open(vatdbf, 'rb')
        db = list(dbfreader(f))
        f.close()
        for record in db[2:]:
            uniques.append(record[0])
            labels.append(record[1].strip())
    else:
        raster = SpatialUtilities.SAHMRaster(kwargs['raster_file'])
        vals = raster.getBlock(0, 0, raster.width, raster.height, 2000, 2000)
        uniques = np.unique(vals[vals.mask == False])
        labels = [str(l) for l in uniques]

    cmap = matplotlib.cm.get_cmap(kwargs['cmap'].name, len(uniques))
    kwargs['cbar_ticks'] = uniques
    kwargs['cbar_labels'] = labels
    return uniques, labels, cmap
