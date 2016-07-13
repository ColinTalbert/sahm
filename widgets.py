#  -*- coding: latin-1 -*-
###############################################################################
# This file is part of the Software for Assisted Habitat Modeling (SAHM) package
# developed by the U.S. Geological Survey Fort Collins Science Center.
# It is intended to be used in the VisTrails Scientific
# VisTrails was developed by New York University (2014-2016), NYU-Poly (2011-2014),
# University of Utah (2006-2011).  VisTrails Contact: contact@vistrails.org
#
# SAHM Contact: talbertc@usgs.gov
#
# --------------------------------------------------------------------------------
# U.S. Geological Survey Disclaimers
# Any use of trade, product or firm names is for descriptive purposes only and does
# not imply endorsement by the U.S. Geological Survey.
#
# Although this information product, for the most part, is in the public domain,
# it also contains copyrighted material as noted in the text. Permission to reproduce
# copyrighted items for other than personal use must be secured from the copyright owner.
#
# Although these data have been processed successfully on a computer system at the
# U.S. Geological Survey, no warranty, expressed or implied is made regarding the
# display or utility of the data on any other system, or for general or scientific
# purposes, nor shall the act of distribution constitute any such warranty. The
# U.S. Geological Survey shall not be held liable for improper or incorrect use
# of the data described and/or contained herein.
#
# Although this program has been used by the U.S. Geological Survey (USGS), no
# warranty, expressed or implied, is made by the USGS or the U.S. Government as
# to the accuracy and functioning of the program and related program material nor
# shall the fact of distribution constitute any such warranty, and no responsibility
# is assumed by the USGS in connection therewith.
# --------------------------------------------------------------------------------
#
# This code is in the public domain and is licensed under Creative Commons CC0 1.0 Universal
#
###############################################################################

from PyQt4 import QtCore, QtGui
import os

try:
    from vistrails.gui.modules.module_configure import StandardModuleConfigurationWidget
    from vistrails.gui.modules.constant_configuration import ConstantWidgetMixin
except:
    from core.modules.module_configure import StandardModuleConfigurationWidget
    from core.modules.constant_configuration import ConstantWidgetMixin

class PredictorListWidget(QtGui.QTreeWidget):
    def __init__(self, p_value, available_tree, parent=None):
        QtGui.QTreeWidget.__init__(self, parent)
        self.available_tree = available_tree
        self.setColumnCount(3)
        self.headerItem().setText(0, "Include")
        self.setColumnWidth(0, 300)
        self.headerItem().setText(1, "Layer")
        self.setColumnWidth(1, 300)
        self.headerItem().setText(2, "Categorical")
        self.setColumnWidth(3, 50)

        self.tree_items = {}
        for source, file_list in self.available_tree.iteritems():
            #  print source, file_list
            source_item = QtGui.QTreeWidgetItem([source])
            self.addTopLevelItem(source_item)
            for (file, desc, categorical) in file_list:
                child_item = QtGui.QTreeWidgetItem([file, desc])
                child_item.setFlags(QtCore.Qt.ItemIsUserCheckable |
                                    QtCore.Qt.ItemIsEnabled)
                child_item.setCheckState(0, QtCore.Qt.Unchecked)
                source_item.addChild(child_item)

                catChk = QtGui.QCheckBox(self)

                if categorical == "N":

                    curVal = (file, "Bilinear", "Mean")
                    catChk.setCheckState(QtCore.Qt.Unchecked)
                else:
                    curVal = (file, "NearestNeighbor", "Majority")
                    catChk.setCheckState(QtCore.Qt.Checked)
#
                self.setItemWidget(child_item, 2, catChk)
                self.tree_items[curVal] = child_item
        self.set_values(p_value)

    def set_values(self, str_value):
        values = []
        if str_value:
            values = eval(str_value)
        files = {}
        for k in self.tree_items.iterkeys():
            files[k[0]] = k[1:]
        for value in values:
            if value[0] in files.keys():
                try:
                    oldValue = (value[0], files[value[0]][0], files[value[0]][1])
                    self.tree_items[oldValue].setCheckState(0, QtCore.Qt.Checked)
                    item = self.itemWidget(self.tree_items[oldValue], 2)
                    if value[1] == '1':
                        item.setCheckState(QtCore.Qt.Checked)
                    else:
                        item.setCheckState(QtCore.Qt.Unchecked)
                except ValueError:
                    pass

    def get_values(self):
        values = []
        for value, item in self.tree_items.iteritems():
            #  print value, item
            if item.checkState(0) == QtCore.Qt.Checked:
                if self.itemWidget(item, 2).checkState() == QtCore.Qt.Checked:
                    categorical = '1'
                    resampleMethod = 'NearestNeighbor'
                    aggMethod = 'Majority'
                else:
                    categorical = '0'
                    resampleMethod = 'Bilinear'
                    aggMethod = 'Mean'
                values.append((value[0], categorical, resampleMethod, aggMethod))

        return str(values)

    def select_all(self):
        for value, item in self.tree_items.iteritems():
            self.tree_items[value].setCheckState(0, QtCore.Qt.Checked)

    def switch_selection(self):
        for value, item in self.tree_items.iteritems():
            if item.checkState(0) == QtCore.Qt.Checked:
                self.tree_items[value].setCheckState(0, QtCore.Qt.Unchecked)
            else:
                self.tree_items[value].setCheckState(0, QtCore.Qt.Checked)

    def query_add(self, query_text):
        itemsChecked = 0
        for value, item in self.tree_items.iteritems():
            if str(query_text) in value[0]:
                self.tree_items[value].setCheckState(0, QtCore.Qt.Checked)
                itemsChecked += 1

        msgbox = QtGui.QMessageBox(self)
        msgbox.setText(str(itemsChecked) + " items selected.")
        msgbox.exec_()

    def query_remove(self, query_text):
        itemsUnchecked = 0
        for value, item in self.tree_items.iteritems():
            if str(query_text) in value[0]:
                self.tree_items[value].setCheckState(0, QtCore.Qt.Unchecked)
                itemsUnchecked += 1

        msgbox = QtGui.QMessageBox(self)
        msgbox.setText(str(itemsUnchecked) + " items deselected.")
        msgbox.exec_()

class PredictorListConfigurationWidget(PredictorListWidget,
                                       ConstantWidgetMixin):

    def __init__(self, param, available_tree, parent=None):
        """__init__(param: vistrails.core.vistrail.module_param.ModuleParam,
                    parent: QWidget)

        Initialize the line edit with its contents. Content type is limited
        to 'int', 'float', and 'string'

        """
        PredictorListWidget.__init__(self, param.strValue, available_tree,
                                     parent)
        ConstantWidgetMixin.__init__(self, param.strValue)

        contents = param.strValue
        contentType = param.type

        #  need to deserialize contents and set tree widget accordingly
        #  self.setText(contents)
        self._contentType = contentType

    def contents(self):
        """contents() -> str
        Re-implement this method to make sure that it will return a string
        representation of the value that it will be passed to the module
        As this is a QLineEdit, we just call text()

        """
        return self.get_values()

    def setContents(self, strValue, silent=True):
        """setContents(strValue: str) -> None
        Re-implement this method so the widget can change its value after
        constructed. If silent is False, it will propagate the event back
        to the parent.
        As this is a QLineEdit, we just call setText(strValue)
        """
#         self.setText(strValue)
#         self.update_text()
#         if not silent:
#             self.update_parent()
        self.set_values(strValue)
#         self.update_text()
        if not silent:
            self.update_parent()

    def sizeHint(self):
        return QtCore.QSize(912, 912)

    def minimumSizeHint(self):
        return self.sizeHint()

    ###########################################################################
    #  event handlers

    def focusInEvent(self, event):
        """ focusInEvent(event: QEvent) -> None
        Pass the event to the parent

        """
        self._contents = self.get_values()
        if self.parent():
            QtCore.QCoreApplication.sendEvent(self.parent(), event)
        QtGui.QTreeWidget.focusInEvent(self, event)

    def focusOutEvent(self, event):
        self.update_parent()
        QtGui.QTreeWidget.focusOutEvent(self, event)
        if self.parent():
            QtCore.QCoreApplication.sendEvent(self.parent(), event)

class PredictorListConfiguration(StandardModuleConfigurationWidget):
    #  FIXME add available_dict as parameter to allow config
    def __init__(self, module, controller, available_tree, parent=None):
        StandardModuleConfigurationWidget.__init__(self, module, controller,
                                                   parent)

        #  set title
        if module.has_annotation_with_key('__desc__'):
            label = module.get_annotation_by_key('__desc__').value.strip()
            title = '%s (%s) Module Configuration' % (label, module.name)
        else:
            title = '%s Module Configuration' % module.name
        self.setWindowTitle(title)
        self.build_gui(available_tree)

    def build_gui(self, available_tree):
        layout = QtGui.QVBoxLayout()
        #  precompute tree so we only load once

        #  factor PredictorListConfigurationWidget so that it can be reused in
        #  both cases
        self.p_value = ''
        for function in self.module.functions:
            if function.name == 'value':
                self.p_value = function.parameters[0].strValue
        #  should just be able to pass this across to the PredictorList config
        self.list_config = PredictorListWidget(self.p_value, available_tree)
        layout.addWidget(self.list_config)

        self.buttonLayout = QtGui.QHBoxLayout()
        self.buttonLayout.setMargin(5)
        self.okButton = QtGui.QPushButton('&OK', self)
        self.okButton.setFixedWidth(110)
        self.buttonLayout.addWidget(self.okButton)
        self.cancelButton = QtGui.QPushButton('&Cancel', self)
        self.cancelButton.setShortcut('Esc')
        self.cancelButton.setFixedWidth(110)
        self.buttonLayout.addWidget(self.cancelButton)

        self.selectAllButton = QtGui.QPushButton('&Select All', self)
        self.selectAllButton.setFixedWidth(110)
        self.buttonLayout.addWidget(self.selectAllButton)

        self.switchSelectionButton = QtGui.QPushButton('&Switch Selection', self)
        self.switchSelectionButton.setFixedWidth(110)
        self.buttonLayout.addWidget(self.switchSelectionButton)

        spacerItem = QtGui.QSpacerItem(10, 0, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.buttonLayout.addItem(spacerItem)

        self.queryLabel = QtGui.QLabel("Query")
        self.buttonLayout.addWidget(self.queryLabel)

        self.queryText = QtGui.QLineEdit(self)
        self.queryText.setFixedWidth(110)
        self.buttonLayout.addWidget(self.queryText)

        self.addQuery = QtGui.QPushButton('&Add', self)
        self.addQuery.setFixedWidth(60)
        self.buttonLayout.addWidget(self.addQuery)

        self.removeQuery = QtGui.QPushButton('&Remove', self)
        self.removeQuery.setFixedWidth(60)
        self.buttonLayout.addWidget(self.removeQuery)

        layout.addLayout(self.buttonLayout)
        self.connect(self.okButton, QtCore.SIGNAL('clicked(bool)'),
                     self.okTriggered)
        self.connect(self.cancelButton, QtCore.SIGNAL('clicked(bool)'),
                     self.close)
        self.connect(self.selectAllButton, QtCore.SIGNAL('clicked(bool)'),
                     self.selectAllTriggered)
        self.connect(self.switchSelectionButton, QtCore.SIGNAL('clicked(bool)'),
                     self.switchSelectionTriggered)
        self.connect(self.addQuery, QtCore.SIGNAL('clicked(bool)'),
                     self.queryAdd)
        self.connect(self.removeQuery, QtCore.SIGNAL('clicked(bool)'),
                     self.queryRemove)
        self.setLayout(layout)

    def okTriggered(self):
        str_value = self.list_config.get_values()
        #  print str_value, "=?=", self.p_value
        if str_value != self.p_value:
#            print 'okTriggered:', str_value
            functions = [('value', [str_value])]
            self.controller.update_functions(self.module, functions)
        self.close()

    def selectAllTriggered(self):
        self.list_config.select_all()

    def switchSelectionTriggered(self):
        self.list_config.switch_selection()

    def sizeHint(self):
        return QtCore.QSize(912, 912)

    def queryAdd(self):
        self.list_config.query_add(self.queryText.text())

    def queryRemove(self):
        self.list_config.query_remove(self.queryText.text())

def get_predictor_widget(class_name, tree):
    def __init__(self, param, parent=None):
        PredictorListConfigurationWidget.__init__(self, param, tree, parent)
    class_name += "PredictorListWidget"
    widget_class = type(class_name, (PredictorListConfigurationWidget,),
                        {'__init__': __init__})
    return widget_class

def get_predictor_config(class_name, tree):
    def __init__(self, module, controller, parent=None):
        PredictorListConfiguration.__init__(self, module, controller, tree,
                                            parent)
    class_name += "PredictorListConfig"
    widget_class = type(class_name, (PredictorListConfiguration,),
                        {'__init__': __init__})
    return widget_class
