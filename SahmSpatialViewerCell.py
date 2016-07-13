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

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_Frame(object):
    def setupUi(self, Frame):
        Frame.setObjectName(_fromUtf8("Frame"))
        Frame.resize(734, 706)
        Frame.setWindowTitle(QtGui.QApplication.translate("Frame", "Frame", None, QtGui.QApplication.UnicodeUTF8))
        Frame.setFrameShape(QtGui.QFrame.StyledPanel)
        Frame.setFrameShadow(QtGui.QFrame.Plain)
        self.gridLayout = QtGui.QGridLayout(Frame)
        self.gridLayout.setMargin(0)
        self.gridLayout.setSpacing(0)
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.splitter = QtGui.QSplitter(Frame)
        self.splitter.setOrientation(QtCore.Qt.Vertical)
        self.splitter.setObjectName(_fromUtf8("splitter"))
        self.legend_frame_2 = QtGui.QFrame(self.splitter)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Preferred)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.legend_frame_2.sizePolicy().hasHeightForWidth())
        self.legend_frame_2.setSizePolicy(sizePolicy)
        self.legend_frame_2.setMinimumSize(QtCore.QSize(0, 0))
        self.legend_frame_2.setBaseSize(QtCore.QSize(0, 0))
        self.legend_frame_2.setFrameShape(QtGui.QFrame.StyledPanel)
        self.legend_frame_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.legend_frame_2.setObjectName(_fromUtf8("legend_frame_2"))
        self.horizontalLayout_2 = QtGui.QHBoxLayout(self.legend_frame_2)
        self.horizontalLayout_2.setSpacing(5)
        self.horizontalLayout_2.setContentsMargins(9, 0, 0, 0)
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.legend_label = QtGui.QLabel(self.legend_frame_2)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Minimum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.legend_label.sizePolicy().hasHeightForWidth())
        self.legend_label.setSizePolicy(sizePolicy)
        self.legend_label.setText(QtGui.QApplication.translate("Frame", "TextLabel", None, QtGui.QApplication.UnicodeUTF8))
        self.legend_label.setObjectName(_fromUtf8("legend_label"))
        self.horizontalLayout_2.addWidget(self.legend_label)
        self.legend = QtGui.QFrame(self.legend_frame_2)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.legend.sizePolicy().hasHeightForWidth())
        self.legend.setSizePolicy(sizePolicy)
        self.legend.setFrameShape(QtGui.QFrame.NoFrame)
        self.legend.setFrameShadow(QtGui.QFrame.Plain)
        self.legend.setLineWidth(0)
        self.legend.setObjectName(_fromUtf8("legend"))
        self.legend_layout = QtGui.QHBoxLayout(self.legend)
        self.legend_layout.setMargin(0)
        self.legend_layout.setObjectName(_fromUtf8("legend_layout"))
        self.horizontalLayout_2.addWidget(self.legend)
        
        self.map_frame = QtGui.QFrame(self.splitter)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Preferred, QtGui.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(1)
        sizePolicy.setHeightForWidth(self.map_frame.sizePolicy().hasHeightForWidth())
        self.map_frame.setSizePolicy(sizePolicy)
        self.map_frame.setFrameShape(QtGui.QFrame.StyledPanel)
        self.map_frame.setFrameShadow(QtGui.QFrame.Sunken)
        self.map_frame.setObjectName(_fromUtf8("map_frame"))
        self.horizontalLayout = QtGui.QHBoxLayout(self.map_frame)
        self.horizontalLayout.setSpacing(0)
        self.horizontalLayout.setMargin(0)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.gridLayout.addWidget(self.splitter, 0, 0, 1, 1)

        self.retranslateUi(Frame)
        QtCore.QMetaObject.connectSlotsByName(Frame)

    def retranslateUi(self, Frame):
        pass

