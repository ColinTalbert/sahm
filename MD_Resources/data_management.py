###############################################################################
##
# Copyright (C) 2010-2012, USGS Fort Collins Science Center.
# All rights reserved.
# Contact: talbertc@usgs.gov
##
# This file is part of the Software for Assisted Habitat Modeling package
# for VisTrails.
##
# "Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
##
#  - Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#  - Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#  - Neither the name of the University of Utah nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
#
# Although this program has been used by the U.S. Geological Survey (USGS),
# no warranty, expressed or implied, is made by the USGS or the
# U.S. Government as to the accuracy and functioning of the program and
# related program material nor shall the fact of distribution constitute
# any such warranty, and no responsibility is assumed by the USGS
# in connection therewith.
#
# Any use of trade, firm, or product names is for descriptive purposes only
# and does not imply endorsement by the U.S. Government.
###############################################################################
#
import Tkinter as tk
import os
import subprocess
import tempfile
import tkFileDialog

import pysb
from PyQt4 import QtGui
from PyQt4 import QtCore
import config
import sys


# class FilePicker(QtGui.QWidget):
#     """
#     An example file picker application
#     """
#
#     def __init__(self):
#         # create GUI
#         QtGui.QMainWindow.__init__(self)
#         self.setWindowTitle('File picker')
#         # Set the window dimensions
#         self.resize(300, 75)
#
#         # vertical layout for widgets
#         self.vbox = QtGui.QVBoxLayout()
#         self.setLayout(self.vbox)
#
#         # Create a label which displays the path to our chosen file
#         self.lbl = QtGui.QLabel('No file selected')
#         self.vbox.addWidget(self.lbl)
#
#         # Create a push button labelled 'choose' and add it to our layout
#         btn = QtGui.QPushButton('Choose a Meta Data XML file1', self)
#         self.vbox.addWidget(btn)
#
#         # Connect the clicked signal to the get_fname handler
#         self.connect(btn, QtCore.SIGNAL('clicked()'), self.get_fname)
#
#     def get_fname(self):
#         """
#         Handler called when 'choose file' is clicked
#         """
#         # When you call getOpenFileName, a file picker dialog is created
#         # and if the user selects a file, it's path is returned, and if not
#         # (ie, the user cancels the operation) None is returned
#         fname = QtGui.QFileDialog.getOpenFileName(self, 'Choose a Meta Data XML file2')
#         if fname:
#             self.lbl.setText(fname)
#             return str(fname)
#         else:
#             self.lbl.setText('No file selected')


def get_fname(parent=None):
    """
    Handler called when 'choose file' is clicked
    """
    # When you call getOpenFileName, a file picker dialog is created
    # and if the user selects a file, it's path is returned, and if not
    # (ie, the user cancels the operation) None is returned
    fname = QtGui.QFileDialog.getOpenFileName(parent, 'Choose a Meta Data XML file2')
    if fname:
        return str(fname)


def get_md_template():
    """
    return the default metadata template file
    TODO: make this a per user variable so that users can customize their starting template,  store in config???

    :return:
    str filename
    """
    cur_dname = os.path.split(os.path.realpath(__file__))[0]
    template_fname = os.path.join(cur_dname, "MDWizard", "demo_template.xml")
    return template_fname


class Buddy_Label(QtGui.QLabel):
    def __init__(self, buddy, parent=None):
        super(Buddy_Label, self).__init__(parent)
        self.buddy = buddy


class Login(QtGui.QDialog):
    def __init__(self, parent=None):
        super(Login, self).__init__(parent)
        self.setWindowTitle('ScienceBase Login Information')
        self.setGeometry(0, 0, 400, 25)
        screen = QtGui.QApplication.desktop().screenNumber(QtGui.QApplication.desktop().cursor().pos())
        center_point = QtGui.QApplication.desktop().screenGeometry(screen).center()
        self.move(center_point)
        self.textName = QtGui.QLineEdit(self)

        self.my_name_label = Buddy_Label(self.textName)  # Create our custom label, and assign myEdit as its buddy
        self.my_name_label.setText('User Name')

        self.textPass = QtGui.QLineEdit(self)
        self.textPass.setEchoMode(QtGui.QLineEdit.Password)
        self.my_pwd_label = Buddy_Label(self.textPass)  # Create our custom label, and assign myEdit as its buddy
        self.my_pwd_label.setText('Password')

        self.buttonLogin = QtGui.QPushButton('Enter', self)
        self.buttonLogin.clicked.connect(self.handle_login)
        self.buttonLogin.setFixedWidth(50)

        # horizontal dialog box
        # layout = QtGui.QHBoxLayout(self)

        # vertical dialog box
        layout = QtGui.QVBoxLayout(self)
        layout.addWidget(self.my_name_label)
        layout.addWidget(self.textName)
        layout.addWidget(self.my_pwd_label)
        layout.addWidget(self.textPass)

        toolbar = QtGui.QToolBar()
        left_spacer = QtGui.QWidget()
        left_spacer.setSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Expanding)
        toolbar.addWidget(left_spacer)
        toolbar.addWidget(self.buttonLogin)
        right_spacer = QtGui.QWidget()
        right_spacer.setSizePolicy(QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Expanding)
        toolbar.addWidget(right_spacer)

        layout.addWidget(toolbar)

    def handle_login(self):
        # make sure the username and password are not just blanks or tabs
        if self.textName.text().strip() != '' and self.textPass.text().strip() != '':
            config.sb_username = self.textName.text()
            config.sb_password = self.textPass.text()
            self.accept()
        else:
            QtGui.QMessageBox.warning(
                self, 'Error', 'Bad user or password')


class Window(QtGui.QMainWindow):
    def __init__(self, parent=None):
        super(Window, self).__init__(parent)
        # self.ui = Ui_MainWindow()
        # self.ui.setupUi(self)


def get_sb_item():
    sb = pysb.SbSession()
    #
    # # Get a public item.  No need to log in.
    item_json = sb.get_item('505bc673e4b08c986b32bf81')
    print "Public Item: " + str(item_json)
    return "I have returned from ScienceBase!"


def data_management():

    # get MetadataEditor.exe from relative pathname...
    # print "this is my path: " + str(os.path.realpath(__file__))
    curr_path = str(os.path.realpath(__file__))
    mde_exe_cmd = curr_path.replace("data_management.py", "MDWizard\MetadataEditor.exe")

    # ctypes.windll.user32.MessageBoxA(0, "MetadataEditor.exe lives here: \n" +
    #                                  mde_exe_cmd, "MD_Resources Module - data_management",  1)

    # get metadata xml record from file browser
    # root = tk.Tk()
    # root.withdraw()
    # root.filename = tkFileDialog.askopenfilename(initialdir="/Development/SupportFiles", title="Select MetaData XML file",
    #                                              filetypes=(("all files", "*.*"), ("MetaData files", "*.xml *.XML")))

    # root.filename = Example()
    #
    # input_file_xml = str(root.filename)

    # file_picker_gui = FilePicker()
    # file_picker_gui.show()

    # input_file_xml = get_fname()
    input_file_xml = get_md_template()
    # ctypes.windll.user32.MessageBoxA(0, "Input File lives here: \n" +
    #                                  input_file_xml, "MD_Resources Module - data_management",  1)

    # generate output XML filename based upon user input
    out_file_path = input_file_xml.replace(".xml", "_mdwiz.xml")
    # ctypes.windll.user32.MessageBoxA(0, "Output File will go here: \n" +
    #                                  out_file_path, "MD_Resources Module - data_management",  1)

    launch_metadatawizard(mde_exe_cmd, input_file_xml, out_file_path)

    return "I have returned from data_management!"


def launch_metadatawizard(cmd, stdin_fname, stdout_fname, async=False):

    #  open the text files we'll be writing our stdOut and stdErr to
    f = tempfile.NamedTemporaryFile(delete=False)
    fname = f.name
    f.close()
    stderr_fname = fname + "stderr.txt"

    std_err_file = open(stderr_fname, 'a')
    std_err_file.seek(0, os.SEEK_END)

    mde_exe_cmd = cmd
    mde_exe_cmd += " " + stdin_fname
    mde_exe_cmd += " " + stdout_fname

    p = subprocess.Popen(mde_exe_cmd)
    if not async:
        p.wait()

    std_err_file.close()
    err_msg = "\n".join(open(stderr_fname, "r").readlines())
    out_msg = "\n".join(open(stdout_fname, "r").readlines())
    return err_msg, out_msg


def get_sb_credentials():

    if config.sb_username.strip() == '' or config.sb_password.strip() == '':
        login = Login()

        if login.exec_() == QtGui.QDialog.Accepted:
            window = Window()
            window.show()
            window.focusWidget()

    else:
        print "We already have username and password! \n" \
              "Username : " + config.sb_username + "\n" \
              "Password : " + config.sb_password

    username_password = {'username': config.sb_username, 'password': config.sb_password}
    return username_password
