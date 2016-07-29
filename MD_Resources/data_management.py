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

import os
import sys
import subprocess
import tempfile
import pysb
from PyQt4 import QtGui
from PyQt4 import QtCore
import config
import zipfile
import requests
import shutil
import lxml.etree as et
from PyQt4.QtGui import *
import xml_utils
from os.path import expanduser
import utils
from .. import *  # gets configuration from __init__.py from parent directory
import re         # allows for use of regular expressions


def get_fname(parent=None):
    """
    Handler called when 'choose file' is clicked
    """

    fname = QtGui.QFileDialog.getOpenFileName(parent, 'Choose a FGDC Metadata xml file', '../../../../',
                                              "FGDC Metadata files (*.xml);; All Files (*)")
    if fname:
        return str(fname)


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


def update_metadata_template():

    curr_path = str(os.path.realpath(__file__))
    curr_dir_name = os.path.split(curr_path)[0]
    workspace_path = os.path.join(curr_dir_name, 'MDWizard')
    mde_exe_cmd = os.path.join(curr_dir_name, "MDWizard", "MetadataEditor.exe")

    users_template = configuration.metadata_template

    # look for custom FGDC template XML in the users .vistrails directory,
    # confirm that the user wants to update/replace it......
    if os.path.exists(users_template):

        print 'found it!'
        # TODO: see if Colin wants file details in the PyQT4 message box

        # ask the user if they want to use their existing FGDC template
        result = QMessageBox.question(
            None, 'SAHM Data Management',
            "Do you wish to continue to update " + users_template + "?",
            QMessageBox.Yes | QMessageBox.No, QMessageBox.No)

        if result == QMessageBox.Yes:
            print 'Yes.'
            input_file_xml = users_template
        else:
            print 'No.'
            input_file_xml = QtGui.QFileDialog.getOpenFileName(
                None,
                'Choose a FGDC Metadata xml file to use as basis for your new custom template ',
                workspace_path, "FGDC Metadata files (*.xml);; All Files (*)")

    else:
        # the users custom user_MD_template_file.xml has not been found in their user directory
        generic_fgdc_template = curr_path.replace("data_management.py", "MDWizard\demo_template.xml")

        if os.path.exists(generic_fgdc_template):
            result = QMessageBox.question(
                None, 'SAHM Data Management', "SAHM has detected " + generic_fgdc_template +
                "\n Would you like to use it as the basis for your new custom template?",
                QMessageBox.Yes | QMessageBox.No, QMessageBox.No)

            if result == QMessageBox.Yes:
                print 'Yes.'
                input_file_xml = generic_fgdc_template
            else:
                print 'No.'
                input_file_xml = QtGui.QFileDialog.getOpenFileName(
                    None,
                    'Choose a FGDC Metadata xml file to use as basis for your new custom template ',
                    workspace_path, "FGDC Metadata files (*.xml);; All Files (*)")

    print input_file_xml
    if input_file_xml.strip() != '':

        # prompt the user for custom filename to save

        output_filename = prompt_user_for_default_xml()
        out_file_path = os.path.join(curr_dir_name, "MDWizard", output_filename)
        configuration.set_deep_value('metadata_template', out_file_path)
        print out_file_path
        launch_metadatawizard(mde_exe_cmd, input_file_xml, out_file_path)

    return False


def prompt_user_for_default_xml():

    output_filename_tuple = QInputDialog.getText(
        None,
        'SAHM custom metadata template creation',
        'Enter a custom filename if you wish:')

    output_filename = output_filename_tuple[0]

    if len(output_filename) > 0:

        # assure user template filename is snake case with a .xml file extension
        output_filename = output_filename.strip()
        output_filename = output_filename.lower()
        # the regular expression replace below will remove '_'
        # This is preventative, spaces will be replaced with '_' at end of function....
        output_filename = output_filename.replace('_', ' ')
        # the regular expression replace below will remove '.'
        # This is preventative, file extension will be added at end of function....
        output_filename = output_filename.replace('.xml', '')

        # use regular expressions to eliminate non alphanumeric characters
        output_filename = re.sub(r'([^\s\w]|_)+', '', output_filename)
        # replace spaces with underscores....
        output_filename = output_filename.replace(' ', '_')

        output_filename += '.xml'
    else:

        # if user fails to specify a valid template filename......
        output_filename = 'user_md_template_file.xml'

    return output_filename


def scrub_pep8(input_string):

    input_string = input_string.strip()
    input_string = input_string.lower()

    # the regular expression replace below will remove '_'
    # This is preventative, spaces will be replaced with '_' at end of function....
    input_string = input_string.replace('_', ' ')

    # use regular expressions to eliminate non alphanumeric characters
    input_string = re.sub(r'([^\s\w]|_)+', '', input_string)
    # replace spaces with underscores....
    input_string = input_string.replace(' ', '_')

    return input_string


def get_md_template():
    """
    return the default metadata template file
    TODO: make this a per user variable so that users can customize their starting template,  store in config???

    :return:
    str filename
    """
    # global session_dir
    curr_path = str(os.path.realpath(__file__))
    curr_dir_name = os.path.split(curr_path)[0]
    workspace_path = os.path.join(curr_dir_name, 'MDWizard')
    # user_md_template_file = os.path.join(curr_dir_name, 'MDWizard', 'user_MD_template_file.xml')

    users_template = configuration.metadata_template

    if os.path.exists(users_template):

        print 'found it!'
        input_file_xml = users_template

    else:
        # the users custom user_MD_template_file.xml has not been found in their user directory
        generic_fgdc_template = os.path.join(curr_dir_name, 'MDWizard', 'demo_template.xml')

        if os.path.exists(generic_fgdc_template):
            result = QMessageBox.question(None, 'SAHM Data Management',
                                          'You don\'t appear to have a custom template saved. \n' +
                                          'SAHM has detected ' + generic_fgdc_template +
                                          '\n Would you like to use it?', QMessageBox.Yes | QMessageBox.No, QMessageBox.No)

            if result == QMessageBox.Yes:
                print 'Yes.'
                input_file_xml = generic_fgdc_template
            else:
                print 'No.'
                input_file_xml = QtGui.QFileDialog.getOpenFileName(None, 'Choose a template FGDC Metadata xml file',
                                                                   workspace_path,
                                                                   "FGDC Metadata files (*.xml);; All Files (*)")

    if input_file_xml.strip() != '':
        return input_file_xml
    else:
        return False


def run_metadata_wizard():

    # get MetadataEditor.exe from relative pathname...
    # print "this is my path: " + str(os.path.realpath(__file__))
    curr_path = str(os.path.realpath(__file__))
    curr_dir_name = os.path.split(curr_path)[0]
    mde_exe_cmd = os.path.join(curr_dir_name, "MDWizard", "MetadataEditor.exe")

    input_file_xml = get_md_template()

    if input_file_xml.strip() == '':
        return False

    print input_file_xml

    # TODO: ask Colin if Root Directory should have anything to do with output XML filename.....
    # root_directory = utils.getrootdir()
    # print 'Root Directory: ', root_directory

    history_node = utils.get_current_history_node_name()

    print 'Original History Node: ', history_node
    history_node = scrub_pep8(history_node)
    print 'Scrubbed History Node: ', history_node

    # ? This may not be an issue as users might not be creating metadata records WITHOUT a history node selected ?
    if history_node == 'root':
        history_node = 'mdwiz'

    out_file_path = input_file_xml.replace(".xml", "_" + history_node + ".xml")
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


def get_contact():
    try:
        user_name = os.environ.get("USERNAME")

        # These are to create errors.....
        # user_name = ''
        response = requests.get("http://geo-nsdi.er.usgs.gov/contact-xml.php?email=" + user_name)

        # These are to create errors.....
        # response = requests.get("xyz://geo-nsdi.er.usgs.gov/contact-xml.php?email=" + user_name)

        if response is NotImplementedError:
            print
            print "Something went horribly wrong.\n\n"
            return False

        print response.text
        print
        etree = et.fromstring(response.content)  # this is type lxml.etree._Element
        # etype = type(etree)
        # print etype
        # print
        # print et.tostring(etree, pretty_print=True)
        # print

        # this will step thru all the nodes looking for only the cntper (name) node
        for node in etree.iter('cntper'):
            if node.text.strip() != '':
                print 'This is my name: ', node.text
            else:
                # TODO: get Colin's feedback on best error trap method
                print
                print "Unable to verify the user is a current USGS employee or affiliate.\n\n"
                return False

        return etree
        # for page in list(etree):
        #     print 'Elements of the XML: ' + str(page)
        # print

        # #  TODO: get Colin's feedback on best way to create nested dictionary
        # # This will step thru all the nodes and create a flat dictionary of the contact info
        # # and print it's contents
        # flat_contact_dictionary = {}
        # for node in etree.iter():
        #     flat_contact_dictionary[node.tag] = node.text
        # print
        # print'flat_contact_dictionary = ', flat_contact_dictionary
        #
        # print
        # # # This will step thru all the nodes and print the node tags and node texts
        # for node in etree.iter():
        #     if node.text is not None:
        #         print 'node.tag: ' + node.tag + '  node.text: ' + node.text

    except OSError as err:
        print("OS error: {0}".format(err))
    except:  # TODO: get Colin's feedback on best error trap method
        print("Unexpected error:", sys.exc_info()[0])
        print
        print "Something went horribly wrong.\n\n"


def get_sb_credentials():

    if config.sb_username.strip() == '' or config.sb_password.strip() == '':
        login = Login()

        if login.exec_() == QtGui.QDialog.Accepted:
            window = Window()
            window.show()
            window.focusWidget()

    else:
        print "We already have username and password! \n" \
              "Username : " + config.sb_username + "\n"

    username_password = {'username': config.sb_username, 'password': config.sb_password}
    return username_password


def get_sb_item():
    sb = pysb.SbSession()
    #
    # # Get a public item.  No need to log in.
    item_json = sb.get_item('505bc673e4b08c986b32bf81')
    print "Public Item: " + str(item_json)
    return "I have returned from ScienceBase!"


def create_zip():

    current_dir = os.path.dirname(os.path.realpath(__file__))
    os_walk = os.walk
    dir_to_zip = current_dir + '\\tmp'
    zipf = zipfile.ZipFile('Python.zip', 'w', zipfile.ZIP_DEFLATED)
    zipdir(dir_to_zip, zipf)
    zipf.close()


def zipdir(path, ziph):
    # ziph is zipfile handle
    for root, dirs, files in os.walk(path):
        my_root = root
        my_dirs = dirs
        for file in files:
            ziph.write(os.path.join(root, file))


def get_contact():
    try:
        user_name = os.environ.get("USERNAME")

        # These are to create errors.....
        # user_name = ''
        response = requests.get("http://geo-nsdi.er.usgs.gov/contact-xml.php?email=" + user_name)

        # These are to create errors.....
        # response = requests.get("xyz://geo-nsdi.er.usgs.gov/contact-xml.php?email=" + user_name)

        if response is NotImplementedError:
            print
            print "Something went horribly wrong.\n\n"
            return False

        print response.text
        print
        etree = et.fromstring(response.content)  # this is type lxml.etree._Element
        # etype = type(etree)
        # print etype
        # print
        # print et.tostring(etree, pretty_print=True)
        # print

        # this will step thru all the nodes looking for only the cntper (name) node
        for node in etree.iter('cntper'):
            if node.text.strip() != '':
                print 'This is my name: ', node.text
            else:
                print
                print "Unable to verify the user is a current USGS employee or affiliate.\n\n"
                return False

        return etree
        # for page in list(etree):
        #     print 'Elements of the XML: ' + str(page)
        # print

        # # This will step thru all the nodes and create a flat dictionary of the contact info
        # # and print it's contents
        # flat_contact_dictionary = {}
        # for node in etree.iter():
        #     flat_contact_dictionary[node.tag] = node.text
        # print
        # print'flat_contact_dictionary = ', flat_contact_dictionary
        #
        # print
        # # # This will step thru all the nodes and print the node tags and node texts
        # for node in etree.iter():
        #     if node.text is not None:
        #         print 'node.tag: ' + node.tag + '  node.text: ' + node.text
        #

    except OSError as err:
        print("OS error: {0}".format(err))
    except:  # TODO: get Colin's feedback on best error trap method
        print("Unexpected error:", sys.exc_info()[0])
        print
        print "Something went horribly wrong.\n\n"


# This class may be used to create nested dictionaries in the future

# nested_contact_dictionary = {}
# for node in etree.iter():
#     if node.text is not None:
#         nested_contact_dictionary[node.tag] = node.text
#     else:
#
# print
# print 'nested_contact_dictionary = ', nested_contact_dictionary
class Vividict(dict):
    def __missing__(self, key):
        value = self[key] = type(self)()
        return value


# useful dialog box TODO: parameterize it for ease of use....
def show_dialog():
    msg = QMessageBox()
    msg.setIcon(QMessageBox.Information)

    msg.setText("This is a message box")
    msg.setInformativeText("This is additional information")
    msg.setWindowTitle("MessageBox demo")
    msg.setDetailedText("The details are as follows:")
    msg.setStandardButtons(QMessageBox.Ok | QMessageBox.Cancel)
    # msg.buttonClicked.connect(msgbtn)

    retval = msg.exec_()
    print "value of pressed message box button:", retval


# holding spot for calls to editing functions in xml_utils.py
def xml_object_editing():

    xml_utils.remove_node_by_name(xml_input=etree, xpath='cntperp/cntorg')

    xml_utils.change_xml_node_text(xml_input=etree, xpath='cntperp/cntorg',
                                   new_node_text='A Undisclosed Location for ExPat Entomological Developers..',
                                   add_if_missing=True)

    xml_utils.replace_xml_node_contents(xml_input=etree, xpath='cntperp/cntorg',
                                        new_node_contents='<samplenode><innernode><pocketcontent1>PC1</pocketcontent1><pocketcontent2>PC2</pocketcontent2></innernode></samplenode>',
                                        add_if_missing=True)
