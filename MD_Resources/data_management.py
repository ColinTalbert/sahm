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
import subprocess
import tempfile
from PyQt4 import QtGui
import config
import zipfile
import requests
import shutil
import lxml.etree as et
import xml_utils
from .. import configuration  # gets configuration from __init__.py from parent directory
import re         # allows for use of regular expressions
import datetime
import pysb
import workflow_utils

sb = pysb.SbSession()


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
    """

    """
    def __init__(self, parent=None):
        super(Login, self).__init__(parent)
        self.setWindowTitle('ScienceBase Login Credentials')
        self.setGeometry(0, 0, 400, 25)
        screen = QtGui.QApplication.desktop().screenNumber(QtGui.QApplication.desktop().cursor().pos())
        center_point = QtGui.QApplication.desktop().screenGeometry(screen).center()
        self.move(center_point)
        self.textName = QtGui.QLineEdit(self)

        self.my_name_label = Buddy_Label(self.textName)  # Create our custom label, and assign myEdit as its buddy
        self.my_name_label.setText('ScienceBase User Name')

        self.textPass = QtGui.QLineEdit(self)
        self.textPass.setEchoMode(QtGui.QLineEdit.Password)
        self.my_pwd_label = Buddy_Label(self.textPass)  # Create our custom label, and assign myEdit as its buddy
        self.my_pwd_label.setText('ScienceBase Password')

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
                self, 'Login Error ', 'Unrecognized User Name or Password for ScienceBase')


class Window(QtGui.QMainWindow):
    def __init__(self, parent=None):
        super(Window, self).__init__(parent)
        # self.ui = Ui_MainWindow()
        # self.ui.setupUi(self)


def workflow_or_history_are_not_specified():
    if workflow_utils.get_current_history_node()[0] == 'ROOT':
        QtGui.QMessageBox.warning(
            None, 'User Warning ', 'A Vistrails Workflow and/or History Node\n'
                                   'must be selected to utilize this functionality')
        return True


def run_metadata_wizard():
    """

    Runs the MetaData Wizard executable with a FGDC metadata template (as determined by the _get_md_template function)
    specifying its output file name based upon the selected workflow history node and FGDC date format
    (ex. Brewers_Sparrow_metadata_YYYYMMDD.xml)

    """

    if workflow_or_history_are_not_specified():
        return None

    curr_session_dir_name = configuration.cur_session_folder

    # TODO:look for preexisting history_name_metadata.xml and confirm they want to use that;
    # if not allow them to select another?
    history_node = workflow_utils.get_current_history_node()[0]
    history_node = _scrub_pep8(history_node)

    # look for existing XML in an archive directory, if not there Create it!
    archive_directory_name = '_archive_' + history_node
    archive_directory_path = os.path.join(curr_session_dir_name, archive_directory_name)
    if not os.path.isdir(archive_directory_path):
        os.makedirs(archive_directory_path)

    # look for existing XML in an archive directory?
    potential_input_file_name = history_node + "_metadata" + ".xml"
    potential_input_file_path = os.path.join(archive_directory_path, potential_input_file_name)
    if os.path.isfile(potential_input_file_path):
        input_file_xml = potential_input_file_path
        out_file_path = input_file_xml
    else:
        input_file_xml = _get_md_template()
        output_file_name = history_node + "_metadata" + ".xml"
        out_file_path = os.path.join(archive_directory_path, output_file_name)

    # Question; Inform/remind the user what history node they have selected and
    # what/where is being created/edited.  allow them to opt out!

    if input_file_xml.strip() == '':
        return False

    print input_file_xml


    # print 'Scrubbed History Node: ', history_node
    #
    # root = utils.getrootdir()
    # print root
    #
    # # ? This may not be an issue as users might not be creating metadata records WITHOUT a history node selected ?
    # if history_node == 'ROOT':
    #     QtGui.QMessageBox.warning(None, "SAHM Workflow Documentation",
    #                               "Please select a workflow history\n" +
    #                               "before creating metadata!!",
    #                               QtGui.QMessageBox.Cancel)
    #     return False

    # assumption - do not include _FGDCdate as the metadata record may be edited many times.....
    # output_file_name = history_node + "_metadata_" + _FGDCdate() + ".xml"

    # get MetadataEditor.exe from relative pathname...
    curr_path = str(os.path.realpath(__file__))
    curr_dir_name = os.path.split(curr_path)[0]
    mde_exe_cmd = os.path.join(curr_dir_name, "MDWizard", "MetadataEditor.exe")

    _launch_metadata_wizard(mde_exe_cmd, input_file_xml, out_file_path)

    return "I have returned from data_management!"


def archive_workflow():
    """
    Creates an '_archive_history' directory in the users session directory containing an 'xml_workflow_document'
    and all of the pipelines necessary component files that will be zipped into a 'vt__archive_history_YYYYMMDD.zip'
    file


     Returns
     -------

    """

    if workflow_or_history_are_not_specified():
        return None

    # assumption get_current_history_node will only ever return on tuple name value pair?
    history_node = workflow_utils.get_current_history_node()[0]

    # assumption do not include FGDC date in archive directory name
    # archive_directory = '_archive_' + _scrub_pep8(history_node) + '_' + _FGDCdate()
    archive_directory = '_archive_' + _scrub_pep8(history_node)

    curr_session_dir_name = configuration.cur_session_folder
    archive_directory_path = os.path.join(curr_session_dir_name, archive_directory)

    print archive_directory_path

    # python views as absolute path
    if not os.path.isdir(archive_directory_path):
        os.mkdir(archive_directory_path)
        print archive_directory_path, 'does not exist, Python will create it!'

    output_workflow_fname = _scrub_pep8(history_node) + '.vt'
    output_workflow_path = os.path.join(archive_directory_path, output_workflow_fname)
    workflow_utils.save_current_history_node_to_vt(output_workflow_path)

    # gather all the workflow files and insert them into the _archive_history_YYYYMMDD directory
    # based upon Colin's function
    vt_input_list_document = _scrub_pep8(history_node) + '.csv'
    vt_input_list_document_path = str(os.path.join(archive_directory_path, vt_input_list_document))
    workflow_utils.create_big_inputs_list(vt_input_list_document_path)

    _copy_vt_files_to_archive_directory(archive_directory_path)

    # zip the contents of the _archive_history_YYYYMMDD directory
    history_node = _scrub_pep8(history_node)
    _create_zip(archive_directory_path, history_node)


    # TODO: delete the *.csv file once it is used to harvest all the input files....


    # # Assumption; it might not be best to delete the archive directory.... discuss with Colin....
    # # Delete the _archive* directory
    # try:
    #     shutil.rmtree(archive_directory_path)
    # except IOError:
    #     print("Error upon either deleting or creating the directory or files.")
    # # Ask the user if they want to push their archive to their ScienceBase account
    # result = QMessageBox.question(
    #     None, 'SAHM Archiving Current Workflow',
    #     "Do you wish to push the " + archive_directory + " file to ScienceBase?",
    #     QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
    #
    # if result == QMessageBox.No:
    #     return False
    #
    # zipfile_name = history_node + _FGDCdate() + '.zip'
    # zipfile_path = os.path.join(archive_directory_path, zipfile_name)
    #
    # # allows user to specify a file and then appends it to a new scienceBase item
    # # This works as well, but again only in debug mode
    # get_sb_credentials()
    # sb.login(config.sb_username, config.sb_password)
    # sb_item_name = 'vt' + archive_directory
    # # Create a new item.  The minimum required is a title for the new item, and the parent ID
    # new_item = {'title': sb_item_name,
    #             'parentId': sb.get_my_items_id(),
    #             'provenance': {'annotation': 'Compressed archive files for VisTrails SAHM workflow '}}
    # new_item = sb.create_item(new_item)
    # print "NEW ITEM: " + str(new_item)
    #
    # new_item = sb.upload_file_to_item(new_item, zipfile_path)
    # print "FILE UPDATE: " + str(new_item)


def upload_archive_to_sciencebase():
    """

    Returns
    -------

    """

    curr_session_dir_name = configuration.cur_session_folder

    # allows user to specify a file and then appends it to a new scienceBase item
    zipfile_path = QtGui.QFileDialog.getOpenFileName(
        None,
        'Choose a SAHM Archive File to Upload to ScienceBase', curr_session_dir_name,
        'vt_archive *.zip;')

    if not zipfile_path:
        return False

    # allow the user to logon to ScienceBase
    get_sb_credentials()
    sb.login(config.sb_username, config.sb_password)

    # parse the zipfile_path
    sb_item_name = os.path.split(zipfile_path)[1]

    # Create a new item.  The minimum required is a title for the new item, and the parent ID
    new_item = {'title': sb_item_name,
                'parentId': sb.get_my_items_id(),
                'provenance': {'annotation': 'Compressed archive files for VisTrails SAHM workflow'}}
    new_item = sb.create_item(new_item)

    sb.upload_file_to_item(new_item, zipfile_path)


def update_metadata_template():
    """

    Returns
    -------

    """

    curr_path = str(os.path.realpath(__file__))
    curr_dir_name = os.path.split(curr_path)[0]
    workspace_path = os.path.join(curr_dir_name, 'MDWizard')
    mde_exe_cmd = os.path.join(curr_dir_name, "MDWizard", "MetadataEditor.exe")

    users_template = configuration.metadata_template

    # look for custom FGDC template XML in the users .vistrails directory,
    # confirm that the user wants to update/replace it......
    if os.path.exists(users_template):

        print 'found it!'

        # ask the user if they want to use their existing FGDC template
        result = QtGui.QMessageBox.question(
            None, 'SAHM Data Management',
            "Do you wish to continue to update " + users_template + "?",
            QtGui.QMessageBox.Yes | QtGui.QMessageBox.No, QtGui.QMessageBox.No)

        if result == QtGui.QMessageBox.Yes:
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
            input_file_xml = generic_fgdc_template
        else:
            input_file_xml = QtGui.QFileDialog.getOpenFileName(
                None,
                'Choose a FGDC Metadata xml file to use as basis for your new custom template ',
                workspace_path, "FGDC Metadata files (*.xml);; All Files (*)")

    print input_file_xml

    if input_file_xml.strip() != '':
        # prompt the user for custom filename to save
        output_filename = _prompt_user_for_default_xml()
        curr_session_dir_name = configuration.cur_session_folder
        out_file_path = os.path.join(curr_session_dir_name, output_filename)
        configuration.set_deep_value('metadata_template', out_file_path)
        print out_file_path
        _launch_metadata_wizard(mde_exe_cmd, input_file_xml, out_file_path)

    return False


def _copy_vt_files_to_archive_directory(archive_directory_path):

    file_and_directory_list = workflow_utils.get_current_copy_list()
    if file_and_directory_list is None:
        return None

    for element in file_and_directory_list:

        input_path = os.path.normcase(element)

        if os.path.isfile(input_path):
            tail = os.path.split(input_path)[1]
            output_path = os.path.join(archive_directory_path, tail)
            shutil.copy(input_path, output_path)
        else:
            # gets rid of the '/*.*' from the input_path
            input_directory_path = os.path.split(input_path)[0]
            directory_name = os.path.basename(input_directory_path)
            output_path = os.path.join(archive_directory_path, directory_name)
            try:
                shutil.copytree(input_directory_path, output_path)
            except OSError as err:
                print("OS error: {0}".format(err))

    # f = open(vt_input_list_document_path, 'r')
    # for line in f:
    #     vt_input_lst = line.split(',')
    #     workflow_name = vt_input_lst[0]
    #     full_path = vt_input_lst[1]
    #     hash = vt_input_lst[2]
    #
    #     if full_path != 'full_path':
    #         full_path = os.path.normcase(full_path)
    #         workflow_path = os.path.split(workflow_name)[0]
    #         file_name = os.path.split(workflow_name)[1]
    #
    #         full_source_path = os.path.join(full_path, file_name)
    #         output_path = os.path.join(archive_directory_path, file_name)
    #
    #         shutil.copy(full_source_path, output_path)
    #         wtf = 'xxxx'

    # output = archive_directory_path


def _prompt_user_for_default_xml():
    """

    Returns
    -------

    """

    output_filename_tuple = QInputDialog.getText(
        None,
        'SAHM custom metadata template creation',
        'Enter a custom filename if you wish:')

    output_filename = output_filename_tuple[0]

    if len(output_filename) > 0:

        output_filename = output_filename.strip()
        # output_filename = output_filename.lower()
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
        user_name = os.environ.get("USERNAME")
        if user_name.strip() != '':
            output_filename = user_name + '_md_template_file.xml'
        else:
            output_filename = 'user_md_template_file.xml'

    return output_filename


def _scrub_pep8(input_string):
    """

    Parameters
    ----------
    input_string

    Returns
    -------
    input_string

    cleans up input string making it suitable for incorporation into a filename, eliminating non-alphanumeric
    characters and replacing spaces with underscores

    """

    input_string = input_string.strip()
    # input_string = input_string.lower()

    # the regular expression replace below will remove '_'
    # This is preventative, spaces will be replaced with '_' at end of function....
    input_string = input_string.replace('_', ' ')

    # use regular expressions to eliminate non alphanumeric characters
    input_string = re.sub(r'([^\s\w]|_)+', '', input_string)
    # replace spaces with underscores....
    input_string = input_string.replace(' ', '_')

    return input_string


def _get_md_template():

    """
    Returns the default metadata template file.  If the user has not saved their own custom metadata
    template in VisTrails configuration.metadata_template the function uses the 'demo_template.xml'
    that ships with VisTrails SAHM..

    returns
    --------
    str md_template as filename

    """

    curr_path = str(os.path.realpath(__file__))
    curr_dir_name = os.path.split(curr_path)[0]
    workspace_path = os.path.join(curr_dir_name, 'MDWizard')

    users_template = configuration.metadata_template

    if os.path.exists(users_template):
        md_template = users_template

    else:
        # the users custom user_MD_template_file.xml has not been found in their user directory
        # use the 'demo_template.xml' that ships with VisTrails SAHM..
        generic_fgdc_template = os.path.join(curr_dir_name, 'MDWizard', 'demo_template.xml')

        if os.path.exists(generic_fgdc_template):
            md_template = generic_fgdc_template
        else:
            # can't find 'demo_template.xml' let the user specify their metadata template
            md_template = QtGui.QFileDialog.getOpenFileName(
                None,
                'Choose a template FGDC Metadata xml file',
                workspace_path,
                'FGDC Metadata files (*.xml);; All Files (*)')

    if md_template.strip() != '':
        return md_template
    else:
        return False


def _launch_metadata_wizard(cmd, input_filename, output_filename, async=False):
    """

    Parameters
    ----------
    cmd - {installation path}\\MDResources\\MDWizard\\MetadataEditor.exe
    input_filename - users custom template saved in their session folder OR {installation path}\\MDResources\\MDWizard\\demo_template.xml
    output_filename
    async

    Returns
    -------

    """

    #  open the text files we'll be writing our stdOut and stdErr to
    f = tempfile.NamedTemporaryFile(delete=False)
    fname = f.name
    f.close()
    stderr_fname = fname + "stderr.txt"

    std_err_file = open(stderr_fname, 'a')
    std_err_file.seek(0, os.SEEK_END)

    mde_exe_cmd = cmd
    mde_exe_cmd += " " + input_filename
    mde_exe_cmd += " " + output_filename

    p = subprocess.Popen(mde_exe_cmd)
    if not async:
        p.wait()

    std_err_file.close()
    err_msg = "\n".join(open(stderr_fname, "r").readlines())
    # out_msg = "\n".join(open(stdout_fname, "r").readlines())
    return err_msg


def get_contact():
    """

    Returns
    -------

    """
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
    """

    Returns
    -------

    """

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


def _create_zip(archive_directory, history_node):
    """

    Returns
    -------

    """
    zipfile_name = 'vt_archive_' + history_node + _FGDCdate() + '.zip'
    curr_session_dir_name = configuration.cur_session_folder
    zipfile_path = os.path.join(curr_session_dir_name, zipfile_name)
    zipf = zipfile.ZipFile(zipfile_path, 'w', zipfile.ZIP_DEFLATED)
    _zipdir(archive_directory, zipf)
    zipf.close()


def _zipdir(path, ziph):
    """

    Parameters
    ----------
    path
    ziph

    Returns
    -------

    """
    # ziph is zipfile handle
    for root, dirs, files in os.walk(path):
        my_root = root
        my_dirs = dirs
        for file in files:
            ziph.write(os.path.join(root, file))


def _FGDCdate():
    """
    Returns current date as a string in YYYYMMDD format
    """
    now = datetime.datetime.now()
    date_string = str(now.strftime('%Y%m%d'))
    return date_string


class Vividict(dict):
    """
        This class may be used to create nested dictionaries in the future
    """
    def __missing__(self, key):
        value = self[key] = type(self)()
        return value


def xml_object_editing():
    """
        holding spot for calls to editing functions in xml_utils.py

    """

    xml_utils.remove_node_by_name(xml_input=etree, xpath='cntperp/cntorg')

    xml_utils.change_xml_node_text(xml_input=etree, xpath='cntperp/cntorg',
                                   new_node_text='A Undisclosed Location for ExPat Entomological Developers..',
                                   add_if_missing=True)

    xml_utils.replace_xml_node_contents(xml_input=etree, xpath='cntperp/cntorg',
                                        new_node_contents='<samplenode><innernode><pocketcontent1>PC1</pocketcontent1><pocketcontent2>PC2</pocketcontent2></innernode></samplenode>',
                                        add_if_missing=True)
