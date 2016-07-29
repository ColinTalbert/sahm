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
import sys

from vistrails.core import system
from vistrails.core.configuration import ConfigurationObject


name = "SAHM"
identifier = "gov.usgs.sahm"
version = '2.0.0'

def package_dependencies():
    import vistrails.core.packagemanager
    manager = vistrails.core.packagemanager.get_package_manager()

    if manager.has_package('org.vistrails.vistrails.spreadsheet'):
        return ['org.vistrails.vistrails.spreadsheet']
    else:
        return []

def package_requirements():
    try:
        from vistrails.core.requirements import python_module_exists, MissingRequirement
    except ImportError:
        from core.requirements import python_module_exists, MissingRequirement

    if not python_module_exists('matplotlib'):
        raise MissingRequirement('matplotlib')
    if not python_module_exists('fiona'):
        raise MissingRequirement('fiona')
    if not python_module_exists('numpy'):
        raise MissingRequirement('numpy')
    if not python_module_exists('osgeo'):
        raise MissingRequirement('osgeo')
    if not python_module_exists('shapely'):
        raise MissingRequirement('shapely')
    if not python_module_exists('pyproj'):
        raise MissingRequirement('pyproj')
    if not python_module_exists('scipy'):
        raise MissingRequirement('scipy')

if system.systemType in ['Microsoft', 'Windows']:
    #  on Windows the default location of these is relative to the python.exe
    pyloc = sys.executable
    configuration = \
        ConfigurationObject(output_dir=r'C:\temp\SAHM_workspace',
                            r_path=r'Central_R\R-3.2.0\bin',
                            maxent_path=r'Not Set',
                            java_path=r'java',
                            cur_session_folder=r"C:\temp\SAHM_workspace",
                            cur_processing_mode="multiple models simultaneously (1 core each)",
                            default_seed='1234',
                            verbose='True',
                            metadata_template=r"")
else:
    configuration = \
        ConfigurationObject(output_dir=r'C:\temp\SAHM_workspace',
                            r_path=r'Not Set',
                            maxent_path=r'Not Set',
                            java_path=r'java',
                            cur_session_folder=r"C:\temp\SAHM_workspace",
                            cur_processing_mode="multiple models simultaneously (1 core each)",
                            default_seed='1234',
                            verbose='True',
                            metadata_template=r"")
