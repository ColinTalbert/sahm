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
import sys

try:
    from vistrails.core import system
except ImportError:
    from core import system

try:
    from vistrails.core.configuration import ConfigurationObject
except ImportError:
    from core.configuration import ConfigurationObject

name = "SAHM"
identifier = "gov.usgs.sahm"
version = '1.2.0'

def package_dependencies():
    try:
        import vistrails.core.packagemanager
        manager = vistrails.core.packagemanager.get_package_manager()
    except ImportError:
        import core.packagemanager
        manager = core.packagemanager.get_package_manager()


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
                            r_path=r'..\\Central_R\R-3.0.1\bin',
                            maxent_path=r'Not Set',
                            cur_session_folder=r"C:\temp\SAHM_workspace",
                            cur_processing_mode="single models sequentially (n - 1 cores each)",
                            verbose='True')
else:
    configuration = \
        ConfigurationObject(output_dir=r'C:\temp\SAHM_workspace',
                            r_path=r'Not Set',
                            maxent_path=r'Not Set',
                            cur_session_folder=r"C:\temp\SAHM_workspace",
                            cur_processing_mode="single models sequentially (n - 1 cores each)",
                            verbose='True')
