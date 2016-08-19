import lxml.etree as et
import lxml
from PyQt4 import QtGui


def remove_node_by_name(xml_input=None, xpath=None):
    '''
    Will remove all instances of a node by a certain 'name' in an xml document.
    See example at: http://drumcoder.co.uk/blog/2010/jun/17/using-elementtree-python/
    The method used to remove elements in the other routines (WriteEAInfo, WriteSpatialRefInfo, WriteSpatialDataOrgInfo) will remove just the first found instance.
    (This code works based on this import construct: "import xml.etree.ElementTree as ET")
    '''
    type_str = type(xml_input)
    print(type_str)

    if type(xml_input) is lxml.etree._Element:
        etree = xml_input
    else:
        # xml is a file path
        etree = et.ElementTree()
        etree.parse(xml_input)

    for bad in etree.xpath(xpath):
        bad.getparent().remove(bad)

    print et.tostring(etree, pretty_print=True)
    return etree


# Will gut and replace a node with new, nested XML contents.
def replace_xml_node_contents(xml_input=None, xpath=None, new_node_contents=None, add_if_missing=True):
    '''
    Inputs:
    1. Full path to XML file to modify.
    2. The path to the node whose contents will be replaced. Skip root element in path (e.g. "idinfo/citation" or "spref", don't include "metadata" for FGDC).
    3. A string representation of well-formatted XML content.
        Example:'<samplenode><innernode><pocketcontent1>PC1</pocketcontent1><pocketcontent2>PC2</pocketcontent2></innernode></samplenode>'
    '''

    if type(xml_input) is lxml.etree._Element:
        etree = xml_input
    else:
        # xml is a file path
        etree = et.ElementTree()
        etree.parse(xml_input)

    node_content = et.XML(new_node_contents)#Converts a string representing XML content to a well-formatted XML node object. This chokes if string has two root elements!

    if etree.find(xpath)!= None:
        stub = etree.find(xpath)
        stub.clear()
        stub.insert(0, node_content)
    elif add_if_missing:

        original_xpath = xpath
        missing_nodes = []
        while etree.find(xpath) is None:
            pathnodes = xpath.split("/")  # Split the path into a list of nodes.
            missing_nodes.insert(0, pathnodes[-1])  # Create a list of nodes we can't find. Start with the lowest node and work up.
            xpath = "/".join(pathnodes[:-1])  # Try again higher up trunk of tree. The new parent node string becomes the string minus the last node.

        for node in missing_nodes:
            stub = etree.find(xpath)
            node_to_add = et.Element(str(node))
            node_to_add.content = node_content
            stub.insert(0, node_to_add)  # This inserts the new element as the first child.
            xpath = xpath + "/" + node  # Rebuild the full path to the node we originally wanted to edit.

        for target_node in etree.xpath(original_xpath):
            target_node.text = new_node_contents

    else:
        raise RuntimeError, \
            "Unable to update the XML file. Check that the file exists, \n" \
            "the provided node-path within the XML, or permission settings."

    print et.tostring(etree, pretty_print=True)
    return etree


# Updates the text content of an XML element, specified by full address/path.
def change_xml_node_text(xml_input=None, xpath=None, new_node_text=None, add_if_missing=True):

    '''
    Inputs:
    1. XML file metadata record.
    2. The full path to the element which needs to have its content edited, skipping the Root. (Example: "idinfo/citation/citeinfo/origin")
    3. The text to replace the current content.
    4. add_if_missing True/False toggle to build out to element when not found. Defaults to True.

    By default the function will attempt to build out to the full path and update the text.
    '''

    if type(xml_input) is lxml.etree._Element:
        etree = xml_input
    else:
        # xml is a file path
        etree = et.ElementTree()
        etree.parse(xml_input)

    # test to see if the node exists......
    r = etree.xpath(xpath)
    if len(r) > 0:
        for target_node in etree.xpath(xpath):
            target_node.text = new_node_text

    elif add_if_missing:

        original_xpath = xpath
        missing_nodes = []
        while etree.find(xpath) is None:
            pathnodes = xpath.split("/")  # Split the path into a list of nodes.
            missing_nodes.insert(0, pathnodes[-1])  # Create a list of nodes we can't find. Start with the lowest node and work up.
            xpath = "/".join(pathnodes[:-1])  # Try again higher up trunk of tree. The new parent node string becomes the string minus the last node.

        for node in missing_nodes:
            stub = etree.find(xpath)
            node_to_add = et.Element(str(node))
            node_to_add.text = new_node_text
            stub.insert(0, node_to_add)  # This inserts the new element as the first child.
            xpath = xpath + "/" + node  # Rebuild the full path to the node we originally wanted to edit.

        for target_node in etree.xpath(original_xpath):
            target_node.text = new_node_text

    else:
        raise RuntimeError, \
            "Unable to update the XML file. Check that the file exists, \n" \
            "the provided node-path within the XML, or permission settings."

    print et.tostring(etree, pretty_print=True)
    return etree
