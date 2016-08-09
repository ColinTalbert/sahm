"""Utility functions for obtaining, querying, and modifying VisTrails
 workflows and pipelines

These were written for and are used for data management and archiving
"""


from vistrails.core.application import get_vistrails_application


def get_current_history_node():
    """
    Queries the current history tree and returns the name (tag)
    and change count of the currently active history node

    Returns
    -------
        tuple (str, int)
            the first item is the name (tag) of the currently selected
                history node.  'root' will be returned if no named node is
                selected
            the second item is the integer count of unnamed nodes (changes)
                since the last named node
    """
    controller = get_vistrails_application().get_current_controller()
    cur_version = controller.current_version
    cur_vt = controller.vistrail
    cur_name = cur_vt.get_pipeline_name(cur_version)
    if "+" in cur_name:
        count = int(cur_name.split(' + ')[-1])
        cur_name = " ".join(cur_name.split()[:-2])
    else:
        count = 0

    return cur_name, count
