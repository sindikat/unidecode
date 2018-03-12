#!/usr/bin/env python

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

"""unidecode_convert.py

Convert the character tables in the Python Unidecode package to JSON. This is
intended to be called by unidecode-convert.el as part of the process to convert
the character tables to Emacs Lisp data.
"""

from __future__ import print_function

import os
import sys
import json
import importlib
from glob import glob

# Source files in Unidecode that we should not attempt to convert
ignore_files = ('__init__.py', 'util.py')


def convert_module(name, destination):
    module = importlib.import_module(name)
    data = list(module.data)
    with open(destination, 'wb') as outfile:
        json.dump(data, outfile)


def convert_modules(source_dir, destination_dir):
    if not os.path.isdir(source_dir):
        print("Source directory does not exist: %s" % source_dir)
        sys.exit(1)
    if not os.path.isdir(destination_dir):
        print("Destination directory does not exist: %s" % destination_dir)
        sys.exit(1)
    sys.path.append(source_dir)
    for filename in glob(os.path.join(source_dir, '*.py')):
        basename = os.path.basename(filename)
        if basename in ignore_files:
            continue
        module_name = os.path.splitext(basename)[0]
        destination = os.path.join(destination_dir, module_name + '.json')
        convert_module(module_name, destination)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: unidecode_convert.py source destination")
        sys.exit(1)
    convert_modules(sys.argv[1], sys.argv[2])
