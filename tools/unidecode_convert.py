#!/usr/bin/env python

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
