#!/usr/bin/python3.7

import argparse
import os
import subprocess

DESC_MSG =      'Tony Compiler by Dimitris and Spyros :)'
USAGE_MSG =     "Usage: python3 tonyc.py [options] file... \n" + \
                "Options: \n" + \
                "-o enable optimizations\n"+ \
                '-f Show final code at stdout\n'+ \
                '-i Show intermediate code at stdout\n'

# Initialize the Parser
#parser = argparse.ArgumentParser(usage=ABOUT_MSG, description=DESC_MSG)

# Create the parser and add arguments
parser = argparse.ArgumentParser()
parser.add_argument(
    'filename',
    help="Filename of Tony program")
group = parser.add_mutually_exclusive_group()
group.add_argument('-f',
    action='store_true',
    default=False,
    help='Input from standard input, assembly code to standard output')
group.add_argument('-i',
    action='store_true',
    default=False,
    help='Input from standard input, intermediate code to standard output')
parser.add_argument(
    '-O',
    action='store_true',
    default=False,
    help='Enable code optimization')


# Parse and print the results
args = parser.parse_args()

if args.O:
    optimize = "-O"
else:
    optimize = ""

name = os.path.splitext(args.filename)[0]

# Run Tony compiler
#./tony {} < {} > {}.imm".format(optimize, args.filename, name) :the os way
try:
    subprocess.run(["./tony", optimize], stdin=open(args.filename, 'r'), \
                stdout=open(name + '.imm', 'w'),
                check=True)
except subprocess.CalledProcessError as e:
    subprocess.run(["rm", name + '.imm'])
    print(e)
    print("hahah")

if args.i:
    with open(name + '.imm', 'r') as interim:
        print(interim.read())
