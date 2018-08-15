import subprocess, argparse
import sys
import fileinput
from os import listdir, remove
from os.path import isfile, join, abspath

def new_file_name(old_file_name):
    return old_file_name[:(old_file_name.rfind('.'))] + "_parsed" + old_file_name[(old_file_name.rfind('.')):]

if __name__ == "__main__":
    #Initializing the Parser
    Arguments = argparse.ArgumentParser (description = "SML Compiler Script Arguments")

    #Adds positional parameters necessary
    Arguments.add_argument('-f', metavar = "filename", nargs = '+', type = str, help= "The filename to be parsed")
    Arguments.add_argument('-c', metavar = "sources", nargs = 1, type = str, help= "The source file to be compiled")

    #Parse the arguments
    Args = Arguments.parse_args()

    if (Args.c != None):
        sources_exists = True # to b changed
        sources_file = Args.c[0]
    else:
        sources_exists = False

if Args.c is not None and len(Args.c) > 1:
    print("Only up to one sources file allowed")
    sys.exit()

files = Args.f 
one_file_fail = False

# for file in files:
#     command = "sml " + file # replace with byte code thing
#     # return_status = subprocess.call(command, shell=True)
#     # if return_status == 1:
#     #   print ("File: " + file + " failed on the SML Parser\n")
#     #   one_file_fail = True

if one_file_fail:
    sys.exit()

if sources_exists:
    for file in files:
        print(file)
        with fileinput.FileInput(sources_file, inplace=True) as open_file:
            for line in open_file:
                print(line.replace(file, new_file_name(file)), end='')

    command = "sml run_sources.sml " + sources_file
    return_status = subprocess.call(command, shell=True)

else:
    if len(files) != 1:
        print("Please provide a sources file with the -c flag if there is more than one file. Use the -h flag for help")
        sys.exit()

    command = "sml " + abspath(join("src", "run_parser.sml")) + " " + abspath(files[0]) + " " + join("src", "sources.cm")
    return_status = subprocess.call(command, shell=True)
    if return_status == 1:
        print ("File: " + files[0] + " failed on the Assertions Parser\n")

    print("\nFile Parsed Successfully!\n")
    command = "sml " + new_file_name(files[0])
    return_status = subprocess.call(command, shell=True)


remove("src/lexer_engine.lex.sml")
remove("src/parser_engine.grm.desc")
remove("src/parser_engine.grm.sig")
remove("src/parser_engine.grm.sml")
