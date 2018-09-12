import subprocess, argparse
import sys
import fileinput
from os import listdir, remove
from os.path import isfile, join, abspath

def new_file_name(old_file_name):
    return old_file_name[:(old_file_name.rfind('.'))] + "_parsed" + old_file_name[(old_file_name.rfind('.')):]

##def modify_file_path(old_file_path):
##    file_path_list = old_file_path.split("\\")
##    new_file_path = ""
##    for i in file_path_list[:len(file_path_list) - 1]:
##        new_file_path += '"' + i + '"\\'
##    return new_file_path + '"' + file_path_list[-1] + '"'
    
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

if sources_exists:
    # Copy files content to a variable, to insert into the file later (since original file will be modified)
    with open(sources_file, "r") as src_file:
        sources_file_data = src_file.read()

    for file in files:
        print(file)

        # Replace the file with the new file name in the sources file
        with fileinput.FileInput(sources_file, inplace=True) as open_file:
            for line in open_file:
                print(line.replace(file, new_file_name(file)), end='')

        # Make the new parsed files
        command = "sml " + abspath(join("src", 
            "run_parser.sml")) + " " + abspath(file) + " " + join("src", "sources.cm")
        return_status = subprocess.call(command, shell=True)

    command = "sml run_sources.sml " + sources_file
    return_status = subprocess.call(command, shell=True)

    # Write back to the file the original data 
    with open(sources_file, "w") as src_file:
        src_file.write(sources_file_data)

    # Remove new files created
    for file in files:
        remove(new_file_name(file))

else:
    if len(files) != 1:
        print("Please provide a sources file with the -c flag if there is more than one file. Use the -h flag for help")
        sys.exit()

    command = "sml " + abspath(join("src", "run_parser.sml")) + " " + abspath(files[0]) + " " + join("src", "sources.cm")
    print (command)
    print ("\n")
    return_status = subprocess.call(command, shell=True)
    print (return_status)
    print ("\n")
    if return_status == 1:
        print ("File: " + files[0] + " failed on the Assertions Parser\n")
        sys.exit()

    print("\nFile Parsed Successfully!\n")
    # Apparantly running 'sml <test.sml' allows you to exit the SML interpreter after running
    command = "sml <" + new_file_name(files[0])
    return_status = subprocess.call(command, shell=True)
    remove(new_file_name(files[0]))

remove("src/lexer_engine.lex.sml")
remove("src/parser_engine.grm.desc")
remove("src/parser_engine.grm.sig")
remove("src/parser_engine.grm.sml")
