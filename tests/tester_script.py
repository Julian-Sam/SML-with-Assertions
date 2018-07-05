from os import listdir, walk
from os.path import join, relpath
import subprocess

def new_file_name(old_file_name):
	return old_file_name[old_file_name.find('.')-1] + "_parsed.sml"

mypath = "."

fileSet = set()

for dir_, _, files in walk(mypath):
    for fileName in files:
    	if fileName != "tester_script.py":
	        relDir = relpath(dir_, mypath)
	        relFile = join(relDir, fileName)
	        fileSet.add(relFile)

for i in fileSet:
	print("sml sml_tester.sml " + "test_files" + i)
	subprocess.Popen("sml sml_tester.sml " + i)
	subprocess.Popen("diff " + i + " " + new_file_name(i))