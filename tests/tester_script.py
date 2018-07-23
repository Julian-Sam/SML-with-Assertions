import subprocess
from os import listdir, remove
from os.path import isfile, join


mypath = "test_files"
broken_files = []

files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

def new_file_name(old_file_name):
	return old_file_name[old_file_name.rfind('.')-1] + "_parsed.sml"

for i in files:
	print(i)

for i in files:
	a = "sml sml_tester.sml " + join("test_files", i)
	return_status = subprocess.call(a, shell=True)
	if return_status == 1:
		broken_files.append(i)


new_files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

for i in new_files:
	if "_parsed.sml" in i:
		remove(join("test_files", i))

for i in broken_files:
	print(i + " does not parse properly")