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
	if "_parsed.sml" in i:
		continue
	a = "sml sml_tester.sml " + join(mypath, i)
	return_status = subprocess.call(a, shell=True)
	if return_status == 1:
		broken_files.append(i)


new_files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

for i in new_files:
	if "_parsed.sml" in i:
		remove(join(mypath, i))

one_fail = False

for i in broken_files:
	one_fail = True
	print(i + " does not parse properly")

if not one_fail:
	print("\n\nAll tests passed!")