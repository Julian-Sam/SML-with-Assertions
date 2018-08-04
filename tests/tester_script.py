import subprocess
from os import listdir, remove
from os.path import isfile, join


mypath = "test.sml"
#broken_files = []

#files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

def new_file_name(old_file_name):
	return old_file_name[old_file_name.rfind('.')-1] + "_parsed.sml"



a = "sml sml_tester1.sml " + mypath
return_status = subprocess.call(a, shell=True)
if return_status == 1:
	print ("Test File Failed On SML Parser\n\n")
else:
	b = "sml_tester2.sml " + mypath
	return_status2 = subprocess.call(b, shell=True)
	if return_status2 == 1:
		print ("Test File Failed On Our Parser\n\n")
	else: 
		print ("\nFile Successfully Parsed\n\n")



#new_files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

#for i in new_files:
#	if "_parsed.sml" in i:
#		remove(join("test_files", i))

#for i in broken_files:
#	print(i + " does not parse properly")