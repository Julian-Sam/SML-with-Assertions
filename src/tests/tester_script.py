import subprocess
import sys
import fileinput
from os import listdir, remove
from os.path import isfile, join

def new_file_name(old_file_name):
	return old_file_name[:(old_file_name.rfind('.')-1)] + "_parsed" + old_file_name[:(old_file_name.rfind('.'))]

print (new_file_name("test.sml"))
sys.exit()

files = [] # to b changed
sources_file # to b changed
sources_exists = False # to b changed
one_file_fail = True

for file in files:
	command = "sml " + file
	return_status = subprocess.call(a, shell=True)
	if return_status == 1:
		print ("File: " + file + " failed on the SML Parser\n")
		one_file_fail = True

if one_file_fail:
	sys.exit()

if sources_exists:
	for file in files:
		with fileinput.FileInput(sources_file, inplace=True, backup='.bak') as open_file:
			for line in open_file:
				print(line.replace(file, new_file_name(file)), end='')



for x in xrange(1,10):
	pass


else:
	b = "sml src/sml_tester.sml " + mypath
	return_status2 = subprocess.call(b, shell=True)
	if return_status2 == 1:
		print ("File Failed On Assertions Parser\n")

