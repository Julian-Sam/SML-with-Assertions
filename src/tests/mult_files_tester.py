import subprocess
from os import listdir, remove
from os.path import isfile, join

mypath = "test_files"
broken_files = []
files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

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


for i in broken_files:
	print(i + " does not parse properly")

if not broken_files:
	print("\n\nAll tests passed!")


remove("../lexer_engine.lex.sml")
remove("../parser_engine.grm.desc")
remove("../parser_engine.grm.sig")
remove("../parser_engine.grm.sml")
