# YACBS (Yet Another C Build System)

## Shortwords:
* uC : Microcontroller

## keypoints to explain and extend

compilation :
c sources -> object files -> executable.
It is the same Process for a microcontroller and for a executable on windows or linux. 
The file format and the endings are different. On a uC it can be an ".elf" file if you want to 
debug using f.e. JTAG or SWI,  or a ".hex" file if you just want to flash the code using an 
In-System-Programmer(ISP), or bootloading the code.

Architectural Difference between, test compiler, target compiler

Header inclusion: test sources first, then arm headers first. 

definition for tests with it's own test\_sources.yml

Please use test\_*name* for the name and for the test definition in test_sources.yml

If just a target is defined, and no command line arguments are given, uses:
["test/test\_compiler.yml", 
"bld/test\_compiler\_define.yml", 
"test\_compilation\_definition.yml"]

If no second argument is given, automatically searches in paths for 
["test/test\_sources.yml", 
"bld/test\_sources.yml"]



## Install instruction

* install haskell stack

		stack install shake yaml

## Example Usage 
	
		stack bld/YACBS.hs bld/uC.yml
		
		(does not work yet) stack bld/YACBS.hs test_dummy test/test_sources.yml test/test_compilation_definition.yml
		
		(does not work yet) stack bld/YACBS.hs test_dummy 

		
		
