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

The "tests" target looks for all "test_*.yml" files in the "test" directory, 
compiles and runs them all in parallel. This happens in perfect safety, 
thanks to the awesome dependency tracking system of shake.


## Install instruction

* First install haskell stack, then:

		stack install shake yaml parsec

## Example Usage( from the 'Drone' directory)

    		stack bld/YACBS.hs bld/XMC4500.ucbuild
		
		stack bld/YACBS.hs bld/test_dummy.yml
		
		stack bld/YACBS.hs tests # Looks for "test/test_*.yml" files 

		
		
