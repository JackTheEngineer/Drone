# Drone
A test driven developed Quadrocopter C Code. 
I know the build Environment to be supported by Windows and Linux. 

## Table of Content
* Initial idea 
* The coding philosophy
* Development environment 
    * The software environment
    * Folder Setup
    * Hardware 
* A note about licensing

## Initial idea and Motication
This Code is supposed to fulfill three goals:
* To share my knowledge, my Environment and the enthusiasm for and about Test Driven Development in C. 
    I am especially refering to the moments where i kept thinking that "There has to be a better way".  
    And there really is: It's called TDD !
* To help me learn how to write physics simulations in C. Before the Microcontroller gets to contol any hardware stuff,   
	the code has to prove itself on a self-written Drone Simulation. 
* To help me get a real good flying Drone. 

## The coding philosophy
Clean  
tested  
expressive  
As much hardwareindependency as possible.  
No comments whenever possible, the code should explain itself.
Good comments if required.  

## Development environment 
### The Software Environment 
haskell stack:  [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)
				Install it, in the way it is described in the link. 
haskell shake:  A very cool syntax in haskell to define dependencies.
				[https://shakebuild.com/](https://shakebuild.com/) 
				Install it with `shake install shake yaml parsec`, to be able to run the build system.
				The `bld/YACBS.hs` is programmed with shake. 
				It requires a yaml file with the ending '.ucbuild' to build 
				an elf, hex and bin file for the microcontroller. 
ARM-C-Compiler: [gcc-arm-none-eabi-4_9-2014q4](https://launchpad.net/gcc-arm-embedded/+milestone/4.9-2014-q4-major)  
                I highly recommend you to download it separately as a binary, and specify the correct binary within the `.ucbuild` files.
ARM-C-GDB:      It comes along with the gcc arm none eabi installation and is used for debugging.  
GCC-C-Compiler: For Windows the latest Version of [MinGW](http://www.mingw.org/) works fine. 
                For Linux take a look at your repositories. ;)  
cppcheck:       [cppcheck](http://cppcheck.sourceforge.net/) With the current configuration 
                it is currently invoked plainly with "cppcheck", without having a nice Global configuration.  
Jlink:          [segger-jlink](https://www.segger.com/jlink-software.html). 
                The Software responsible for the connection to the ARM-device, using the "Single wire-debug"  
Eclipse-CPP:    [eclipse C\C++ IDE](http://www.eclipse.org/downloads/packages/eclipse-ide-cc-developers/mars2)  
Gnuplot :	[gnuplot](http://www.gnuplot.info/)  
and obviously - [git](https://git-scm.com/)

### Folder setup
`hardware/`:  	This is the hardware abstraction layer. 
				All stuff belonging to SPI, GPIO, UART and so on goes here.
				In case of switching hardware, this makes it much easier to 
				port Code to another Mikrocontroller. The Content is currently 
				generated with Infineon DAVE. 
`Drone_src/`: 	This is the actual Code for the Drone, without the hardware part
`RFM75_src/`: 	The common code for the RFM75 Radio tranceiver used both by the 
				Remote control and by the Drone, again, without the hardware.
				Maybe this will become an own repository.
`RemoteControl_src`: This is the code For the remote control only
`Common_src`: Some code that is also shared between the projects.			 
`test/`: The code for testing the mikrocontroller Code
`vendor/`: contains unity sources and makros for compiling the tests


### Hardware
For the Drone main Controller I use the ARM-M4-Evaluation Board
[https://www.infineon.com/cms/en/product/evaluation-boards/kit_xmc47_relax_5v_ad_v1/](XMC4700 Relax Kit Lite 5V) 
by Infineon with an XMC4700 Mikrocontroller.
For The Remote Control I use the 2Go-XMC1100 Mikrocontroller. 
I Don't recommend bying the XMC4500 Relax 
with either the AB step, nor the AC step as the generated code with the code examples
does not correctly control the SPI interface. After the first byte, it just sends
5 more sck pulses and stops for some reason. I didn't read it up and did not want to 
fiddle with the register values of the USIC to fiddle it out.
There are quite some errors listed in the ERRATA sheets belonging to the USICS.   
In the XMC4700 and XMC1100 these errors have been fixed, and the SPI works.

For the remote Control I use the Chinese HopeRF RFM75 Chip, Bought at Pollin.

For the gyroscope and accelerometer i use the MPU 9265 sensor.

### Plotting over Uart

	pip install pyserial pyqtgraph numpy
	
###### A note about licensing:
Unless the Source files have their own Licensing note,
or are contained within the Folders "vendor" or "XMC_Libs",
they are provided with the GNU-GPL3 freedom. With the current use of the 
XMC-4500 Infineon-ARM-hardware, the **binaries are NOT GNU-GPL3-Free**. 
If you share the compiled binaries, you have to provide the infineon and the ARM 
licensinc notes found in the top sections of every file of "XMC_Libs" and "CIMSIS_Libs". 
I know, that i cannot provide everything GNU-GPL3 free, but everything 
from the hardware part, i want you to extensively use and modify.
Probably there is a License supporting this kind of configuration.
I haven't read enought about different types of licences.

#### Thanks to:
   The awesome [Jakob Holderbaum](http://jakob.io/) who taught me TDD ! (@holderbaum)  
   Mark VanderVoord, Mike Karlesky, and Greg Williams for writing the [Unity Test framework](http://www.throwtheswitch.org/unity/).
