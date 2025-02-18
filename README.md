# Drone
A test driven developed Quadrocopter C Code. 
I know the build Environment to be supported by Windows and Linux. 

## XMC on Linux
This is an example code which compiles code from Infineon XMC4500, XMC4700 and XMC1100 on Linux. 
I used the Infineon IDE to generate the hardware layer code for the microcontrollers, and copied it into 
my linux project. The XMCLib is pretty usable by now, and for some simpler cases sometimes doesn't even need 
the generated code. If you want to define a new SPI / I2C / QSPI interface, it's probably simpler to use the code generator though.

## Table of Content
* Initial idea 
* Development environment 
    * The software environment
    * Folder Setup
    * Hardware 
* A note about licensing

## Initial idea and Motication
This Code is supposed to fulfil mostly one goal:
* To share my knowledge, my Environment and the enthusiasm for and about Test Driven Development in C. 
    I am especially refering to the moments where i kept thinking that "There has to be a better way".  
    And there really is: It's called TDD !

## Development environment 
### The Software Environment 
- Haskell stack:  
  [Haskell stack](https://docs.haskellstack.org/en/stable/README/)
  Install it, in the way it is described in the link. 
- Haskell Shake:  
  A very cool syntax in haskell to define dependencies.
  [https://shakebuild.com/](https://shakebuild.com/) 
  Install it with `stack install shake yaml`, to be able to run the build system. 
  I have do admit that i didn't dive in too deeply into the stack build system and 
  package management, but there is also the option to add the dependencies to 
  `extra-deps` of your `~/.stack/global-project/stack.yaml` as
  
		extra-deps:
			- shake-0.18.3
			- yaml-0.11.2.0
					
	The `bld/YACBS.hs` is programmed with shake. 
	It requires a yaml file with the ending '.ucbuild' to build 
	an elf, hex and bin file for the microcontroller.
	It also supports building selected sources
	for a software test on your laptop, by using the awesome embedded 
	Unity test framework by 'Throw The Switch'.

- ARM-C-Compiler: 
  [gcc-arm-none-eabi-4_9-2014q4](https://launchpad.net/gcc-arm-embedded/+milestone/4.9-2014-q4-major)  I highly recommend you to download it separately as a binary, 
  and specify the correct binary within the `.ucbuild` files.
- ARM-C-GDB:  
  It comes along with the gcc arm none eabi installation and is used for debugging.  
- GCC-C-Compiler:  
  For Windows the latest Version of [MinGW](http://www.mingw.org/) works fine. 
  For Linux take a look at your repositories. ;)  
- cppcheck:  
  [cppcheck](http://cppcheck.sourceforge.net/)  
  With the current configuration  it is currently invoked plainly with "cppcheck", 
  without having a nice Global configuration.  
- Jlink:  
  [segger-jlink](https://www.segger.com/jlink-software.html). 
  The Software responsible for the connection to the ARM-device, using the "Single wire-debug"  
- Eclipse-CPP:    
  [eclipse C\C++ IDE](http://www.eclipse.org/downloads/packages/eclipse-ide-cc-developers/mars2)  
- [git](https://git-scm.com/)

### Compiling the Project and different Targets
Once you have installed the haskell `stack` tool, and run `stack install shake yaml`, you need to configure 
the path to your `arm-none-eabi-gcc` executable. This is done by modifying the yaml files with the `*.ucbuild` 
extension in the `bld` directory here:
```
Drone_XMC4500.ucbuild
Drone_XMC4700.ucbuild
MotionSensor_I2C_Test.ucbuild
RemoteControl_XMC1100.ucbuild
RFM75_XMC4500_Test.ucbuild
RFM75_XMC4700_Test.ucbuild
```
Each `.ucbuild` represents a uC - (read microcontroller) target / subproject, with it's own compiler settings sources, linker files and compilation flags. 
You need to supply the  base - path to the arm-none-eabi-gcc project, by setting the `ccBase` to the folder on your system.
This is needed because some sub-paths from there are needed for the compilation too.  
``` yaml
ccBase: /nix/store/ra5wly4vbakq66srldxm57lg8qnmb4jr-gcc-arm-embedded-12.3.rel1  ## change here. Resulting path should be without the '/' at the end
```
I recommend you to use your editor and replace that variable in all the files, so all targets can be compiled on your system. This is an open TODO.
to put those projectwide commonly used variables in a separate file.
Furthermore, in order to run the tests, you need a more or less recent 'gcc' on your PATH.
Once you set those variables, you can use the following commands to compile (calling from within the project directory): 

- *a single test* : `stack runghc bld/YACBS.hs test/test_dummy.yml` 
- *a singgle microcontroller target* : `stack runghc bld/YACBS.hs bld/Drone_XMC4700.ucbuild` 
- *clean* :`stack runghc bld/YACBS.hs clean` 
- *all targets* : `stack runghc bld/YACBS.hs all`
- *all tests* : `stack runghc bld/YACBS.hs tests` , this might run into problems, if any of the `.ucbuild` files 
  contains a nonexistent, or wrong path

### Folder setup
- `hardware/`:  	
  This is the hardware abstraction layer. 
  All stuff belonging to SPI, GPIO, UART and so on goes here.
  In case of switching hardware, this makes it much easier to 
  port Code to another Mikrocontroller. The Content is currently 
  generated with Infineon DAVE. 
- `Drone_src/`: 	
  This is the actual Code for the Drone, without the hardware part
- `RFM75_src/`: 	
  The common code for the RFM75 Radio tranceiver used both by the 
  Remote control and by the Drone, again, without the hardware.
  Maybe this will become an own repository.
- `RemoteControl_src`: 
  This is the code For the remote control only
- `Common_src`: 
  Some code that is also shared between the projects.
- `test/`: 
  The code for testing the mikrocontroller Code
- `vendor/`: 
  contains unity sources and makros for compiling the tests, [Unity Test framework](http://www.throwtheswitch.org/unity/).


### Hardware
As the Code evolved, I was using two Infineon Boards at the same time, the XMC4500 relax kit lite and
the ARM-M4-Evaluation Board
[https://www.infineon.com/cms/en/product/evaluation-boards/kit_xmc47_relax_5v_ad_v1/](XMC4700 Relax Kit Lite 5V)  by Infineon with an XMC4700 Mikrocontroller.
The build system builds both of the targets, each with different pinouts. 
If you would like me to upload the DAVE projects, 
which generated the 'hardware/XMC4X00/Generated' Code, leave an Issue in the Issuetracker.
For The Remote Control I use the 2Go-XMC1100 Mikrocontroller.
With neither the XMC4500 AA step, nor the AC step, the generated code with the code examples correctly control the SPI interface. Immediately
There are quite some errors listed in the ERRATA sheets belonging to the USICS of the XMC4500.  
In the XMC4700 and XMC1100 these errors have been fixed, and the SPI works.

Nevertheless, after some magic things happened -- probably some minor modifications 
to the SPI code -- the SPI started to work on the XMC4500. 
Yet, i don't know exacly, what the reason was, neither the breaking code change.
For the remote Control I use the Chinese HopeRF RFM75 Chip, Bought at Pollin.
For the gyroscope and accelerometer I use the MPU 9265 sensor, 
the most common 9D-Motion Sensor.

### Libraries for Plotting over Uart 

	pip install pyserial pyqtgraph numpy
	
###### A note about licensing:
Unless the Source files have their own Licensing note,
or are contained within the Folders "vendor" or "XMC_Libs",
they are provided with the GNU-GPL3 freedom. With the current use of the 
XMC-4500 Infineon-ARM-hardware, the **binaries are NOT GNU-GPL3-Free**. 
If you share the compiled binaries, you have to provide the infineon and the ARM 
licensing notes found in the top sections of every file of `XMC_Libs` and `CIMSIS_Libs`. 
I know, that i cannot provide everything GNU-GPL3 free, but everything apart
from the hardware part, i want you to extensively use and modify.
Probably there is a License supporting this kind of configuration.
I haven't read enought about different types of licences.

#### Thanks to:
   The awesome [Jakob Holderbaum](https://holderbaum.io/) who taught me TDD ! (@holderbaum)  
   Mark VanderVoord, Mike Karlesky, and Greg Williams for writing the [Unity Test framework](http://www.throwtheswitch.org/unity/).
