# Drone
A test driven developed Quadrocopter C Code. 
I know the build Environment to be supported by Windows and Linux. 

## Table of Content
* Initial Idea 
* The Philosophy
* Development Environment 
    * The Software Environment
    * Hardware 
* A note about licensing

## Initial Idea
This Code is supposed to fulfill three aims:
* To share my knowledge, my Environment and the enthusiasm for and about Test Driven Development in C. 
    I am especially refering to the moments where i kept thinking that "There has to be a better way".  
    And there really is: It's called TDD !
* To help me learn how to write physics simulations in C. Before the Microcontroller gets to contol any hardware stuff,   
	the code has to prove itself on a self-written Drone Simulation. 
* To help me get a real good flying Drone. 

## The Philosophy
Clean, tested Code. As much hardwareindependency as possible. 

## Development Environment 
### The Software Environment 
Ruby : for using Rake  
        For Windows i recommend the [rubysinstaller](http://rubyinstaller.org/)   
        For Linux take a look at your repositories.  
        My currently used version 2.3.1.  
        (i haven't tried any other versions - 
        just get 2.3.1 if you have any issues and change the rake command to use the specific 2.3.1 version)  
Rake : the build tool - instad of the outdated make.
        Rake is just an extenstion for Ruby. It provides a convenient way of telling the Computer how to compile a binary excecutable and on which files the binary depends.
        For Installation open the terminal and type: gem install rake  
ARM-C-Compiler: [gcc-arm-none-eabi-4_9-2014q4](https://launchpad.net/gcc-arm-embedded/+milestone/4.9-2014-q4-major)  
                I highly recommend you to download it separately as a binary, and specify the correct binary within the rake-arm-definitions.
ARM-C-GDB:      It comes along with the gcc arm none eabi installation and is used for debugging.  
GCC-C-Compiler: For Windows the latest Version of [MinGW](http://www.mingw.org/) works fine. 
                For Linux take a look at your repositories. ;)  
cppcheck:       [cppcheck](http://cppcheck.sourceforge.net/) With the current configuration 
                it is currently invoked plainly with "cppcheck", without having a nice Global configuration.  
Jlink:          [segger-jlink](https://www.segger.com/jlink-software.html). 
                The Software responsible for the connection to the ARM-device, using the "Single wire-debug"  
Eclipse-CPP:    [eclipse C\C++ IDE](http://www.eclipse.org/downloads/packages/eclipse-ide-cc-developers/mars2)  
and obviously - [git](https://git-scm.com/).

### The Hardware
Currently the only Hardware I use is an ARM-M4-Evaluation Board by Infineon using the XMC4500.
I haven't ordered any Drone Hardware like motors and sensors yet. 

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
