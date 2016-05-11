# Drone
A test driven developed Quadrocopter C Code. 
I know it to support Windows and Linux -- I don't have an Äpfel Machine.

## Table of Content
* Initial Idea 
* Development Environment 
    * The Software Environment
    * Hardware 
* A note about licene

## Initial Idea
This Code is supposed to fulfill three aims:
* To share my knowledge, my Environment and the enthusiasm for and about Test Driven Development in C. 
    I am especially refering to the moments where i kept thinking that "There has to be a better way".
* To learn how to write physics simulations in C 
* To get a real good flying Drone 


## Development Environment 
### The Software Environment 
Ruby : for using Rake  
        For Windows i recommend the [rubysinstaller](http://rubyinstaller.org/)   
        For Linux take a look at your repositories.    
        My currently used version 2.3.1.   
Rake : the build tool - instad of the outdated make.
        Rake is just an extenstion for Ruby. It provides a convenient way of telling the Computer how to compile a binary excecutable and on which files the binary depends.
        For Installation open the terminal and type: gem install rake    

ARM-C-Compiler: [gcc-arm-none-eabi-4_9-2014q4](https://launchpad.net/gcc-arm-embedded/+milestone/4.9-2014-q4-major)  
                I highly recommend you to download it separately as a binary, and specify the correct binary within the rake-arm-definitions.
GCC-C-Compiler: For Windows the latest Version of [MinGW](http://www.mingw.org/) works fine. 
                For Linux take a look at your repositories. ;)
                
### The Hardware
Currently the only Hardware I use is an ARM-M4-Evaluation Board by Infineon using the XMC4500.
I haven't ordered the Drone Hardware yet. 

###### A note about licensing:
    Unless the Source files have their own Licensing note, or are contained within the Folders "vendor" or "XMC_Libs", they are provided with the GNU-GPL3 freedom.

#### Thanks to:
   The awesome Jakob Holderbaum who taught me TDD!
   Mark VanderVoord, Mike Karlesky, and Greg Williams for writing the Unity Test framework !
