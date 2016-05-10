# Drone
A test driven developed Quadrocopter C Code. 
It's supposed to support Windows and Linux - Sorry not to care about Äpfel.

## Initial Idea
This Code is supposed to fulfill three aims:
* To share my knowledge, my Environment and the enthusiasm for and about Test Driven Development in C 
* To learn how to write physics simulations in C 
* To get a real good flying Drone 


## Software Development Environment 
### The Software Tools
Ruby : for using Rake  
    For Windows i recommend the [rubysinstaller](http://rubyinstaller.org/)   
    For Linux take a look at your repositories.    
    My currently used version 2.3.1.   
Rake : the build tool - instad of make   
    For Installation open the terminal and type: gem install rake    

ARM-C-Compiler: [gcc-arm-none-eabi-4_9-2014q4](https://launchpad.net/gcc-arm-embedded/+milestone/4.9-2014-q4-major)  
  I highly recommend you to download it separately as a binary, and specify the correct binary within the rake-arm-definitions.
