# Todos
## Build system
   * "all" target runs hardcoded specified targets and "tests", needs  for
     like "stack runghc bld/YACBS.hs all"

## Hardware
* check if continuously reading all registers with one command works
* for -O2 ---> try using SPI config app, and test 'coded' version of clock select signal, and a different

* PRIO 2:
   - Optionally, for speedup implement the spi with TRANSFER functions
   - signal: RFM75_disconnected should be present, 
	 and on reconnection reinitialized ? (bang testing the status register, or != 0xFF on answer)

## Testing the Code:
   * Either Continue Euler Integral Simulation
     - Find out moments of the Motors
     - Find propeller masses
     - Find and set constants for fake_motors.h
     
   * Find a 3D simulator and adapt it to my code / or my code to it
   
 
