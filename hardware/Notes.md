# Hardware configuration
For the selection of the SPI Type use 'low if inactive, transmit on falling edge, receive on rising edge' -> which generates: `XMC_SPI_CH_BRG_SHIFT_CLOCK_PASSIVE_LEVEL_0_DELAY_ENABLED`

At 2MHz SPI Freq:
On RFM75 SPI use no Interword - Delay, and at least some Chip Select delay ( i used 4 clock cycles, -> 2 uS). Even though, the RFM75 Manual says it's supposed to be some 10's of nanoseconds, it seems to be required.


# Notes to Infineon
ADC\_MEASUREMENT\_Extern.h to adc_measurement_extern.h in adc_measurement.h
DAVE\_common.h to DAVE\_Common.h in uart.h
Add a errata note to the XMC1100-2Go TLI4790 on the schematic to 
tell about the short circuits on the LED's.


# Notes to RFM75 
All registers have to be written. 
The order of commands, in which a RX-pipe is configured, Matters. 
The registers have to be set in the same ascending order as in the datasheet. 

# If you want to port the Code to another XMC Mikrocontroller:
Make sure it's not the XMC4500 AA, AB, or AC step, as they have a very much fucked up USIC that does not work
with the 'common' spi code. If you do port it to the xmc4500 please make sure that the clock of the SPI works correctly.
When I was testing it, during the SPI communication the clock just sent 13 pulses instead of 16. Dunno why.
