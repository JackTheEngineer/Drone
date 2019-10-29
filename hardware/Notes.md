# Hardware configuration

For the selection of the SPI Type use 'low if inactive, transmit on falling edge, receive on rising edge' -> which generates: `XMC_SPI_CH_BRG_SHIFT_CLOCK_PASSIVE_LEVEL_0_DELAY_ENABLED`

At 2MHz SPI Freq:
On RFM75 SPI use no Interword - Delay, and at least some Chip Select delay ( i used 4 clock cycles, -> 2 uS). Even though, the RFM75 Manual says it's supposed to be some 10's of nanoseconds, it's required.


# Notes to Infineon
ADC\_MEASUREMENT\_Extern.h to adc_measurement_extern.h in adc_measurement.h
DAVE\_common.h to DAVE\_Common.h in uart.h
Add a errata note to the XMC1100-2Go TLI4790 on the schematic to 
tell about the short circuits on the LED's.

