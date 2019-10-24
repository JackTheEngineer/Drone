# Hardware configuration

XMC4500:
For the selection of the SPI Type use 'low if inactive, transmit on falling edge, receive on rising edge' -> which generates: `XMC_SPI_CH_BRG_SHIFT_CLOCK_PASSIVE_LEVEL_0_DELAY_ENABLED`


On RFM75 SPI use no Interword - Delay, and at least some Chip Select delay ( i used 4 clock cycles, -> 2 uS). Even though, the RFM75 Manual says it's supposed to be some 10's of nanoseconds, 
it's required.
