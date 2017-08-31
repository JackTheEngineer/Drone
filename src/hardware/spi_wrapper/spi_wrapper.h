#include "base.h"
#include "spi_master.h"

void SPI_transmit(const SPI_MASTER_t *const handle, uint8_t* data, uint32_t count);
void SPI_receive(const SPI_MASTER_t *const handle, uint8_t* data, uint32_t count);
