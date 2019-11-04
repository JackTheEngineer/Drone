/*
 * rfm_spi.h
 *
 *  Created on: Nov 4, 2019
 *      Author: jakov
 */

#ifndef HARDWARE_XMC1100NEW_RFM_SPI_WRAPPER_RFM_SPI_H_
#define HARDWARE_XMC1100NEW_RFM_SPI_WRAPPER_RFM_SPI_H_

#include "base.h"

typedef enum _Disable_CE_{
	LEAVE_CSN_ENABLED,
	DISABLE_CSN,
}Disable_CSN_e;

void spi_rfm_send_bytes(uint8_t const *bytes,
		uint8_t bufsize,
		Disable_CSN_e ce_disable);
void spi_rfm_read_bytes(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e ce_disable);
void spi_rfm_read_bytes_no_cmd(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e ce_disable);

#endif /* HARDWARE_XMC1100NEW_RFM_SPI_WRAPPER_RFM_SPI_H_ */
