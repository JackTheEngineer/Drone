/*
 * rfm_spi.c
 *
 *  Created on: Nov 4, 2019
 *      Author: jakov
 */

#include "rfm_spi.h"
#include "hardware.h"

_STATIC_ _INLINE_ void _spi_rfm_wait_for_receive(void);
_STATIC_ _INLINE_ void _spi_rfm_clear_receive_indication(void);
_STATIC_ _INLINE_ void _spi_rfm_send_byte(uint8_t byte);


void spi_rfm_send_bytes(uint8_t const *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){

	SPI_MASTER_SetMode(&RFM75_SPI, XMC_SPI_CH_MODE_STANDARD);
	SPI_MASTER_EnableSlaveSelectSignal(&RFM75_SPI,
			RFM75_SPI.config->slave_select_pin_config[0]->slave_select_ch);

	for(uint8_t i=0; i<bufsize; i++){
		_spi_rfm_send_byte(bytes[i]);
	}

	if(csn_disable){
		SPI_MASTER_DisableSlaveSelectSignal(&RFM75_SPI);
	}
}
_STATIC_ _INLINE_ void _spi_rfm_send_byte(uint8_t byte){
	_spi_rfm_clear_receive_indication();
	SPI_MASTER_Transmit(&RFM75_SPI, &byte, 1);
	_spi_rfm_wait_for_receive();
}

void spi_rfm_read_bytes_no_cmd(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){
	/* Change SPI  Channel configuration */
	SPI_MASTER_SetMode(&RFM75_SPI, XMC_SPI_CH_MODE_STANDARD);
	SPI_MASTER_EnableSlaveSelectSignal(&RFM75_SPI,
			RFM75_SPI.config->slave_select_pin_config[0]->slave_select_ch);

	_spi_rfm_clear_receive_indication();
	/* Receive the status command from the  SPI flash chip */
	SPI_MASTER_Receive(&RFM75_SPI, bytes, bufsize);
	while(RFM75_SPI.runtime->rx_busy);

	if(csn_disable){
		/* Disable the Slave Select Line */
		SPI_MASTER_DisableSlaveSelectSignal(&RFM75_SPI);
	}
}

void spi_rfm_read_bytes(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){

	/* Change SPI  Channel configuration */
	SPI_MASTER_SetMode(&RFM75_SPI, XMC_SPI_CH_MODE_STANDARD);
	SPI_MASTER_EnableSlaveSelectSignal(&RFM75_SPI,
			RFM75_SPI.config->slave_select_pin_config[0]->slave_select_ch);

	_spi_rfm_clear_receive_indication();

	/* Send the read status register command to SPI flash chip */
	SPI_MASTER_Transmit(&RFM75_SPI, bytes, 1);
	while(RFM75_SPI.runtime->tx_busy);

	_spi_rfm_wait_for_receive();

	_spi_rfm_clear_receive_indication();
	/* Receive the status command from the  SPI flash chip */
	SPI_MASTER_Receive(&RFM75_SPI, &bytes[1], bufsize-1);
	while(RFM75_SPI.runtime->rx_busy);

	if(csn_disable){
		/* Disable the Slave Select Line */
		SPI_MASTER_DisableSlaveSelectSignal(&RFM75_SPI);
	}
}

_STATIC_ _INLINE_ void _spi_rfm_wait_for_receive(void){
	uint32_t status1;
	uint32_t status2;
	do
	{
		status1 = SPI_MASTER_GetFlagStatus(&RFM75_SPI, XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
		status2 = SPI_MASTER_GetFlagStatus(&RFM75_SPI, XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
	} while(((status1 == 0) && (status2 == 0)));
}

_STATIC_ _INLINE_ void _spi_rfm_clear_receive_indication(void){
	/* Clear the flags */
	SPI_MASTER_ClearFlag(&RFM75_SPI, XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
	SPI_MASTER_ClearFlag(&RFM75_SPI, XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
}
