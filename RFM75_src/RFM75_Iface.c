/*
 * RC_SPI_Iface.c
 *
 *  Created on: Oct 19, 2017
 *      Author: chocolate
 */

#include "RFM75_Iface.h"
#include "spi_master.h"
#include "xmc_gpio.h"
#include "hardware.h"


void _RC_Iface_wait_for_receive(void);
void _RC_Iface_clear_receive_indication(void);
void RC_Iface_send_byte(uint8_t byte);

void RC_Iface_init(void){
	DIGITAL_IO_Init(&RFM75_CE_PIN);
	SPI_MASTER_Init(&RFM75_SPI);
	PIN_INTERRUPT_Init(&RFM75_INTERRUPT_PIN);
}


void RC_Iface_send_bytes(uint8_t const *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){

	SPI_MASTER_SetMode(&RFM75_SPI, XMC_SPI_CH_MODE_STANDARD);
	SPI_MASTER_EnableSlaveSelectSignal(&RFM75_SPI,
			RFM75_SPI.config->slave_select_pin_config[0]->slave_select_ch);

	_RC_Iface_clear_receive_indication();
	for(uint8_t i=0; i<bufsize; i++){
		SPI_MASTER_Transmit(&RFM75_SPI, &bytes[i], 1);
	}
	_RC_Iface_wait_for_receive();
	_RC_Iface_clear_receive_indication();

	if(csn_disable){
		SPI_MASTER_DisableSlaveSelectSignal(&RFM75_SPI);
	}
}
/*
void RC_Iface_send_byte(uint8_t byte){
	_RC_Iface_clear_receive_indication();
	SPI_MASTER_Transmit(&RFM75_SPI, &byte, 1);
	_RC_Iface_wait_for_receive();
} */

void RC_Iface_read_bytes_no_cmd(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){
	/* Change SPI  Channel configuration */
	SPI_MASTER_SetMode(&RFM75_SPI, XMC_SPI_CH_MODE_STANDARD);
	SPI_MASTER_EnableSlaveSelectSignal(&RFM75_SPI,
			RFM75_SPI.config->slave_select_pin_config[0]->slave_select_ch);

	_RC_Iface_clear_receive_indication();
	/* Receive the status command from the  SPI flash chip */
	SPI_MASTER_Receive(&RFM75_SPI, bytes, bufsize);
	while(RFM75_SPI.runtime->rx_busy);

	if(csn_disable){
		/* Disable the Slave Select Line */
		SPI_MASTER_DisableSlaveSelectSignal(&RFM75_SPI);
	}
}

void RC_Iface_read_bytes(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){

	/* Change SPI  Channel configuration */
	SPI_MASTER_SetMode(&RFM75_SPI, XMC_SPI_CH_MODE_STANDARD);
	SPI_MASTER_EnableSlaveSelectSignal(&RFM75_SPI,
			RFM75_SPI.config->slave_select_pin_config[0]->slave_select_ch);

	_RC_Iface_clear_receive_indication();

	/* Send the read status register command to SPI flash chip */
	SPI_MASTER_Transmit(&RFM75_SPI, bytes, 1);
	while(RFM75_SPI.runtime->tx_busy);

	_RC_Iface_wait_for_receive();

	_RC_Iface_clear_receive_indication();
	/* Receive the status command from the  SPI flash chip */
	SPI_MASTER_Receive(&RFM75_SPI, &bytes[1], bufsize-1);
	while(RFM75_SPI.runtime->rx_busy);

	if(csn_disable){
		/* Disable the Slave Select Line */
		SPI_MASTER_DisableSlaveSelectSignal(&RFM75_SPI);
	}
}

void _RC_Iface_wait_for_receive(void){
	uint32_t status1;
	uint32_t status2;
	do
	{
		status1 = SPI_MASTER_GetFlagStatus(&RFM75_SPI, XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
		status2 = SPI_MASTER_GetFlagStatus(&RFM75_SPI, XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
	} while(((status1 == 0) && (status2 == 0)));
}

void _RC_Iface_clear_receive_indication(void){
	/* Clear the flags */
	SPI_MASTER_ClearFlag(&RFM75_SPI, XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
	SPI_MASTER_ClearFlag(&RFM75_SPI, XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
}

void RC_Iface_CE_high(void){
	DIGITAL_IO_SetOutputHigh(&RFM75_CE_PIN);
}

void RC_Iface_CE_low(void){
	DIGITAL_IO_SetOutputLow(&RFM75_CE_PIN);
}

