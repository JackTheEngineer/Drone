/*
 * RC_SPI_Iface.c
 *
 *  Created on: Oct 19, 2017
 *      Author: chocolate
 */

#include "RFM75_Iface.h"
#include "spi_master.h"
#include "xmc_gpio.h"

#define CE_PORT XMC_GPIO_PORT5
#define CE_PIN 4


void _RC_Iface_wait_for_receive(void);
void _RC_Iface_clear_receive_indication(void);
void RC_Iface_send_byte(uint8_t byte);

_STATIC_ XMC_GPIO_CONFIG_t gpio_config = {
    .mode = XMC_GPIO_MODE_OUTPUT_PUSH_PULL,
    .output_level = XMC_GPIO_OUTPUT_LEVEL_LOW,
};

void RC_Iface_init(void){
	XMC_GPIO_Init(CE_PORT, CE_PIN, &gpio_config);
	SPI_MASTER_Init(&SPI_MASTER_0);
	PIN_INTERRUPT_Init(&RFM75_IRQ_PIN);
}


void RC_Iface_send_bytes(uint8_t const *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){

	SPI_MASTER_EnableSlaveSelectSignal(&SPI_MASTER_0,
			SPI_MASTER_0.config->slave_select_pin_config[0]->slave_select_ch);

	SPI_MASTER_SetMode(&SPI_MASTER_0, XMC_SPI_CH_MODE_STANDARD);

	for(uint8_t i=0; i<bufsize; i++){
		RC_Iface_send_byte(bytes[i]);
	}

	if(csn_disable){
		SPI_MASTER_DisableSlaveSelectSignal(&SPI_MASTER_0);
	}
}

void RC_Iface_send_byte(uint8_t byte){
	_RC_Iface_clear_receive_indication();
	SPI_MASTER_Transmit(&SPI_MASTER_0, &byte, 1);
	_RC_Iface_wait_for_receive();
}

void RC_Iface_read_bytes_no_cmd(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){
	/* Change SPI  Channel configuration */
	SPI_MASTER_SetMode(&SPI_MASTER_0, XMC_SPI_CH_MODE_STANDARD);
	SPI_MASTER_EnableSlaveSelectSignal(&SPI_MASTER_0,
			SPI_MASTER_0.config->slave_select_pin_config[0]->slave_select_ch);

	_RC_Iface_clear_receive_indication();
	/* Receive the status command from the  SPI flash chip */
	SPI_MASTER_Receive(&SPI_MASTER_0, bytes, bufsize);
	while(SPI_MASTER_0.runtime->rx_busy);

	if(csn_disable){
		/* Disable the Slave Select Line */
		SPI_MASTER_DisableSlaveSelectSignal(&SPI_MASTER_0);
	}
}

void RC_Iface_read_bytes(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e csn_disable){

	/* Change SPI  Channel configuration */
	SPI_MASTER_SetMode(&SPI_MASTER_0, XMC_SPI_CH_MODE_STANDARD);
	SPI_MASTER_EnableSlaveSelectSignal(&SPI_MASTER_0,
			SPI_MASTER_0.config->slave_select_pin_config[0]->slave_select_ch);

	_RC_Iface_clear_receive_indication();

	/* Send the read status register command to SPI flash chip */
	SPI_MASTER_Transmit(&SPI_MASTER_0, bytes, 1);
	while(SPI_MASTER_0.runtime->tx_busy);

	_RC_Iface_wait_for_receive();

	_RC_Iface_clear_receive_indication();
	/* Receive the status command from the  SPI flash chip */
	SPI_MASTER_Receive(&SPI_MASTER_0, &bytes[1], bufsize-1);
	while(SPI_MASTER_0.runtime->rx_busy);

	if(csn_disable){
		/* Disable the Slave Select Line */
		SPI_MASTER_DisableSlaveSelectSignal(&SPI_MASTER_0);
	}
}

void _RC_Iface_wait_for_receive(void){
	uint32_t status1;
	uint32_t status2;
	do
	{
		status1 = SPI_MASTER_GetFlagStatus(&SPI_MASTER_0, XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
		status2 = SPI_MASTER_GetFlagStatus(&SPI_MASTER_0, XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
	} while(((status1 == 0) && (status2 == 0)));
}

void _RC_Iface_clear_receive_indication(void){
	/* Clear the flags */
	SPI_MASTER_ClearFlag(&SPI_MASTER_0, XMC_SPI_CH_STATUS_FLAG_ALTERNATIVE_RECEIVE_INDICATION);
	SPI_MASTER_ClearFlag(&SPI_MASTER_0, XMC_SPI_CH_STATUS_FLAG_RECEIVE_INDICATION);
}

void RC_Iface_CE_high(void){
	XMC_GPIO_SetOutputHigh(CE_PORT, CE_PIN);
}

void RC_Iface_CE_low(void){
	XMC_GPIO_SetOutputLow(CE_PORT, CE_PIN);
}

