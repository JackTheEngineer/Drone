/*
 * RC_SPI_Iface.c
 *
 *  Created on: Oct 19, 2017
 *      Author: chocolate
 */

#include "RFM75_hardware.h"
#include "rfm_spi.h"

volatile uint32_t rxtx_interrupt;

void RFM75_hardware_init(void){
	DIGITAL_IO_Init(&RFM75_CE_PIN);
	SPI_MASTER_Init(&RFM75_SPI);
	PIN_INTERRUPT_Init(&RFM75_INTERRUPT_PIN);
	PIN_INTERRUPT_SetEdgeSensitivity(&RFM75_INTERRUPT_PIN, PIN_INTERRUPT_EDGE_FALLING);
	PIN_INTERRUPT_Enable(&RFM75_INTERRUPT_PIN);
}

uint8_t RFM75_SPI_read_reg_value(uint8_t cmd)
{
	uint8_t readbytes[2] = {
			cmd,
			0
	};
	spi_rfm_read_bytes(readbytes, 2, DISABLE_CSN);
	return readbytes[1];
}

void RFM75_SPI_write_reg_val(uint8_t cmd, uint8_t val)
{
	uint8_t writebuf[2] = {cmd, val};
	spi_rfm_send_bytes(writebuf, 2, DISABLE_CSN);
}

void RFM75_SPI_read_buffer(uint8_t start_register, uint8_t * buf, uint8_t len)
{
	spi_rfm_send_bytes(&start_register, 1, LEAVE_CSN_ENABLED);
	spi_rfm_read_bytes_no_cmd(buf, len, DISABLE_CSN);
}

void RFM75_SPI_write_buffer(uint8_t * cmdbuf, uint8_t len)
{
	spi_rfm_send_bytes(cmdbuf, len, DISABLE_CSN);
}

void RFM75_SPI_write_buffer_at_start_register(uint8_t start_register,
												const uint8_t * buf,
												uint8_t len)
{
	spi_rfm_send_bytes(&start_register, 1, LEAVE_CSN_ENABLED);
	spi_rfm_send_bytes(buf, len, DISABLE_CSN);
}

void RFM75_PinInterruptHandler(void){
	rxtx_interrupt = 1;
}

