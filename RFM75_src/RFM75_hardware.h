/*
 * RC_SPI_Iface.h
 *
 *  Created on: Oct 19, 2017
 *      Author: chocolate
 */

#ifndef SRC_INTERFACE_RC_SPI_IFACE_H_
#define SRC_INTERFACE_RC_SPI_IFACE_H_

#include "base.h"
#include "hardware.h"

void RFM75_hardware_init(void);

uint8_t RFM75_SPI_read_reg_value(uint8_t cmd);
void RFM75_SPI_write_reg_val(uint8_t cmd, uint8_t val);
void RFM75_SPI_read_buffer(uint8_t start_register, uint8_t * buf, uint8_t len);
void RFM75_SPI_write_buffer(uint8_t * cmdbuf, uint8_t len);
void RFM75_SPI_write_buffer_at_start_register(uint8_t start_register,
												const uint8_t * buf,
												uint8_t len);

_STATIC_ _INLINE_ void RFM75_CE_PIN_high(void){
	DIGITAL_IO_SetOutputHigh(&RFM75_CE_PIN);
}

_STATIC_ _INLINE_ void RFM75_CE_PIN_low(void){
	DIGITAL_IO_SetOutputLow(&RFM75_CE_PIN);
}

#endif /* SRC_INTERFACE_RC_SPI_IFACE_H_ */
