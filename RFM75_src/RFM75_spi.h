/*
 * RC_SPI_Iface.h
 *
 *  Created on: Oct 19, 2017
 *      Author: chocolate
 */

#ifndef SRC_INTERFACE_RC_SPI_IFACE_H_
#define SRC_INTERFACE_RC_SPI_IFACE_H_

#include "base.h"
#include "pin_interrupt.h"
#include "radio_module.h"

typedef enum _Disable_CE_{
	LEAVE_CSN_ENABLED,
	DISABLE_CSN,
}Disable_CSN_e;

void RFM75_hardware_init(void);
void RC_Iface_send_bytes(uint8_t const *bytes,
		uint8_t bufsize,
		Disable_CSN_e ce_disable);
void RC_Iface_read_bytes(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e ce_disable);
void RC_Iface_read_bytes_no_cmd(uint8_t *bytes,
		uint8_t bufsize,
		Disable_CSN_e ce_disable);
void RC_Iface_toggle_CE(void);
void RFM75_CE_PIN_high(void);
void RFM75_CE_PIN_low(void);


#endif /* SRC_INTERFACE_RC_SPI_IFACE_H_ */
