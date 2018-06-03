/*
 * RFM75_driver.h
 *
 *  Created on: Oct 25, 2017
 *      Author: chocolate
 */

#ifndef SRC_INTERFACE_RFM75_DRIVER_H_
#define SRC_INTERFACE_RFM75_DRIVER_H_

#include "RFM75_codes.h"
#include "RFM75_Iface.h"

typedef enum __RFM75_RX_Status{
	RFM_RX_SUCCESS,
	RFM_RX_FAILURE,
}RFM75_Rx_Status_e;

#define DATASIZE_RFM75_TRANSMIT 8

void RFM75_Init(void);
void RFM75_Transmit_bytes(uint8_t *buffer, uint8_t size);
void RFM75_set_bank(uint8_t bank);
RFM75_Rx_Status_e _RFM75_Receive_bytes(uint8_t *buffer, uint8_t size);

#endif /* SRC_INTERFACE_RFM75_DRIVER_H_ */
