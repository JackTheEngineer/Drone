/*
 * RFM75_driver.h
 *
 *  Created on: Oct 25, 2017
 *      Author: chocolate
 */

#ifndef SRC_INTERFACE_RFM75_DRIVER_H_
#define SRC_INTERFACE_RFM75_DRIVER_H_

#include "RFM75_codes.h"
#include "radio_module.h"
#include "RFM75_hardware.h"

typedef enum __RFM75_RX_Status{
	RFM_RX_SUCCESS,
	RFM_RX_FAILURE,
}RFM75_Rx_Status_e;

bool RFM75_Init(void);
CombinedReg_t RFM75_Transmit_bytes(const uint8_t *buff,
				    const uint8_t length,
				    const uint32_t maxTimeoutUs,
					uint8_t *receive_payload, /* can be NULL, if expected_ack_data == false */
				    bool expected_ack_data);
void RFM75_set_bank(uint8_t bank);
RFM75_Rx_Status_e RFM75_Receive_bytes(uint8_t buffer[32]);

#endif /* SRC_INTERFACE_RFM75_DRIVER_H_ */
