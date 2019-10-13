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
#include "radio_module.h"

typedef enum __RFM75_RX_Status{
	RFM_RX_SUCCESS,
	RFM_RX_FAILURE,
}RFM75_Rx_Status_e;

#define DATASIZE_RFM75_TRANSMIT ((const uint32_t) 8)

bool RFM75_Init(void);
TransmitResult_t RFM75_Transmit_bytes(const uint8_t *buff,
				    const uint32_t *length,
				    const uint32_t maxTimeoutUs,
				    bool requestAck);
void RFM75_set_bank(uint8_t bank);
RFM75_Rx_Status_e _RFM75_Receive_bytes(uint8_t buffer[32]);

#endif /* SRC_INTERFACE_RFM75_DRIVER_H_ */
