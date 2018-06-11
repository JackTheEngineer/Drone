/*
 * rc_control.h
 *
 *  Created on: Apr 22, 2016
 *      Author: jakov
 */

#ifndef RC_CONTROL_H_
#define RC_CONTROL_H_

#include "base.h"

typedef struct Flight_Data{
	uint16_t throttle;
	int32_t rotation;
	int32_t x_tilt;
	int32_t y_tilt;
}RC_Data_t;

void RC_Control_decode_message(uint8_t *received_bytes, RC_Data_t* rc_data);

#endif /* RC_CONTROL_H_ */
