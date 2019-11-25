/*
 * rc_control.h
 *
 *  Created on: Apr 22, 2016
 *      Author: jakov
 */

#ifndef RC_CONTROL_H_
#define RC_CONTROL_H_

#include "base.h"
#include "control_params.h"

typedef struct RC_Data{
	uint16_t throttle;
	int16_t rotation;
	int16_t x_tilt;
	int16_t y_tilt;
}RC_Data_t;

void RC_Control_decode_message(uint8_t *received_bytes, RC_Data_t* rc_data);
void RC_Control_decode_PID(uint8_t *received_bytes, ControlParams_t *control_params);

#endif /* RC_CONTROL_H_ */
