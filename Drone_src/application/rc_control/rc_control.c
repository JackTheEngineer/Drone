
/*
 * rc_control.c
 *
 *  Created on: Apr 22, 2016
 *      Author: jakov
 */

#include "rc_control.h"
#include "byte_manip.h"


/* received_bytes should be at least of length 8 */
void RC_Control_decode_message(uint8_t *received_bytes, RC_Data_t* rc_data){
	uint16_t joystick_values[4];
	format_u8buf_to_four_ui12(received_bytes, joystick_values);
	rc_data->throttle = joystick_values[0];
	rc_data->rotation = joystick_values[1];
	rc_data->x_tilt = joystick_values[2];
	rc_data->y_tilt = joystick_values[3];
}



