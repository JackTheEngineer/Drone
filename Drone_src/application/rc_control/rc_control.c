
/*
 * rc_control.c
 *
 *  Created on: Apr 22, 2016
 *      Author: jakov
 */

#include "rc_control.h"
#include "byte_manip.h"
#include "led_module.h"

/* received_bytes should be at least of length 8 */
void RC_Control_decode_message(uint8_t *received_bytes, RC_Data_t* rc_data, ControlParams_t *control_params){
	uint8_t position = 0;
	uint16_t joystick_values[4];
	format_u8buf_to_four_ui12(received_bytes, joystick_values);
	position = 6;
	rc_data->throttle = joystick_values[3];
	rc_data->rotation = joystick_values[2];
	rc_data->x_tilt = joystick_values[1];
	rc_data->y_tilt = joystick_values[0];


	float testfloat;
	testfloat = format_u8_buf_to_float(&received_bytes[position]);
	if(testfloat != 0.0){
		control_params->P = testfloat;
		position += 4;
		control_params->D = format_u8_buf_to_float(&received_bytes[position]);
		position += 4;
		led_toggle(_LED1);
	}
}
