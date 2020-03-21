
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
	uint8_t bytes_position = 0;
	uint16_t joystick_values[4];
	format_u8buf_to_four_ui12(received_bytes, joystick_values);
	bytes_position = 6;
	rc_data->throttle = joystick_values[3];
	rc_data->rotation = joystick_values[2] - 2048;
	rc_data->x_tilt = joystick_values[1] - 2048;
	rc_data->y_tilt = joystick_values[0] - 2048;


	float first_float;
	first_float = format_u8_buf_to_float(&received_bytes[bytes_position]);
	if(first_float != 0.0){
		control_params->P = first_float;
		bytes_position += 4; // 10
		control_params->D = format_u8_buf_to_float(&received_bytes[bytes_position]);
		bytes_position += 4; // 14
		control_params->I = format_u8_buf_to_float(&received_bytes[bytes_position]);
		bytes_position += 4; // 18
		led_toggle(_LED1);
	}
}
