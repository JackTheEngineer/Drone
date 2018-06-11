
/*
 * rc_control.c
 *
 *  Created on: Apr 22, 2016
 *      Author: jakov
 */

#include "rc_control.h"
#include "byte_formatting.h"

/* received_bytes should be at least of length 8 */
void RC_Control_decode_message(uint8_t *received_bytes, RC_Data_t* rc_data){
	rc_data->throttle = format_u8buf_to_u16(&received_bytes[0]);
	rc_data->rotation = (int16_t)format_u8buf_to_u16(&received_bytes[2]);
	rc_data->x_tilt = (int16_t)format_u8buf_to_u16(&received_bytes[4]);
	rc_data->y_tilt = (int16_t)format_u8buf_to_u16(&received_bytes[6]);
}



