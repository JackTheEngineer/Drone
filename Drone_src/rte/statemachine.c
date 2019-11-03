/*
 * statemachine.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#include "timetasks.h"
#include "led_module.h"
#include "motors.h"
#include "i2c_master.h"
#include "RFM75_driver.h"
#include "delay.h"
#include "rc_control.h"
#include "os.h"
#include "uart.h"
#include "pid_controller.h"
#include "serialize_vector.h"
#include "byte_formatting.h"

#define OFFSET_MEASUREMENTS 100

uint32_t offset_count = 0;
Sensordata_t offset_measurements[OFFSET_MEASUREMENTS];
Sensordata_t start_offset;
Vector_i32_t angular_position;
Vector_i32_t angular_speeds[3];
uint32_t integrator_count = 0;
uint32_t rc_no_data_receive_count = 0;

typedef Vector_i32_t* (*Selector_func)(void *target, uint32_t index);


void State_Run(uint32_t ticks, OS_t *os);
void State_Calibrate(uint32_t ticks, OS_t *os);

void average_i32_vector_with_selector(void *data, Selector_func selector, uint32_t length, Vector_i32_t *result);
Vector_i32_t* select_acceleration(void *sensordata, uint32_t index);
Vector_i32_t* select_angle_speed(void *sensordata, uint32_t index);
Vector_i32_t* select_magnetic_field(void *sensordata, uint32_t index);



void Statemachine_do(uint32_t ticks, OS_t *os){
	switch(*(os->current_state)){
		case STATE_INITIALIZE:
			break;
		case STATE_CALIBRATE:
			State_Calibrate(ticks, os);
			break;
		case STATE_RUN:
			State_Run(ticks, os);
			break;
		default:
			break;
	}
}

void State_Calibrate(uint32_t ticks, OS_t *os){
	if((ticks % 2) == 0){
		Motion_sensor_get_data(&offset_measurements[offset_count]);
		offset_count++;
		if(offset_count >= OFFSET_MEASUREMENTS){
			average_i32_vector_with_selector(offset_measurements, select_acceleration, OFFSET_MEASUREMENTS, &start_offset.acceleration);
			average_i32_vector_with_selector(offset_measurements, select_angle_speed, OFFSET_MEASUREMENTS, &start_offset.angle_speed);
			average_i32_vector_with_selector(offset_measurements, select_magnetic_field, OFFSET_MEASUREMENTS, &start_offset.magnetic_field);
			offset_count = 0;
			*os->current_state = STATE_RUN;
		}
	}
}

void State_Run(uint32_t ticks, OS_t *os){
	uint8_t received_bytes[32];
	uint8_t received_length;
	POINTER_TO_CONTAINER(RC_Data_t, rc_data);
	POINTER_TO_CONTAINER(Motorcontrolvalues_t, motors);
	POINTER_TO_CONTAINER(Vector_i32_t, helper_speed);
	uint8_t send_bytes[37];
	format_set_u8_buf_to(0, send_bytes, 36);
	send_bytes[36] = '\n';

	if((ticks % 20) == 0){
		received_length = RFM75_Receive_bytes(received_bytes);
		if(received_length == 0){
			rc_no_data_receive_count++;
		}
		delay_ms(2);
		Vect_i32_set_all_values_to(helper_speed, 0);
		Motion_sensor_get_data(os->motion_sensor);
		Vect_i32_times_const(&start_offset.angle_speed, -1, helper_speed);
		Vect_i32_add(&os->motion_sensor->angle_speed, helper_speed, helper_speed);
		integrator_count++;

		/* Now helper speed has the value without offset */
		Vect_i32_copy_from_to(helper_speed, &angular_speeds[integrator_count]);

		if(integrator_count >= 2){
			Vect_i32_times_const(&angular_speeds[1], 4, helper_speed);
			Vect_i32_add_to(helper_speed, &angular_speeds[2]);
			Vect_i32_add_to(helper_speed, &angular_speeds[0]);

			Vect_i32_div_by_const(helper_speed, 6, helper_speed);
			Vect_i32_add_to(&angular_position, helper_speed);

			Vect_i32_copy_from_to(&angular_speeds[2], &angular_speeds[0]);
			integrator_count = 0;
		}


		if(received_length == 16){
			RC_Control_decode_message(received_bytes, rc_data);
			Motors_set_all_data_speed(motors, rc_data->throttle/4);
			led_toggle(LED1);
			rc_no_data_receive_count = 0;
		}
		if(rc_no_data_receive_count > 20){
			Motors_set_all_data_speed(motors, 0);
		}
		Motors_act_on_pwm(motors);

		serialize_vector_as_i32(&os->motion_sensor->acceleration, &send_bytes[0]);
		serialize_vector_as_i32(&os->motion_sensor->angle_speed, &send_bytes[12]);
		serialize_vector_as_i32(&os->motion_sensor->magnetic_field, &send_bytes[24]);
		UART_Transmit(&DebugUart, send_bytes, 37);
	}
	if((ticks % 500) == 0){
		led_toggle(LED1);
	}
}

void average_i32_vector_with_selector(void *data, Selector_func selector, uint32_t length, Vector_i32_t *result){
	Vector_i32_t copy_vector_list[length];

	for(uint32_t i = 0; i < length; i++){
		Vect_i32_copy_from_to(selector(data, i), &copy_vector_list[i]);
	}
	Vect_i32_sum_up_list_of_vectors(copy_vector_list, result, OFFSET_MEASUREMENTS);
	Vect_i32_div_by_const(result, length, result);
}

Vector_i32_t* select_acceleration(void *sensordata, uint32_t index){
	return &(((Sensordata_t*)sensordata)[index].acceleration);
}
Vector_i32_t* select_angle_speed(void *sensordata, uint32_t index){
	return &(((Sensordata_t*)sensordata)[index].angle_speed);
}
Vector_i32_t* select_magnetic_field(void *sensordata, uint32_t index){
	return &(((Sensordata_t*)sensordata)[index].magnetic_field);
}


