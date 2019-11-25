/*
 * statemachine.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */

#include "control_loop.h"
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
#include "byte_manip.h"
#include "madgwickFilter.h"

#define OFFSET_MEASUREMENTS 200
#define PI 3.141592653589793f
#define IMU_TO_RAD ((PI*250.0f)/(32767.0f * 180.0f) *(3.0f/4.0f))

extern const AddressAndChannel_t default_RFM75_Addr;

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
			Motion_sensor_set_angular_speed_offset(&start_offset.angle_speed);
			offset_count = 0;
			*os->current_state = STATE_RUN;
			RFM75_startListening(&default_RFM75_Addr);
		}
	}
}

void State_Run(uint32_t ticks, OS_t *os){
	static uint32_t remembered_time_20ms;
	static uint32_t remembered_time_5ms;
	static uint8_t received_bytes[32] = {0};
	static Quaternion_t quaternion = {{1.0f, 0.0, 0.0, 0.0}};
	static uint8_t sendbytes[32]={0};
	static ControlParams_t control_params = {
			.P.v = {250.0, 0.0, 0.0},
			.I.v = {20.0, 0.0, 0.0},
			.D.v = {20.0, 0.0, 0.0},
			.sum_err.v = {0.0, 0.0, 0.0},
			.last_err.v = {0.0, 0.0, 0.0},
	};
	CombinedReg_t creg;


	STATIC_POINTER_TO_CONTAINER(RC_Data_t, remote_control_data);
	STATIC_POINTER_TO_CONTAINER(Motorcontrolvalues_t, motors);
	POINTER_TO_CONTAINER(Vector_t, acc);
	POINTER_TO_CONTAINER(Vector_t, omega);

	if(overflow_save_diff_u32(ticks, remembered_time_5ms) >= 5){
		Motion_sensor_get_data(os->motion_sensor);
		Vect_transform_i32_to_float(&os->motion_sensor->acceleration, acc);
		Vect_transform_i32_to_float_with_mult(&os->motion_sensor->angle_speed, omega, IMU_TO_RAD);
		MadgwickAHRSupdateIMU(omega, acc, &quaternion);
		format_float_buf_to_u8_buf(&quaternion.q[0], 4, sendbytes);
	}

	if(overflow_save_diff_u32(ticks, remembered_time_20ms) >= 20){
		remembered_time_20ms = ticks;

		creg = RFM75_Receive_bytes_feedback(received_bytes);
		if(creg.length == 0){
			rc_no_data_receive_count++;
		}
		if(creg.length == 32){
			led_toggle(_LED2);
			RC_Control_decode_message(received_bytes, remote_control_data);
			RC_Control_decode_PID(received_bytes, &control_params);
			rc_no_data_receive_count = 0;
		}

		ControlLoop_run(&quaternion, &control_params, remote_control_data, motors);
		format_u16buf_to_u8buf(motors->motorspeeds, NMBR_OF_MOTORS, &sendbytes[16]);
		RFM75_SPI_write_buffer_at_start_register(W_ACK_PAYLOAD(0), sendbytes, 32);

		if(rc_no_data_receive_count > 15){
			Motors_set_all_data_speed(motors, 0);
			remote_control_data->throttle = 0;
			Motors_act_on_pwm(motors);
		}
		Motors_act_on_pwm(motors);
	}
}

void average_i32_vector_with_selector(void *data, Selector_func selector,
		uint32_t length, Vector_i32_t *result){
	Vector_i32_t sumvect;

	for(uint32_t i = 0; i < length; i++){
		Vect_i32_add_to(&sumvect, selector(data, i));
	}
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



