/*
 * motion_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */

#include "motion_sensor.h"
#include "byte_manip.h"
#include "msensor_iface.h"

void Motion_sensor_init(Sensordata_t *sensordata){
	Motion_sensor_set_data_zero(sensordata);
	Motionsensor_Init();
}

void Motion_sensor_set_data_zero(Sensordata_t *sensordata){
	Vect_i32_set_all_values_to(&sensordata->acceleration, 0);
	Vect_i32_set_all_values_to(&sensordata->angle_speed, 0);
	Vect_i32_set_all_values_to(&sensordata->magnetic_field, 0);
}

void Motion_sensor_get_data(Sensordata_t *sensordata){
	if(sensordata == NULL){return;}
	uint8_t vals[7] = {0};
	Motionsensor_I2C_readBytes(ACCEL_XOUT_H, &vals[0], 6);
	Vect_i32_write(&sensordata->acceleration, 0, (int16_t)((((uint16_t)vals[0]) << 8)|(vals[1])));
	Vect_i32_write(&sensordata->acceleration, 1, (int16_t)((((uint16_t)vals[2]) << 8)|(vals[3])));
	Vect_i32_write(&sensordata->acceleration, 2, (int16_t)((((uint16_t)vals[4]) << 8)|(vals[5])));
	Motionsensor_I2C_readBytes(GYRO_XOUT_H, &vals[0], 6);
	Vect_i32_write(&sensordata->angle_speed, 0, (int16_t)((((uint16_t)vals[0]) << 8)|(vals[1])));
	Vect_i32_write(&sensordata->angle_speed, 1, (int16_t)((((uint16_t)vals[2]) << 8)|(vals[3])));
	Vect_i32_write(&sensordata->angle_speed, 2, (int16_t)((((uint16_t)vals[4]) << 8)|(vals[5])));
//	uint8_t readval = MSensor_Iface_readByte(AK8963_ADDRESS, AK8963_ST1);
//	if(readval & 0x1) { // wait for magnetometer data ready bit to be set
//		MSensor_Iface_readBytesFromi2c_addr(AK8963_ADDRESS, AK8963_XOUT_L, vals, 7);  // Read the six raw data and ST2 registers sequentially into data array
//		uint8_t c = vals[6]; // End data read by reading ST2 register
//		if(!(c & 0x08)) { // Check if magnetic sensor overflow set, if not then report data
//			Vect_i32_write(&sensordata->magnetic_field, 1, (int16_t)((((uint16_t)vals[0]) << 8)|(vals[1])));
//			Vect_i32_write(&sensordata->magnetic_field, 2, (int16_t)((((uint16_t)vals[2]) << 8)|(vals[3])));
//			Vect_i32_write(&sensordata->magnetic_field, 3, (int16_t)((((uint16_t)vals[4]) << 8)|(vals[5])));
//		}
//	}
}

void Motion_sensor_set_angular_speed_offset(Vector_i32_t *omega_offset){
	int16_t si16_val;
	uint8_t vals[6] = {0};
	for(uint8_t i=0; i < 3; i++){
		si16_val = (-1)*(int16_t)Vect_i32_read(omega_offset, i);
		format_u16_to_u8_highbyte_first(si16_val, &vals[i*2]);
	}

	for(uint8_t i=0; i < 6; i++){
		Motionsensor_I2C_writeByte((XG_OFFSET_H + i), vals[i]);
	}
}



       
