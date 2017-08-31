/*
 * motion_sensor.c
 *
 *  Created on: Apr 13, 2016
 *      Author: jakov
 */

#include "motion_sensor.h"
#include "spi_wrapper.h"

int32_t SPI_read_double_value(const SPI_MASTER_t *const handle,
			      uint8_t high_code,
			      uint8_t low_code);

void Motion_sensor_init(Sensordata_t *sensordata){
	uint8_t disable_i2c[] = {
		((WRITE)|(USER_CTRL)),
		I2C_IF_DIS,
	};
	SPI_MASTER_Init(&SPI_MASTER_0);
	SPI_transmit(&SPI_MASTER_0, disable_i2c, 2);
	Motion_sensor_set_data_zero(sensordata);
}

void Motion_sensor_set_data_zero(Sensordata_t *sensordata){
	Vect_i32_set_all_values_to(&sensordata->acceleration, 0);
	Vect_i32_set_all_values_to(&sensordata->angle_speed, 0);
	Vect_i32_set_all_values_to(&sensordata->magnetic_field, 0);
}



void Motion_sensor_get_data(Sensordata_t *sensordata){
	if(sensordata == NULL){return;}
	int32_t value = SPI_read_double_value(&SPI_MASTER_0,
					      ACCEL_XOUT_H,
					      ACCEL_XOUT_L);
	Vect_i32_write(&sensordata->acceleration, 1, value);
}

int32_t SPI_read_double_value(const SPI_MASTER_t *const handle,
			      uint8_t high_code,
			      uint8_t low_code){
	if(handle == NULL){return 0;}
	uint8_t receive;
	int32_t value;
	uint8_t data = (READ | low_code);
	SPI_transmit(handle, &data, 1);
	SPI_MASTER_Receive(handle, &receive, 1);
	while(handle->runtime->rx_busy);
	value = receive;
	data = (READ | high_code);	
	SPI_transmit(handle, &data, 1);
	SPI_MASTER_Receive(handle, &receive, 1);
	value |= (receive << 8);
	while(handle->runtime->rx_busy);
	return value;
}
