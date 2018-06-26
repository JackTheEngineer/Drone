/*
 * msensor_iface.c
 *
 *  Created on: Sep 11, 2017
 *      Author: chocolate
 */

#include "msensor_iface.h"
#include "i2c_master.h"
#include "delay.h"

uint32_t i2c_duration_count = 0;
#define MAX_COUNT 50000

void MSensor_Iface_Init(void){
	I2C_MASTER_Init(&I2C_MASTER_0);

	MSensor_Iface_writeByte(PWR_MGMT_1, (H_RESET));
	delay_ms(100);
	MSensor_Iface_writeByte(PWR_MGMT_1, (OPTIMAL_CLOCK));
	delay_ms(200);

	/* This enables the DLPF_CFG ( Digital lowpass filter config) to
	 * run at a Bandwidth of 8800Hz, With a delay of 64muS
	 * FCHOICE must be then set to
	 * | <1> | <0> |
	 * |  x  |  0  |
	 *
	 */
	MSensor_Iface_writeByte(CONFIG, 0x00);
	MSensor_Iface_writeByte(GYRO_CONFIG, GYRO_1000DPS);
	MSensor_Iface_writeByte(ACCEL_CONFIG, ACCEL_4G);
}

void MSensor_Iface_readBytes(uint8_t startaddress, uint8_t *read_buf, uint8_t size){
	I2C_MASTER_Transmit(&I2C_MASTER_0, true, MPU9250_ADDRESS, &startaddress, 1, false);
	i2c_duration_count = 0;
	while(I2C_MASTER_0.runtime->tx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return;
		}
	}
	I2C_MASTER_Receive(&I2C_MASTER_0, true, MPU9250_ADDRESS, read_buf, size, true, true);
	i2c_duration_count = 0;
	while(I2C_MASTER_0.runtime->rx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return;
		}
	}
}

void MSensor_Iface_writeByte(uint8_t address, uint8_t value){
	uint8_t sendbuf[2];
	sendbuf[0] = address;
	sendbuf[1] = value;
	I2C_MASTER_Transmit(&I2C_MASTER_0, true, MPU9250_ADDRESS, &sendbuf[0], 2, true);
	i2c_duration_count = 0;
	while(I2C_MASTER_0.runtime->tx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return;
		}
	}
}

uint8_t MSensor_Iface_readByte(uint8_t address){
	uint8_t receive;
	I2C_MASTER_Transmit(&I2C_MASTER_0, true, MPU9250_ADDRESS, &address, 1, false);
	i2c_duration_count = 0;
	while(I2C_MASTER_0.runtime->tx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return 0;
		}
	}
	I2C_MASTER_Receive(&I2C_MASTER_0, true, MPU9250_ADDRESS, &receive, 1, true, true);
	i2c_duration_count = 0;
	while(I2C_MASTER_0.runtime->rx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return 0;
		}
	}
	return receive;
}


