/*
 * msensor_iface.c
 *
 *  Created on: Sep 11, 2017
 *      Author: chocolate
 */

#include "msensor_iface.h"

#include "i2c_master.h"
#include "delay.h"

uint8_t Mscale = 1; // Choose either 14-bit or 16-bit magnetometer resolution
uint8_t Mmode = 0x02; // 2 for 8 Hz, 6 for 100 Hz continuous magnetometer data read
uint32_t i2c_duration_count = 0;
#define MAX_COUNT 150000

void Motionsensor_Init(void){
	// I2C_MASTER_Init(&MotionSensor_I2C);

	Motionsensor_I2C_writeByte(PWR_MGMT_1, (H_RESET));
	delay_ms(100);
	Motionsensor_I2C_writeByte(PWR_MGMT_1, (OPTIMAL_CLOCK));
	delay_ms(200);

	/* This enables the DLPF_CFG ( Digital lowpass filter config) to
	 * run at a Bandwidth of 8800Hz, With a delay of 64muS
	 * FCHOICE must be then set to
	 * | <1> | <0> |
	 * |  x  |  0  |
	 *
	 */
	Motionsensor_I2C_writeByte(MSU_CONFIG_REG, 0x00);
	Motionsensor_I2C_writeByte(MSU_GYRO_CONFIG_REG, GYRO_1000DPS);
	Motionsensor_I2C_writeByte(MSU_ACCEL_CONFIG_REG, ACCEL_4G);

	//MSensor_Iface_writeByteToi2c_addr(AK8963_ADDRESS, AK8963_CNTL, Mscale << 4 | Mmode);
}

void Motionsensor_I2C_readBytes(uint8_t startaddress, uint8_t *read_buf, uint8_t size){
	Motionsensor_I2C_readBytesFromi2c_addr(MPU9250_ADDRESS, startaddress, read_buf, size);
}


void Motionsensor_I2C_writeByte(uint8_t address, uint8_t value){
	Motionsensor_I2C_writeByteToi2c_addr(MPU9250_ADDRESS, address, value);
}

void Motionsensor_I2C_readBytesFromi2c_addr(uint8_t i2c_addr, uint8_t startaddress,
		uint8_t *read_buf, uint8_t size){
	I2C_MASTER_Transmit(&MotionSensor_I2C, true, i2c_addr, &startaddress, 1, false);
	i2c_duration_count = 0;
	while(MotionSensor_I2C.runtime->tx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return;
		}
	}
	I2C_MASTER_Receive(&MotionSensor_I2C, true, i2c_addr, read_buf, size, true, true);
	i2c_duration_count = 0;
	while(MotionSensor_I2C.runtime->rx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return;
		}
	}
}


void Motionsensor_I2C_writeByteToi2c_addr(uint8_t i2c_addr, uint8_t address, uint8_t value){
	uint8_t sendbuf[2];
	sendbuf[0] = address;
	sendbuf[1] = value;
	I2C_MASTER_Transmit(&MotionSensor_I2C, true, i2c_addr, &sendbuf[0], 2, true);
	i2c_duration_count = 0;
	while(MotionSensor_I2C.runtime->tx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return;
		}
	}
}

uint8_t Motionsensor_I2C_readByte(uint8_t i2c_addr, uint8_t address){
	uint8_t receive;
	I2C_MASTER_Transmit(&MotionSensor_I2C, true, i2c_addr, &address, 1, false);
	i2c_duration_count = 0;
	while(MotionSensor_I2C.runtime->tx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return 0;
		}
	}
	I2C_MASTER_Receive(&MotionSensor_I2C, true, i2c_addr, &receive, 1, true, true);
	i2c_duration_count = 0;
	while(MotionSensor_I2C.runtime->rx_busy){
		i2c_duration_count++;
		if(i2c_duration_count > MAX_COUNT){
			return 0;
		}
	}
	return receive;
}


