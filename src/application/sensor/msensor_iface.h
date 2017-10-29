/*
 * msensor_iface.h
 *
 *  Created on: Sep 11, 2017
 *      Author: chocolate
 */

#ifndef SRC_APPLICATION_SENSOR_MSENSOR_IFACE_H_
#define SRC_APPLICATION_SENSOR_MSENSOR_IFACE_H_


#include "base.h"

/*
 * CODES
 */
#define I2C_IF_DIS (1<<4) /* reset/disable the I2C interface */
#define H_RESET (1<<7)
#define OPTIMAL_CLOCK (1)

#define GYRO_250DPS (00)
#define GYRO_500DPS (0b01 << 3)
#define GYRO_1000DPS (0b10 << 3)
#define GYRO_2000DPS (0b11 << 3)

#define ACCEL_2G (00)
#define ACCEL_4G (0b01 << 3)
#define ACCEL_8G (0b10 << 3)
#define ACCEL_16G (0b11 << 3)

#define TEMP_OUT_FIFO (1 << 7)
#define GYRO_XOUT_FIFO (1 << 6)
#define GYRO_YOUT_FIFO (1 << 5)
#define GYRO_ZOUT_FIFO (1 << 4)
#define ACCEL_FIFO (1 << 3)

/*
 * ADDRESSES
 */
#define CONFIG 26
#define GYRO_CONFIG 27
#define ACCEL_CONFIG 28

#define FIFO_ENABLE 35
#define ACCEL_XOUT_H 59
#define ACCEL_XOUT_L 60
#define ACCEL_YOUT_H 61
#define ACCEL_YOUT_L 62
#define ACCEL_ZOUT_H 63
#define ACCEL_ZOUT_L 64
#define TEMP_OUT_H 65
#define TEMP_OUT_L 66
#define GYRO_XOUT_H 67
#define GYRO_XOUT_L 68
#define GYRO_YOUT_H 69
#define GYRO_YOUT_L 70
#define GYRO_ZOUT_H 71
#define GYRO_ZOUT_L 72

#define USER_CTRL 106 /* User control register address */
#define PWR_MGMT_1 107
#define PWR_MGMT_2 108
#define WHO_AM_I 117

#define MPU9250_ADDRESS (0x68 << 1)

/* At max sensitivity */
#define GYRO_SENSITIVITY 131 /* LSB for 1 Deg/sec */
/* At max sensitivity */
#define ACCEL_SENSITIVITY 16384 /* LSB for 1 g = 9.81 m/s^2 */

uint8_t MSensor_Iface_readByte(uint8_t address);
void MSensor_Iface_writeByte(uint8_t address, uint8_t value);
void MSensor_Iface_readBytes(uint8_t startaddress, uint8_t *read_buf, uint8_t size);
void MSensor_Iface_Init(void);


#endif /* SRC_APPLICATION_SENSOR_MSENSOR_IFACE_H_ */
