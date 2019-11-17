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

#define XG_OFFSET_H 19
#define XG_OFFSET_L 20
#define YG_OFFSET_H 21
#define YG_OFFSET_L 22
#define ZG_OFFSET_H 23
#define ZG_OFFSET_L 24


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
#define MSU_CONFIG_REG 26
#define MSU_GYRO_CONFIG_REG 27
#define MSU_ACCEL_CONFIG_REG 28

#define MSU_FIFO_ENABLE_REG 35
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

//Magnetometer Registers
#define AK8963_WHO_AM_I  0x00 // should return 0x48
#define AK8963_INFO      0x01
#define AK8963_ST1       0x02  // data ready status bit 0
#define AK8963_XOUT_L	 0x03  // data
#define AK8963_XOUT_H	 0x04
#define AK8963_YOUT_L	 0x05
#define AK8963_YOUT_H	 0x06
#define AK8963_ZOUT_L	 0x07
#define AK8963_ZOUT_H	 0x08
#define AK8963_ST2       0x09  // Data overflow bit 3 and data read error status bit 2
#define AK8963_CNTL      0x0A  // Power down (0000), single-measurement (0001), self-test (1000) and Fuse ROM (1111) modes on bits 3:0
#define AK8963_ASTC      0x0C  // Self test control
#define AK8963_I2CDIS    0x0F  // I2C disable
#define AK8963_ASAX      0x10  // Fuse ROM x-axis sensitivity adjustment value
#define AK8963_ASAY      0x11  // Fuse ROM y-axis sensitivity adjustment value
#define AK8963_ASAZ 0x12 // Fuse ROM z-axis sensitivity adjustment value

#define AK8963_ADDRESS (0x0C << 1) //  Address of magnetometer

/* At max sensitivity */
#define GYRO_SENSITIVITY 131 /* LSB for 1 Deg/sec */
/* At max sensitivity */
#define ACCEL_SENSITIVITY 16384 /* LSB for 1 g = 9.81 m/s^2 */

uint8_t Motionsensor_I2C_readByte(uint8_t i2c_addr, uint8_t address);
void Motionsensor_I2C_readBytesFromi2c_addr(uint8_t i2c_addr, uint8_t startaddress,
		uint8_t *read_buf, uint8_t size);
void Motionsensor_I2C_writeByte(uint8_t address, uint8_t value);
void Motionsensor_I2C_writeByteToi2c_addr(uint8_t i2c_addr, uint8_t address, uint8_t value);
void Motionsensor_I2C_readBytes(uint8_t startaddress, uint8_t *read_buf, uint8_t size);
void Motionsensor_Init(void);


#endif /* SRC_APPLICATION_SENSOR_MSENSOR_IFACE_H_ */
