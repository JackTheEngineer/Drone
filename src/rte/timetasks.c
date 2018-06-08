/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */
#include "timetasks.h"
#include "led_module.h"
#include "motor_pwm.h"
#include "i2c_master.h"
#include "RFM75_driver.h"
#include "delay.h"

void TimeTasks_run(uint32_t ticks, OS_t *os){
	uint8_t sendbytes[32];
	uint16_t speedval = 0;
	uint8_t length;

	if((ticks % 10) == 0){
		length = RFM75_Receive_bytes(sendbytes);
		if(length != 0){
			speedval = ((uint16_t)sendbytes[1] << 8) | sendbytes[0];
			led_toggle(LED0);
		}
	}
	if((ticks % 500) == 0){
		led_toggle(LED1);
	}
}

