/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */
#include "timetasks.h"
#include "led_module.h"
#include "motor_pwm.h"
#include "uart.h"

uint8_t read_data = 0x0;

#define SPEEDS 5
static const uint32_t speeds[SPEEDS] = {
		100,
		300,
		500,
		800,
		999,
};

int32_t cut_index(int32_t *f, uint32_t bufsize){
	if(*f < 0){
		*f = bufsize - 1;
	}
	if(*f >= (bufsize)){
		*f = 0;
	}
	return *f;
}
void Change_speed(int32_t *frequ_index, Direction_t updown){
	if(frequ_index == NULL){
		return;
	}
	int32_t frequ;

	if(updown == UP){
		*frequ_index = *frequ_index + 1;
	}
	if(updown == DOWN){
		*frequ_index = *frequ_index - 1;
	}
	
	frequ = cut_index(frequ_index, SPEEDS);
	PWM_Motor_Set_Rate(speeds[frequ], 1);
	*frequ_index = frequ;
}

void TimeTasks_run(uint32_t ticks, OS_t *os){
	if((ticks % TIME5MS) == 0){
		if(button_readEdge(os->button_1) == RISING_EDGE){
			Change_speed(os->frequ_index, UP);
		}
		if(button_readEdge(os->button_2) == RISING_EDGE){
			Change_speed(os->frequ_index, DOWN);
		}

		UART_Receive(&UART_0, &read_data, 1);
		UART_Transmit(&UART_0, &read_data, 1);
		Motion_sensor_get_data(os->motion_sensor);
		read_data = 0x0;
	}
	if((ticks % TIME100MS) == 0){
		led_toggle(LED0);
	}
	if((ticks % TIME1S) == 0){
		led_toggle(LED1);
	}
}

