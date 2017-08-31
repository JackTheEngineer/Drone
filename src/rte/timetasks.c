/*
 * timetasks.c
 *
 *  Created on: Jul 11, 2017
 *      Author: chocolate
 */
#include "timetasks.h"
#include "led_module.h"
#include "motor_pwm.h"
#include "dbg_uart.h"

/* To be removed */
#include "spi_wrapper.h"

void Action_5ms(OS_t* os);

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

void Action_5ms(OS_t* os){
	uint8_t receive;
	if (button_readEdge(os->button_1) == RISING_EDGE) {
		Change_speed(os->frequ_index, UP);
	}
	if (button_readEdge(os->button_2) == RISING_EDGE) {
		Change_speed(os->frequ_index, DOWN);
	}

	uint8_t data = (READ|WHO_AM_I);
	SPI_transmit(&SPI_MASTER_0, &data, 1);
	SPI_receive(&SPI_MASTER_0, &receive, 1);
	DBG_Uart_send_num(&UART_0, receive);
}

void TimeTasks_run(uint32_t ticks, OS_t *os){
	if((ticks % TIME5MS) == 0){
		Action_5ms(os);
	}
	if((ticks % TIME100MS) == 0){
		led_toggle(LED0);
	}
	if((ticks % TIME1S) == 0){
		led_toggle(LED1);
	}
}

