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

void Action_5ms(OS_t* os);

#define SPEEDS 5
static const uint32_t speeds[SPEEDS] = {
		10,
		15,
		20,
		30,
		40,
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
	if (button_readEdge(os->button_1) == RISING_EDGE) {
		Change_speed(os->frequ_index, UP);
	}
	if (button_readEdge(os->button_2) == RISING_EDGE) {
		Change_speed(os->frequ_index, DOWN);
	}
	//Motion_sensor_get_data(os->motion_sensor);
	//DBG_Uart_send_num(&UART_0, (uint32_t)receive);
}

void Wechsle_Motor(void){
	static uint8_t motorzahl = 0;
	uint8_t stopzahl = 3;

	/* startet motor 0, und stoppe motor 3 */
	/*starte motor 1, und stoppe motor 0 */
	PWM_Motor_Set_Rate(300, motorzahl);

	if(motorzahl == 0){
		stopzahl = 3;
	}else{
		stopzahl = motorzahl-1;
	}
	
	PWM_Motor_Set_Rate(0, stopzahl);

	motorzahl = motorzahl+1;

	if(motorzahl >= 4){
			motorzahl = 0;
	}
}

void TimeTasks_run(uint32_t ticks, OS_t *os){
	uint8_t sendbytes[32];
	uint8_t length;
	uint16_t value = 0;

	if((ticks % 10) == 0){

		length = RFM75_Receive_bytes(sendbytes);
		if(length == 16){
			value = ((uint16_t)sendbytes[1] << 8)| (sendbytes[0]);
			led_toggle(LED1);
			PWM_Motor_Set_Rate(value/4, 0);
			PWM_Motor_Set_Rate(value/4, 1);
			PWM_Motor_Set_Rate(value/4, 2);
			PWM_Motor_Set_Rate(value/4, 3);
		}

		Motion_sensor_get_data(os->motion_sensor);
		asm("NOP");
	}
	if((ticks % TIME100MS) == 0){

	}
	if((ticks % TIME1S) == 0){
	}
	if((ticks % TIME2S) == 0){
		//Wechsle_Motor();
	}
}

