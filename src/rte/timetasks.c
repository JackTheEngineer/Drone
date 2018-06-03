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
	uint8_t receive = 0;
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
	uint8_t sendbytes[DATASIZE_RFM75_TRANSMIT] = {11, 22, 33, 44, 55, 66, 77, 88};
	if((ticks % TIME5MS) == 0){
		uint8_t readbytes[5] = {R_RX_PAYLOAD, 0,0,0,0};
//		uint8_t testcmd[2] = {SETUP_AW,  0};
//		uint8_t write_cmd[2] = {WRITE_COMMAND_RFM(SETUP_AW), ADDRESS_WIDTH_3};
		Action_5ms(os);

//		RC_Iface_send_bytes(write_cmd, 2, DISABLE_CE);
//		RC_Iface_read_bytes(testcmd, 2, DISABLE_CE);
		RFM75_Transmit_bytes(sendbytes, DATASIZE_RFM75_TRANSMIT);

		asm("NOP");
	}
	if((ticks % TIME100MS) == 0){
		led_toggle(LED0);
	}
	if((ticks % TIME1S) == 0){
		led_toggle(LED1);
	}
	if((ticks % TIME2S) == 0){
		Wechsle_Motor();
	}
}

