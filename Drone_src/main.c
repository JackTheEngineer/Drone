#include "application/motion_sensor/motion_sensor.h"
#include "motor_pwm.h"
#include "timetasks.h"
#include "led_module.h"
#include "buttons.h"
#include "os.h"
#include "delay.h"
#include "RFM75_driver.h"
#include "statemachine.h"
#include "uart.h"
#include "byte_formatting.h"

void blinkdelay(uint32_t time, uint32_t times);

uint32_t volatile tick_count;
extern void SysTick_Handler(void){
	tick_count++;
}

bool UpdateTime(uint32_t *last_ticks){
	if(tick_count != *last_ticks){
		*last_ticks = tick_count;
		return true;
	}else{
		return false;
	}
}

int main(void)
{
	uint32_t last_ticks = 0;
	POINTER_TO_CONTAINER(OS_t, os);
	POINTER_TO_CONTAINER(Sensordata_t, motion_sensor);
	State_t state = STATE_CALIBRATE;
	Button_t btn1 = {
			.btn = BUTTON1,
			.laststate = false,
	};

	Button_t btn2 = {
			.btn = BUTTON2,
			.laststate = false,
	};

	os->motion_sensor = motion_sensor;
	os->button_1 = &btn1;
	os->button_2 = &btn2;
	os->current_state = &state;

	PWM_Init();
	PWM_Motor_Set_Rate(0, 0);
	PWM_Motor_Set_Rate(0, 1);
	PWM_Motor_Set_Rate(0, 2);
	PWM_Motor_Set_Rate(0, 3);

	SysTick_Config(SystemCoreClock/1000); /* 1 ms Tick */
	DelayTimer_Init();

	Motion_sensor_init(os->motion_sensor);

	leds_init();
	buttons_init();
	delay_ms(200);
	/* This delay is needed for Vcc stabilization of the RFM75 transmitter */
	bool initialize = RFM75_Init();
	while(initialize == 0){
		delay_ms(25);
		led_toggle(LED0);
	}

	RFM75_setRxModeIfNeeded();
	configRxPipe(/* Pipe number */ 0,
		     address,
		     /* Static = 1, Dynamic = 0 */ 0,
		     /*	Enable Auto Acknowledge */ 1);
	RFM75_setChannel(50);

	UART_Init(&DebugUart);

	CE_HIGH;

	while(1U){
		if(UpdateTime(&last_ticks)){
			Statemachine_do(last_ticks, os);
		}
	}
	return 1;
}

void blinkdelay(uint32_t time, uint32_t times){
	for(uint32_t i=0; i< times; i++){
		delay_ms(time);
		led_toggle(LED1);
	}
}
