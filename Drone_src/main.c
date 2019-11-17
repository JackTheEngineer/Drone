#include "motion_sensor.h"
#include "motor_pwm.h"
#include "timetasks.h"
#include "led_module.h"
#include "buttons.h"
#include "os.h"
#include "delay.h"
#include "RFM75_driver.h"
#include "statemachine.h"
#include "uart.h"
#include "byte_manip.h"

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
			.btn = _BUTTON1,
			.laststate = false,
	};

	Button_t btn2 = {
			.btn = _BUTTON2,
			.laststate = false,
	};

	os->motion_sensor = motion_sensor;
	os->button_1 = &btn1;
	os->button_2 = &btn2;
	os->current_state = &state;

	DAVE_Init();
	PWM_Init();
	PWM_Motor_Set_Rate(0, 0);
	PWM_Motor_Set_Rate(0, 1);
	PWM_Motor_Set_Rate(0, 2);
	PWM_Motor_Set_Rate(0, 3);

	buttons_init();
	leds_init();
	SysTick_Config(SystemCoreClock/1000); /* 1 ms Tick */
	DelayTimer_Init();

	delay_ms(150);

	Motion_sensor_init(os->motion_sensor);

	bool initialize = false;
	while(initialize == false){
		delay_ms(25);
		initialize = RFM75_Init();
		led_toggle(_LED1);
	}
	led_off(_LED1);
	while(1U){
		if(UpdateTime(&last_ticks)){
			Statemachine_do(last_ticks, os);
		}
	}
	return 1;
}
