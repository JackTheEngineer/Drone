#include "motor_pwm.h"
#include "timetasks.h"
#include "led_module.h"
#include "buttons.h"
#include "os.h"
#include "motion_sensor.h"
#include "delay.h"
#include "RFM75_driver.h"

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
	Button_t btn1 = {
			.btn = BUTTON1,
			.laststate = false,
	};

	Button_t btn2 = {
			.btn = BUTTON2,
			.laststate = false,
	};
	int32_t f = 0;

	os->motion_sensor = motion_sensor;
	os->button_1 = &btn1;
	os->button_2 = &btn2;
	os->frequ_index = &f;

	PWM_Init();
	PWM_Motor_Set_Rate(0, 0);
	PWM_Motor_Set_Rate(0, 1);
	PWM_Motor_Set_Rate(0, 2);
	PWM_Motor_Set_Rate(0, 3);

	SysTick_Config(SystemCoreClock/1000); /* 1 ms Tick */
	DelayTimer_Init();

	//Motion_sensor_init(os->motion_sensor);

	leds_init();
	buttons_init();

	delay_ms(100);
	RFM75_Init();

	while(1U){
		if(UpdateTime(&last_ticks)){
			TimeTasks_run(last_ticks, os);
		}
	}
	return 1;
}
