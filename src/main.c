#include "motor_pwm.h"
#include "timetasks.h"
#include "led_module.h"
#include "buttons.h"
#include "os.h"
#include "uart.h"
#include "motion_sensor.h"

/* To be removed */
#include "spi_wrapper.h"




extern uint32_t volatile tick_count;
extern UART_t UART_0;

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
	uint32_t last_ticks;
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
	UART_Init(&UART_0);

	Motion_sensor_init(os->motion_sensor);

	TickInterrupt_init();
	leds_init();
	buttons_init();

	PWM_Motor_Set_Rate(100, 0);
	PWM_Motor_Set_Rate(100, 1);
	PWM_Motor_Set_Rate(100, 2);
	PWM_Motor_Set_Rate(100, 3);

	while(1U){
		if(UpdateTime(&last_ticks)){
			TimeTasks_run(last_ticks, os);
		}
	}
	return 1;
}


