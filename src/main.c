#include "motor_pwm.h"
#include "timetasks.h"
#include "led_module.h"
#include "buttons.h"
#include "os.h"

extern uint32_t volatile tick_count;

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
	Button_t btn1 = {
			.btn = BUTTON1,
			.laststate = false,
	};

	Button_t btn2 = {
			.btn = BUTTON2,
			.laststate = false,
	};
	int32_t f = 0;

	os->button_1 = &btn1;
	os->button_2 = &btn2;
	os->frequ_index = &f;

	PWM_Init();

	TickInterrupt_init();
	leds_init();
	buttons_init();

	PWM_Motor1_Set_Rate(500);
	PWM_Motor2_Set_Rate(500);
	PWM_Motor3_Set_Rate(500);
	PWM_Motor4_Set_Rate(500);

	while(1U){
		if(UpdateTime(&last_ticks)){
			TimeTasks_run(last_ticks, os);
		}
	}
	return 1;
}


