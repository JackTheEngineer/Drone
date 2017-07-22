#include "DAVE.h"
#include "motor_pwm.h"
#include "timetasks.h"
#include "led_module.h"

volatile uint32_t tick5ms;

int main(void)
{
	PWM_Init();

	TickInterrupt_init();
	leds_init();

	PWM_Motor1_Set_Rate(500);
	PWM_Motor2_Set_Rate(500);
	PWM_Motor3_Set_Rate(500);
	PWM_Motor4_Set_Rate(500);

	while(1U){
		TimeTasks_run();
	}
	return 1;
}


