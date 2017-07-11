
#include "base.h"
#include "led_module.h"
#include "../rte/timetasks.h"
#include "xmc_scu.h"
#include "pwm.h"

extern XMC_SCU_CLOCK_CONFIG_t clock_config;
uint32_t GlobalTimerSignal;
uint32_t ticks;

int main(void){

	XMC_SCU_CLOCK_Init(&clock_config);

	leds_init();
	PWM_Init();
	Tick_Interrupt_Init();

	while(true){
		if(GlobalTimerSignal != 0 ){
			GlobalTimerSignal--;
			ticks++;
			Run_Next_Task(&ticks);
		}
	}
	return 0;
}
