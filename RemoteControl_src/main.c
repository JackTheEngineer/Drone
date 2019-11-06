#include "base.h"
#include "timetasks.h"
#include "delay.h"
#include "RFM75_driver.h"
#include "led.h"
#include "joystick.h"
#include "hardware.h"

extern const AddressAndChannel_t default_RFM75_Addr;

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

int main(void){
	POINTER_TO_CONTAINER(OS_t, os);
	uint32_t last_ticks = 0;
	
	(void)DAVE_Init();
	LED_init();
	SysTick_Config(SystemCoreClock/1000);
	DelayTimer_Init();

	bool initialize = false;
	while(initialize == false){
		delay_ms(25);
		initialize = RFM75_Init();
		LED_toggle();
	}
	RFM75_prepareForTransmission(&default_RFM75_Addr);
	LED_off();

	while(true){
		if(UpdateTime(&last_ticks)){
			TimeTasks_run(last_ticks, os);
		}
	}
	return 1;
}

