#include "base.h"
#include "timetasks.h"
#include "delay.h"
#include "RFM75_driver.h"
#include "led.h"
#include "joystick.h"
#include "hardware.h"

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
	uint8_t address[5] = {1,0,0,0,0};
	bool initialization;
	
	(void)DAVE_Init();
	SysTick_Config(SystemCoreClock/1000);
	LED_init();
	DelayTimer_Init();
	// Joystick_Init();
	delay_ms(100);
	initialization = RFM75_Init();
	if(initialization == false){
		while(true){

		}
	}
	RFM75_setTxModeIfNeeded();
	configTxPipe(address, TX_DPL);
	RFM75_setChannel(50);

	while(true){
		if(UpdateTime(&last_ticks)){
			TimeTasks_run(last_ticks, os);
		}
	}
	return 1;
}

