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
	uint8_t address[5] = {0x35, 0xAF, 0x42, 0x23, 0x99};
	bool initialization;
	
	(void)DAVE_Init();
	SysTick_Config(SystemCoreClock/1000);
	LED_init();
	DelayTimer_Init();
	delay_ms(100);
	initialization = RFM75_Init();
	if(initialization == false){
		while(true){

		}
	}
	RFM75_configTxPipe(address, true);
	RFM75_set_TX_mode();
	RFM75_setChannel(50);
	RFM75_turn_on();

	while(true){
		if(UpdateTime(&last_ticks)){
			TimeTasks_run(last_ticks, os);
		}
	}
	return 1;
}

