#include "motion_sensor.h"
#include "base.h"
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
	uint32_t last_ticks = 0;

	POINTER_TO_CONTAINER(Sensordata_t, motion_sensor);
	DAVE_STATUS_t status = DAVE_Init();

	SysTick_Config(SystemCoreClock/1000); /* 1 ms Tick */

	DIGITAL_IO_SetOutputHigh(&LED1);
	DIGITAL_IO_SetOutputHigh(&LED2);
	Motion_sensor_init(motion_sensor);

	while(1U){
		if(UpdateTime(&last_ticks)){
			if(tick_count % 10 == 0){
				Motion_sensor_get_data(motion_sensor);
				asm("NOP");
			}
		}
	}
}
