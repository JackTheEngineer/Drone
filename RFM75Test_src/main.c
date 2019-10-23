#include "motion_sensor.h"
#include "base.h"
#include "hardware.h"
#include "RFM75_driver.h"


uint32_t volatile g_systick_count;


int main(void){
	uint8_t address[5] = {1,0,0,0,0};
	(void)DAVE_Init();

	SYSTIMER_Start();
	DIGITAL_IO_SetOutputHigh(&LED1);
	DIGITAL_IO_SetOutputHigh(&LED2);

	bool initialize=0;

	while(initialize == 0){
		delay_ms(25);
		initialize = RFM75_Init();
		DIGITAL_IO_ToggleOutput(&LED1);
	}
	RFM75_setRxModeIfNeeded();
	configRxPipe(/* Pipe number */ 0,
		     address,
		     /* Static = 1, Dynamic = 0 */ 0,
		     /*	Enable Auto Acknowledge */ 1);
	RFM75_setChannel(50);
	CE_HIGH;


	uint8_t received_bytes[32] = {0};
	uint8_t received_length;
	while(1U){
		if((g_systick_count % 100) == 0){
			received_length = RFM75_Receive_bytes(received_bytes);
			asm("nop");
		}
	}
}

